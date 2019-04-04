"""Deals with the socket communication between the i-PI and drivers.

Deals with creating the socket, transmitting and receiving data, accepting and
removing different driver routines and the parallelization of the force
calculation.
"""

import os
import socket
import select
import string
import numpy as np

from messages import verbosity, warning, info

__all__ = ['InterfaceSocket']

HDRLEN = 12         # Messages arbitrary length
TIMEOUT = 0.02      # Timeout of Fortran side
SERVERTIMEOUT = 15  # Timeout of Python side
NTIMEOUT = 20       # Intents to receive data

def Message(mystr):
    """Returns a header of standard length HDRLEN."""
    return string.ljust(string.upper(mystr), HDRLEN)


class Disconnected(Exception):
    """Disconnected: Raised if client has been disconnected."""
    pass


class InvalidSize(Exception):
    """Disconnected: Raised if client returns forces with inconsistent number of atoms."""
    pass


class InvalidStatus(Exception):
    """InvalidStatus: Raised if client has the wrong status.

    Shouldn't have to be used if the structure of the program is correct.
    """
    pass


class Status(object):
    """Simple class used to keep track of Fortran side.

    Uses bitwise or to give combinations of different status options.
    i.e. Status.Up | Status.Ready would be understood to mean that the client
    was connected and ready to receive the position and cell data.

    Attributes:
       Disconnected: Flag for if the client has disconnected.
       Up: Flag for if the client is running.
       Ready: Flag for if the client has ready to receive position and cell data.
       NeedsInit: Flag for if the client is ready to receive forcefield
          parameters.
       HasData: Flag for if the client is ready to send force data.
       Busy: Flag for if the client is busy.
       Timeout: Flag for if the connection has timed out.
    """
    Disconnected = 0
    Up = 1
    Ready = 2
    NeedsInit = 4
    HasData = 8
    Busy = 16
    Timeout = 32


class DriverSocket(socket.socket):
    """Deals with communication between the client and driver code.

    Deals with sending and receiving the data between the client and the driver
    code. This class holds common functions which are used in the driver code,
    but can also be used to directly implement a python client.
    Basically it's just a wrapper around socket to simplify some of the
    specific needs of i-PI communication pattern.

    Attributes:
       _buf: A string buffer to hold the reply from the other connection.
    """
    def __init__(self, socket):
        """Initialises DriverSocket.

        Args:
           socket: A socket through which the communication should be done.
        """
        super(DriverSocket, self).__init__(_sock=socket)
        self._buf = np.zeros(0, np.byte)
        if socket:
            self.peername = self.getpeername()
        else:
            self.peername = "no_socket"

    def send_msg(self, msg):
        """Send the next message through the socket.

        Args:
           msg: The message to send through the socket.
        """
        return self.sendall(Message(msg))

    def recv_msg(self, l=HDRLEN):
        """Get the next message send through the socket.

        Args:
           l: Length of the accepted message. Defaults to HDRLEN.
        """
        return self.recv(l)

    def recvall(self, dest):
        """Gets the potential energy, force and virial from the driver.

        Args:
           dest: Object to be read into.
        Raises:
           Disconnected: Raised if client is disconnected.
        Returns:
           The data read from the socket to be read into dest.
        """

        blen = dest.itemsize * dest.size
        if (blen > len(self._buf)):
            self._buf.resize(blen)
        bpos = 0
        ntimeout = 0

        while bpos < blen:
            timeout = False

            # pre-2.5 version.
            try:
                bpart = ""
                bpart = self.recv(blen - bpos)
                if len(bpart) == 0:
                    raise socket.timeout    # if this keeps returning no data, we are in trouble....
                self._buf[bpos:bpos + len(bpart)] = np.fromstring(bpart, np.byte)
            except socket.timeout:
                #warning(" @SOCKET:   Timeout in recvall, trying again!", verbosity.low)
                timeout = True
                ntimeout += 1
                if ntimeout > NTIMEOUT:
                    warning(" @SOCKET:  Couldn't receive within %5d attempts. Time to give up!" % (NTIMEOUT), verbosity.low)
                    raise Disconnected()
                pass
            if not timeout and len(bpart) == 0:
                raise Disconnected()
            bpos += len(bpart)

            # post-2.5 version: slightly more compact for modern python versions
            # try:
            #   bpart = 1
            #   bpart = self.recv_into(self._buf[bpos:], blen-bpos)
            # except socket.timeout:
            #   print " @SOCKET:   Timeout in status recvall, trying again!"
            #   timeout = True
            #   pass
            # if (not timeout and bpart == 0):
            #   raise Disconnected()
            # bpos += bpart

        if np.isscalar(dest):
            return np.fromstring(self._buf[0:blen], dest.dtype)[0]
        else:
            return np.fromstring(self._buf[0:blen], dest.dtype).reshape(dest.shape)


class Driver(DriverSocket):
    """Deals with communication between the client and driver code.

    Deals with sending and receiving the data from the driver code. Keeps track
    of the status of the driver. Initialises the driver forcefield, sends the
    position and cell data, and receives the force data.

    Attributes:
       waitstatus: Boolean giving whether the Python sockets is waiting to get a status answer.
       status: Keeps track of the status of the driver.
       lastreq: The ID of the last request processed by the client.
       locked: Flag to mark if the client has been working consistently on one image.
    """
    def __init__(self, socket):
        """Initialises Driver.
        Args:
           socket: A socket through which the communication should be done.
        """
        super(Driver, self).__init__(socket=socket)
        self.waitstatus = False
        self.status = Status.Up
        self.lastreq = None
        self.locked = False


    def shutdown(self, how=socket.SHUT_RDWR):
        """Tries to send an exit message to clients to let them exit gracefully."""
        self.sendall(Message("exit"))
        self.status = Status.Disconnected
        super(DriverSocket, self).shutdown(how)

    def _getstatus(self):
        """Gets driver status.

        Returns:
           An integer labelling the status via bitwise or of the relevant members
           of Status.
        """
        if not self.waitstatus:
            try:
                # This can sometimes hang with no timeout.
                # Using the recommended 60 s.
                readable, writable, errored = select.select([], [self], [], 60)
                if self in writable:
                    self.sendall(Message("status"))
                    self.waitstatus = True
            except socket.error:
                return Status.Disconnected

        try:
            reply = self.recv(HDRLEN)
            self.waitstatus = False  # got some kind of reply
        except socket.timeout:
            warning(" @SOCKET:   Timeout in status recv!", verbosity.trace)
            return Status.Up | Status.Busy | Status.Timeout
        except:
            warning(" @SOCKET:   Other socket exception. Disconnecting client and trying to carry on.", verbosity.trace)
            return Status.Disconnected

        if not len(reply) == HDRLEN:
            return Status.Disconnected
        elif reply == Message("ready"):
            return Status.Up | Status.Ready
        elif reply == Message("needinit"):
            return Status.Up | Status.NeedsInit
        elif reply == Message("havedata"):
            return Status.Up | Status.HasData
        else:
            warning(" @SOCKET:    Unrecognized reply: " + str(reply), verbosity.low)
            return Status.Up

    def get_status(self):
        """ Sets (and returns) the client internal status. Wait for an answer if
            the client is busy. """
        status = self._getstatus()
        while status & Status.Busy:
            status = self._getstatus()
        self.status = status
        return status

    def initialize(self, rid, pars):
        """Sends the initialisation string to the driver.

        Args:
           rid: The index of the request, i.e. the replica that
              the force calculation is for.
           pars: The parameter string to be sent to the driver.

        Raises:
           InvalidStatus: Raised if the status is not NeedsInit.
        """

        if self.status & Status.NeedsInit:
            try:
                self.sendall(Message("init"))
                self.sendall(np.int32(rid))
                self.sendall(np.int32(len(pars)))
                self.sendall(pars)
            except:
                self.get_status()
                return
        else:
            raise InvalidStatus("Status in init was " + self.status)

    def getforce(self):
        """Gets the potential energy, force and virial from the driver.

        Raises:
           InvalidStatus: Raised if the status is not HasData.
           Disconnected: Raised if the driver has disconnected.

        Returns:
           A list of the form [potential, force, virial, extra].
        """

        self.sendall(Message("getforce"))
        reply = ""
        while True:
            try:
                reply = self.recv_msg()
            except socket.timeout:
                warning(" @SOCKET:   Timeout in getforce, trying again!", verbosity.low)
                continue
            except:
                warning(" @SOCKET:   Error while receiving message: %s" % (reply), verbosity.low)
                raise Disconnected()
            print(reply)
            if reply == Message("getforce"):
                break
            else:
                warning(" @SOCKET:   Unexpected getforce reply: %s" % (reply), verbosity.low)
            if reply == "":
                raise Disconnected()

        # A = np.zeros(4*2, np.float64)
        # A = self.recvall(A)
        # print(A)
        assert False

class InterfaceSocket(object):
    """Host server class.

    Deals with distribution of all the jobs between the different client servers
    and both initially and as clients either finish or are disconnected.
    Deals with cleaning up after all calculations are done. Also deals with the
    threading mechanism, and cleaning up if the interface is killed.

    Attributes:
       address: A string giving the name of the host network.
       port: An integer giving the port the socket will be using.
       slots: An integer giving the maximum allowed backlog of queued clients.
       mode: A string giving the type of socket used.
       latency: A float giving the number of seconds the interface will wait
          before updating the client list.
       timeout: A float giving a timeout limit for considering a calculation dead
          and dropping the connection.
       server: The socket used for data transmition.
       clients: A list of the driver clients connected to the server.
       requests: A list of all the jobs required in the current PIMD step.
       jobs: A list of all the jobs currently running.
       _poll_thread: The thread the poll loop is running on.
       _prev_kill: Holds the signals to be sent to clean up the main thread
          when a kill signal is sent.
       _poll_true: A boolean giving whether the thread is alive.
       _poll_iter: An integer used to decide whether or not to check for
          client connections. It is used as a counter, once it becomes higher
          than the pre-defined number of steps between checks the socket will
          update the list of clients and then be reset to zero.
    """

    def __init__(self, address="localhost", port=31415, slots=1, mode="unix", timeout=1.0):
        """Initialises interface.

        Args:
           address: An optional string giving the name of the host server.
              Defaults to 'localhost'.
           port: An optional integer giving the port number. Defaults to 31415.
           slots: An optional integer giving the maximum allowed backlog of
              queueing clients. Defaults to 4.
           mode: An optional string giving the type of socket. Defaults to 'unix'.
           latency: An optional float giving the time in seconds the socket will
              wait before updating the client list. Defaults to 1e-3.
           timeout: Length of time waiting for data from a client before we assume
              the connection is dead and disconnect the client.

        Raises:
           NameError: Raised if mode is not 'unix' or 'inet'.
        """

        self.address = address
        self.port = port
        self.slots = slots
        self.mode = mode
        self.timeout = timeout

    def open(self):
        """Creates a new socket.

        Used so that we can create a interface object without having to also
        create the associated socket object.
        """

        if self.mode == "unix":
            self.server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            try:
                self.server.bind("/tmp/ipi_" + self.address)
                info("Created unix socket with address " + self.address, verbosity.medium)
            except socket.error:
                raise RuntimeError("Error opening unix socket. Check if a file " + ("/tmp/ipi_" + self.address) + " exists, and remove it if unused.")

        elif self.mode == "inet":
            self.server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
            self.server.bind((self.address, self.port))
            info("Created inet socket with address " + self.address + " and port number " + str(self.port), verbosity.medium)
        else:
            raise NameError("InterfaceSocket mode " + self.mode + " is not implemented (should be unix/inet)")

        self.server.listen(self.slots)
        self.server.settimeout(SERVERTIMEOUT)

    def close(self):
        """Closes down the socket."""

        info(" @SOCKET: Shutting down the driver interface.", verbosity.low)

        try:
            self.server.shutdown(socket.SHUT_RDWR)
            self.server.close()
        except:
            info(" @SOCKET: Problem shutting down the server socket. Will just continue and hope for the best.", verbosity.low)
        if self.mode == "unix":
            os.unlink("/tmp/ipi_" + self.address)


