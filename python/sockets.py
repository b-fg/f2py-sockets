"""
Deals with the socket communication between Python (server) and Fortran (client or driver)
"""
import socket
import select
import numpy as np

__all__ = ['InterfaceSocket']

HDRLEN = 12         # Messages arbitrary length
TIMEOUT = 0.02      # Timeout of Fortran side
SERVERTIMEOUT = 15  # Timeout of Python side
NTIMEOUT = 20       # Intents to receive data

def Message(mystr):
    """Returns a header of standard length HDRLEN."""
    return mystr.upper().ljust(HDRLEN)


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

    Attributes:
       _buf: A string buffer to hold the reply from the other connection.
    """
    def __init__(self, socket):
        """Initialises DriverSocket.

        Args:
           socket: A socket through which the communication should be done.
        """
        super().__init__(family=socket.family, type=socket.type, proto=socket.proto, fileno=socket.fileno())
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
        return self.sendall(Message(msg).encode())

    def recv_msg(self, l=HDRLEN):
        """Get the next message send through the socket.

        Args:
           l: Length of the accepted message. Defaults to HDRLEN.
        """
        return self.recv(l).decode()

    def recvall(self, dest):
        """Gets the data in dest. Dest is the empty data array

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

        while bpos < blen:
            timeout = False
            # post-2.5 version: slightly more compact for modern python versions
            try:
              bpart = 1
              bpart = self.recv_into(self._buf[bpos:], blen-bpos)
            except socket.timeout:
              print(" @SOCKET:   Timeout in status recvall, trying again!")
              timeout = True
              pass
            if (not timeout and bpart == 0):
              raise Disconnected()
            bpos += bpart

        if np.isscalar(dest):
            return np.fromstring(self._buf[0:blen], dest.dtype)[0]
        else:
            return np.fromstring(self._buf[0:blen], dest.dtype).reshape(dest.shape, order='F')


class Driver(DriverSocket):
    """Deals with communication between the client and driver code.

    Deals with sending and receiving the data from the driver code. Keeps track
    of the status of the driver.

    Attributes:
       waitstatus: Boolean giving whether the Python sockets is waiting to get a status answer.
       status: Keeps track of the status of the driver.
    """
    def __init__(self, socket):
        """Initialises Driver.
        Args:
           socket: A socket through which the communication should be done.
        """
        super(Driver, self).__init__(socket=socket)
        self.waitstatus = False
        self.status = Status.Up


    def initialise(self):
        self.sendall(Message("init").encode())


    def shutdown(self, how=socket.SHUT_RDWR):
        """Tries to send an exit message to clients to let them exit gracefully."""
        self.sendall(Message("exit").encode())
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
                # This can sometimes hang with no timeout. Using the recommended 60 s.
                readable, writable, errored = select.select([], [self], [], 60)
                if self in writable:
                    self.sendall(Message("status").encode())
                    self.waitstatus = True
            except socket.error:
                return Status.Disconnected

        try:
            reply = self.recv(HDRLEN).decode()
            self.waitstatus = False  # got some kind of reply
        except socket.timeout:
            print(" @SOCKET:   Timeout in status recv!")
            return Status.Up | Status.Busy | Status.Timeout
        except:
            print(" @SOCKET:   Other socket exception. Disconnecting client and trying to carry on.")
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
            print(" @SOCKET:    Unrecognized reply: " + str(reply))
            return Status.Up


    def get_status(self):
        """ Sets (and returns) the client internal status. Wait for an answer if
            the client is busy. """
        status = self._getstatus()
        while status & Status.Busy:
            status = self._getstatus()
        self.status = status
        return status


    def get_data(self, A):
        """Gets the data from the driver.

        Raises:
           InvalidStatus: Raised if the status is not HasData.
           Disconnected: Raised if the driver has disconnected.

        Returns:
           Data array
        """
        self.sendall(Message("getdata").encode()) # Send a message to the driver to get the data and wait for reply
        reply = ""
        while True:
            try:
                reply = self.recv_msg()
            except socket.timeout:
                print(" @SOCKET:   Timeout in getdata, trying again!",)
                continue
            except:
                print(" @SOCKET:   Error while receiving message: %s" % (reply))
                raise Disconnected()

            if reply == Message("dataready"):
                break
            else:
                print(" @SOCKET:   Unexpected getdata reply: %s" % (reply))
            if reply == "":
                raise Disconnected()

        A = self.recvall(A)
        return A

    def send_data(self, A):
        """Send data to the driver.

        Raises:
           InvalidStatus: Raised if the status is not HasData.
           Disconnected: Raised if the driver has disconnected.

        Returns:
           Data array
        """
        if (self.status & Status.Ready):
            try:
                self.sendall(Message("senddata").encode())
                self.sendall(A.flatten(order='F'))
                self.status = Status.Up | Status.Busy
            except:
                print("Error in sendall, resetting status")
                self.get_status()
                return
        else:
            raise InvalidStatus("Status in sendpos was " + self.status)


class InterfaceSocket(object):
    """Host server class.

    Attributes:
       address: A string giving the name of the host network.
       port: An integer giving the port the socket will be using.
       slots: An integer giving the maximum allowed backlog of queued clients.
       timeout: A float giving a timeout limit for considering a calculation dead
          and dropping the connection.
       server: The socket used for data transmition.
    """

    def __init__(self, address="localhost", port=31415, slots=1, timeout=1.0):
        """Initialises interface.

        Args:
           address: An optional string giving the name of the host server.
              Defaults to 'localhost'.
           port: An optional integer giving the port number. Defaults to 31415.
           slots: An optional integer giving the maximum allowed backlog of
              queueing clients. Defaults to 4.
           timeout: Length of time waiting for data from a client before we assume
              the connection is dead and disconnect the client.
        """
        self.address = address
        self.port = port
        self.slots = slots
        self.timeout = timeout

    def open(self):
        """Creates a new socket.

        Used so that we can create a interface object without having to also
        create the associated socket object.
        """
        self.server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.server.bind((self.address, self.port)) # What addresses do we listen to and what port. Leave bind('',self.port) to listen for any local address
        print("Created inet socket with address " + self.address + " and port number " + str(self.port))

        self.server.listen(self.slots) # How many connections request do we allow? Default 1
        # self.server.settimeout(SERVERTIMEOUT) # Time before Python server gives up on listening for connections

    def close(self):
        """Closes down the socket."""
        print(" @SOCKET: Shutting down the driver interface.")

        try:
            self.server.shutdown(socket.SHUT_RDWR)
            self.server.close()
        except:
            print(" @SOCKET: Problem shutting down the server socket. Will just continue and hope for the best.")