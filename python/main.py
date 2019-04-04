import socket
import string
import numpy as np

from sockets import Driver, InterfaceSocket

HDRLEN = 12

def Message(mystr):
    """Returns a header of standard length HDRLEN."""
    return string.ljust(string.upper(mystr), HDRLEN)

def MessageRev(mystr):
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
    """Simple class used to keep track of the status of the client.
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

server = InterfaceSocket()
server.open()

client, address = server.server.accept()
client.settimeout(server.timeout)
driver = Driver(client)
print(" @SOCKET:   Client asked for connection from " + str(address) + ". Now hand-shaking.")

A = driver.get_data()
print(A)