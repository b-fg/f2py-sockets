"""Deals with the socket communication between the i-PI and drivers.

Deals with creating the socket, transmitting and receiving data, accepting and
removing different driver routines and the parallelization of the force
calculation.
"""


import sys
import os
import socket
import select
import string
import time
import threading

import numpy as np

from sockets import Driver, InterfaceSocket
from messages import verbosity, warning, info

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

s = InterfaceSocket(mode='inet')
s.open()
client, address = s.server.accept()
client.settimeout(s.timeout)
driver = Driver(client)
info(" @SOCKET:   Client asked for connection from " + str(address) + ". Now hand-shaking.", verbosity.low)
# print(driver.get_status())

driver.sendall(Message("getforce"))
reply = ""
while True:
    try:
        reply = driver.recv_msg()
    except socket.timeout:
        warning(" @SOCKET:   Timeout in getforce, trying again!", verbosity.low)
        continue
    except:
        warning(" @SOCKET:   Error while receiving message: %s" % (reply), verbosity.low)
        raise Disconnected()

    # a = Message("forceready")
    # print(reply + '=' + a + '.')
    # print(reply == Message("forceready"))
    if reply == Message("forceready"):
        break
    else:
        warning(" @SOCKET:   Unexpected getforce reply: %s" % (reply), verbosity.low)
    if reply == "":
        raise Disconnected()

A = np.zeros(4*2, np.float64)
A = driver.recvall(A)
print(A)