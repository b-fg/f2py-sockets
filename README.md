
## **F2Py Sockets**
### A minimal working example on how to use sockets to transfer data from Fortran to Python and viceversa
This repository shows how to pass a n-dimensional array from Fortran to Python and viceversa. The Fortran `fsockets.f90` (wrapper of the `sockets.c`), the driver.f90 and the `socket.py` files were found in the [i-pi](https://github.com/i-pi/i-pi) repository. Here  the `driver.f90` and the `sockets.py` files have been modified to show a simple example on how to easily transfer data. Refer to the [i-pi](https://github.com/i-pi/i-pi) repository for extended funcionalities.

### How it works
A socket is created in Python allowing remote connections on a certain `port` and `address`
~~~~
server = socket.socket(socket.AF_INET, socket.SOCK_STREAM) 	# Creates INET socket with TCP protocol
server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)   # Set socket options
server.bind((self.address, self.port))	# What addresses and port does the socket listen to
server.listen(1) # How many connection requests do we allow at the same time
~~~~
Now a socket has been opened by the server and can be contacted by the remote socket, our driver, _a.k.a_ client (`driver.f90`). The remote socket needs to be opened in Fortran using the `fsockets.f90` wrapper of the `sockets.c`. We only need to use the wrapper as follows
~~~~
call open_socket(socket, inet, port, host)
~~~~
where `socket` is the socket id, `inet` is the TCP protocol specification, `port` is the port number and `host` is the server address.

From the server (Python) side, we need to accept the Fortran connection. For example,
~~~~
while True:
    # accept connections from outside
    (client, address) = server.accept()
    # now do something with the clientsocket, for example send a message
    client.sendall("A message!".encode())
~~~~

### Requirements
`python3.x` and `numpy` required. For older Python versions (2.x) check the `python_2.7` branch. The main difference is found in the creation of a `socket` object from another `socket` object.


Fortran and C compilers also required. Works fine with `gfortran` and `gcc` respectively. The Fortran compiler choice is specifically important for the declaration of single/double precision variables in Fortran for which the (e.g.) `real(kind=4)` standard is used instead of `real*4`.


### Usage
Run the Python server with
~~~~
cd python
python main.py
~~~~
The Fortran driver needs to be compiled first:
~~~~
cd ../fortran
make
~~~~
And now you can run it with
~~~~
./driver.x -h localhost -p 31415
~~~~
The driver should now be able to find the server socket. And the data will start transfering from Fortran to Python and viceversa.
