from sockets import Driver, InterfaceSocket, Status
import numpy as np
import time

sh = (4,2)
dtype = np.float32

def do_something(A):
    time.sleep(5)
    return A*2

def main():
    server = InterfaceSocket()
    server.open()

    client, address = server.server.accept()
    # client.settimeout(server.timeout)
    driver = Driver(client)
    print(" @SOCKET:   Client asked for connection from " + str(address) + ". Now hand-shaking.")

    A = np.zeros(sh, dtype)
    # while(1):
    for i in range(10):
        stat = driver.get_status()
        if stat == Status.Up | Status.NeedsInit:
            driver.initialise()
            print('Driver initialised.')
        elif stat == Status.Up | Status.HasData:
            A = driver.get_data(A)
            print('Data received: \n {}'.format(A))
            A = do_something(A)

        elif stat == Status.Up | Status.Ready:
            driver.send_data(A)
            print('Data sent: \n {}'.format(A))


if __name__ == '__main__':
    main()