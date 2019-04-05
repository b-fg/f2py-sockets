from sockets import Driver, InterfaceSocket

def main():
    server = InterfaceSocket()
    server.open()

    client, address = server.server.accept()
    # client.settimeout(server.timeout)
    driver = Driver(client)
    print(" @SOCKET:   Client asked for connection from " + str(address) + ". Now hand-shaking.")

    A = driver.get_data()
    print(A)

if __name__ == '__main__':
    main()