from sockets import Driver, InterfaceSocket


server = InterfaceSocket()
server.open()

client, address = server.server.accept()
client.settimeout(server.timeout)
driver = Driver(client)
print(" @SOCKET:   Client asked for connection from " + str(address) + ". Now hand-shaking.")

A = driver.get_data()
print(A)