% -module(clienttls1).
ssl:start().

{ok, Socket} = gen_tcp:connect("localhost", 9999, [], infinity).

{ok, TLSSocket} = ssl:connect(Socket, [{certfile, "./priv/cert.pem"}, {keyfile, "./priv/key.pem"}], infinity).

ssl:connection_information(TLSSocket).

ssl:send(TLSSocket, "foo").