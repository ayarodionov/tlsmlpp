-module(servertls).

ssl:start().

{ok, ListenSocket} = ssl:listen(9999, [{certfile, "./priv/cert.pem"}, {keyfile, "./priv/key.pem"},{reuseaddr, true}]).

{ok, TLSTransportSocket} = ssl:transport_accept(ListenSocket).
{ok, Socket} = ssl:handshake(TLSTransportSocket).
ssl:send(Socket, "foo").
