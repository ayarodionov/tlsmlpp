% -module(servertls).
% https://erlang.org/doc/apps/ssl/ssl.pdf

ssl:start().

{ok, ListenSocket} = gen_tcp:listen(9999, [{reuseaddr, true}]).

{ok, Socket} = gen_tcp:accept(ListenSocket).

inet:setopts(Socket, [{active, false}]).

{ok, TLSSocket} = ssl:handshake(Socket, [{certfile, "./priv/cert.pem"}, {keyfile, "./priv/key.pem"}]).

ssl:setopts(TLSSocket, [{active, true}]).

ssl:connection_information(TLSSocket).

flush().
