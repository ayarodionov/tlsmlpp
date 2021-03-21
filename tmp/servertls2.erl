-module(servertls2).

{ok, _} = application:ensure_all_started(ssl).

LOpts = [{certfile, "./priv/cert.pem"}, {keyfile, "./priv/key.pem"}, {versions, ['tlsv1.2','tlsv1.3']}, {session_tickets, stateless}].

{ok, LSock} = ssl:listen(8001, LOpts).

{ok, CSock} = ssl:transport_accept(LSock).

ssl:handshake(CSock).
{ok, CSock2} = ssl:transport_accept(LSock).