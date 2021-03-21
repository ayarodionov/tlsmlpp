-module(clienttls2).
{ok, _} = application:ensure_all_started(ssl).

 COpts = [{cacertfile, "./priv/cert.pem"}, {versions, ['tlsv1.2','tlsv1.3']}, {log_level, debug}, {session_tickets, auto}].

 ssl:connect("localhost", 8001, COpts).

ssl:connect("localhost", 8001, COpts).