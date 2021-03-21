-module(clienttls).

{ok, Socket} = ssl:connect("localhost", 9999, [], infinity).

flush().