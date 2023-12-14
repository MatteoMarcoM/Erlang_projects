% hello world erlang concurrency
-module(pingpong).
-export([
    helloPing/0,
    helloPong/0
]).

% invia un messaggio {ping, _} a un Attore che risponde con {pong, _}
helloPing() ->
    PidHelloPong = spawn(pingpong, helloPong, []),
    HelloPingMsg = PidHelloPong!{ping, self()},
    io:format(">>> Send: ~p\n", [HelloPingMsg]),
    receive
        {pong, Msg} -> io:format(">>> Receive: ~p\n", [Msg])
    end
.

% risponde a {ping, PidSender} con {pong, "Hello from Pong"}
helloPong() ->
    receive
        {ping, PidHelloPing} -> PidHelloPing!{pong, "Hello from Pong"}
    end
.