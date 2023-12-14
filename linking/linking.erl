% linking example (fault tolerance)
-module(linking).

-export([
    start/0,
    loop/0,
    request/1,
    start_monitor/0,
    request_monitor/1,
    stop_server/0
]).

% spawn link function to start server
start() -> 
    % setto il flag trap_exit in modo che
    % se il processo linkato muore io non muoio
    % ma devo gestire l'eccezione 
    % il messaggio di errore e' {'EXIT', Pid, Reason}
    process_flag(trap_exit, true),
    register(
        % registro il pid (vedi sotto)
        add_one, 
        % spawno loop() e mi linko a lui
        % il link e' bidirezionale
        spawn_link(linking, loop, [])
    )
.

% server function
loop() ->
    receive
        {stop} -> ok;
        % API per ottenere l'incremento di un int
        {request, Pid, Integer} -> 
            Pid!{result, Integer + 1},
            linking:loop()
    end
.

% client side function
request(Integer) ->
    add_one!{request, self(), Integer},
    receive
        {result, Result} -> Result;
        % messaggio di errore (il servizio e' morto)
        {'EXIT', Pid, _Reason} ->
            % se il server muore lo reinstanzio
            linking:start(),
            % ritorno l'errore che ho gestito 
            {process_dead, Pid}
    after
        3000 -> timeout_reached
    end
.

% utility
stop_server() -> add_one!{stop}.

% per link unidirezionali usare i monitor
% non muoio se i miei linkati muoiono
% spawn link function to start server
start_monitor() -> 
    % se il processo linkato muore io non muoio
    % ma devo gestire l'eccezione 
    % il messaggio di errore e' 
    % {'DOWN', Reference, process, Pid, Reason}
    {Pid, _} = spawn_monitor(linking, loop, []),
    register(add_one, Pid)
.

% client side function
request_monitor(Integer) ->
    add_one!{request, self(), Integer},
    receive
        {result, Result} -> Result;
        % il messaggio di errore e' {'DOWN', ...}
        {'DOWN', _Reference, process, Pid, _Reason} ->
            % se il server muore lo reinstanzio
            linking:start_monitor(),
            % ritorno l'errore che ho gestito 
            {process_dead, Pid}
    after
        3000 -> timeout_reached
    end
.

%%%%%%%%%%%% DEMO (da shell)
% linking:start().
% linking:request(10).
% self().
%%% il processo loop fa crash ma io no
%%% il mio pid non cambia
% linking:request(pippo).
% self().
% linking:request(18).
% linking:stop_server().
%%% ora uso un monitor
% linking:start_monitor().
% linking:request_monitor(10).
% self().
%%% se io crasho il server non lo fa
% 1/0.
%%% ho cambiato pid (shell)
% self().
%%% il server non deve essere reistanziato (start)
% linking:request_monitor(18).
% linking:stop_server().