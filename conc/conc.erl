% concurrency
-module(conc).
-import(timer, [sleep/1]).
-export([
    provaNoBlock/0,
    message_ok/0,
    timeout_message/0,
    mia_sleep/1,
    mia_flush/0,
    demo_flush/0,
    priority_msg/0,
    all_registered_pids/0,
    simple_register/1,
    better_register/0,
    is_actor_alive/1,
    register_demo/0
]).

provaNoBlock() ->
    receive
        {ping, Pid} -> Pid!{pong}
    after
        % dopo 10 secondo mi sblocco
        % non e' una sleep
        10000 -> io:format("Timeout reached without messages!\n")
    end
.

% il messaggio viene ricevuto in tempo 4sec < 10sec
message_ok() -> 
    Attore = spawn(conc, provaNoBlock, []),
    % dopo 4 secondi invio un messaggio di ping
    timer:sleep(4000),
    Attore!{ping, self()}
.

% il messaggio NON viene ricevuto in tempo 16sec > 10sec
timeout_message() ->
    Attore = spawn(conc, provaNoBlock, []),
    % non fara' in tempo e triggerera' il timeout
    % dopo 16 secondi invio un messaggio di ping
    timer:sleep(16000),
    Attore!{ping, self()}
.

% implemento mia sleep
mia_sleep(Timer) ->
    receive
        % pattern vuoto che non fa match con nulla
    after
        Timer -> io:format("Timeout!\n")
    end
.

% implemento mia flush
% se c'e' qualcosa nella coda la stampo a video
mia_flush() ->
    receive
        AnyPattern -> 
            io:format("Shell received message: ~p\n", [AnyPattern]),
            conc:mia_flush()
    after
        % se non ha messaggi nella coda 
        % restituisce subito (0 sec) ok
        0 -> ok
    end
.

% demo per usare mia_flush()
% mi invio 10 messaggi e faccio flush
demo_flush() ->
    lists:foreach(fun(X) -> self()!X end, lists:seq(1, 10)),
    conc:mia_flush()
.

% messaggi con priorità
% Pattern 1 Max priority
% ...
% Pattern n Min priority
priority_msg() ->
    % dammi il tempo di inviare dei messaggi da shell
    timer:sleep(5000),
    receive
        % Max priority for alarm 1
        {alarm, 1} -> io:format("This is a message of priority 1\n")
    after 0 -> 
            receive
                {alarm, 2} -> io:format("This is a message of priority 2\n")
            after 0 ->
                receive 
                    _Any -> io:format("This is a standard message of priority 3\n")
                end
            end
    end
.
             
% registra PID come atomo (circa DNS per processi)
% restituisce true se ha successo
register_pid(NameAtom, Pid) ->
    register(NameAtom, Pid)
.

% tutti PID registrati sul nodo self()
all_registered_pids() -> registered().

% registro un attore
simple_register(RegActAtom) ->
    % creo un attore che risponde a ciao
    AttoreFun = fun() ->
        receive
            {ciao, Pid} -> Pid!{ciao}
        end
    end,
    Attore = spawn(AttoreFun),
    % registro il PID dell'attore
    % restituisco il successo o meno
    % NB non puoi registrare lo stesso atomo
    % per due attori diversi
    register_pid(RegActAtom, Attore)
.

% verifica attore e' ancora vivo
is_actor_alive(RegActAtom) ->
    % NB voglio sapere se l'attore e' ancora vivo
    % RegActAtom e' l'atomo (nome) con cui registro il
    % Pid del mio attore
    PidAttore = whereis(RegActAtom),
    case PidAttore /= undefined of
        true -> io:format("~p is alive!\n", [RegActAtom]);
        false -> io:format("~p is dead!\n", [RegActAtom])
    end
.

% demo di registrazione di actor1
register_demo() ->
    simple_register(actor1),
    % NB se la shell crasha non funziona is_actor_alive()
    is_actor_alive(actor1),
    % saluto e l'attore termina
    io:format("Invio: ~p\n", [actor1!{ciao, self()}]),
    timer:sleep(1000),
    % NB se la shell crasha non funziona is_actor_alive()
    is_actor_alive(actor1)
.

% registrazione piu' safe
better_register() ->
    % NB ognuno registra se stesso
    % prima registro locale
    register(locale, self()),
    io:format("Shell Pid (locale): ~p\n", [whereis(locale)]),
    % poi spawno un attore che si auto-registra come remoto
    spawn(
        fun() ->
            % l'attore si registra come remoto
            register(remoto, self()),
            io:format("Actor Pid (remoto): ~p\n", [whereis(remoto)]),
            % mi invia un ack
            locale!{remoto, ok},
            % aspetto di ricevere {ciao}
            receive
                {ciao} -> io:format("~p: Ho ricevuto ciao!\n", [self()])
            end
        end
    ),
    % appena remoto e' pronto invio {ciao}
    receive
        {remoto, ok} -> remoto!{ciao}
    end
.

% se la shell crasha prova a
% deregistrare tutti i PID
% free variables (da shell)
% f().