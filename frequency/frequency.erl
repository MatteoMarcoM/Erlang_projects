% esempio di utilizzo di processi linkati
% gestore di frequenze (operatore telecom)
-module(frequency).
-export([
    get_frequencies/0,
    start/0,
    init/0,
    loop/0,
    list_free/0,
    list_occupied/0,
    allocate_me/1,
    stop/0
]).

% popola le frequenze
get_frequencies() -> [10, 11, 12, 13, 14, 15].


% fa partire il server e lo registra in locale
start() ->
    register(
        server, 
        spawn(frequency, init, [])
    )
.

% inizializza il server
% setta il trap exit a true per non morire se muoiono i client
init() ->
    process_flag(trap_exit, true),
    % creo una tabella: Frequenza <--> Pid 
    ets:new(freq, [named_table]),
    lists:foreach(
        fun(Freq) ->
            % all'inizio tutte le frequenze sono libere
            ets:insert(freq, {Freq, not_allocated})
        end,
        frequency:get_frequencies()
    ),
    frequency:loop()
.

% API del servizio
loop() ->
    receive
        {allocate, Freq, Pid} -> 
            Result = ets:lookup(freq, Freq),
            case Result of
                [{Freq, not_allocated}] ->
                    % assegnamo al Pid la frequenza libera
                    ets:insert(freq, {Freq, Pid}),
                    % mi collego al processo
                    % se muore me ne accorgo 
                    % (per liberare la risorsa)
                    link(Pid),
                    Pid!{allocated, {Freq, Pid}};
                _ ->
                    Pid!{already_in_use}
            end,
            frequency:loop();
        {get_free, Pid} ->
            FreeFreq = ets:foldl(
                fun({F, P}, Acc) ->
                    case P of
                        % se la frequenza e' libera la restiuisco
                        not_allocated -> [F] ++ Acc;
                        % se la frequenza e' in uso passo oltre
                        _ -> Acc
                    end
                end,
                [],
                % tabella ets
                freq
            ),
            Pid!{free_freq, FreeFreq},
            frequency:loop();
        {get_occupied, Pid} ->
            OccupiedFreq = ets:foldl(
                fun({F, P}, Acc) ->
                    case P of
                        % se la frequenza e' libera passo oltre
                        not_allocated -> Acc;
                        % altrimenti la restituisco
                        _ -> [F] ++ Acc
                    end
                end,
                [],
                % tabella ets
                freq
            ),
            Pid!{occupied_freq, OccupiedFreq},
            freq:loop();
        {stop} -> 
            % libero la RAM
            ets:delete(freq),
            % deregistro il server name
            unregister(server),
            ok;
        % gestisco l'eccezione in caso di morte di un processo spawnato
        {'EXIT', Pid, _Reason} ->
            % faccio una sorta di garbage collector delle frequenze
            % ritorno le frequenze da liberare
            % restituisce la frequenza corrispondente a Pid o []
            ToFree = ets:foldl(
                fun({F, P}, Acc) ->
                    case P of
                        Pid -> [F] ++ Acc;
                        _ -> Acc
                    end
                end,
                [],
                freq
            ),
            % restituisce la frequenza corrispondente a Pid o []
            case ToFree of
                [F | _T] ->
                    % la frequenza corrispondente a Pid viene liberata (sovrascrittura)
                    ets:insert(freq, {F, not_allocated});
                % la frequenza e' gia' stata liberata
                _ -> ok
            end,
            frequency:loop()
    end
.

% at the end stop and unregister the server, delete ets
stop() -> server!{stop}.

% client side
list_free() ->
    server!{get_free, self()},
    receive
        Msg -> Msg
    after
        3000 -> timeout
    end
.

list_occupied() ->
    server!{get_occupied, self()},
    receive
        Msg -> Msg
    after
        3000 -> timeout
    end
.

allocate_me(Freq) ->
    server!{allocate, Freq, self()},
    receive
        Msg -> Msg
    after
        3000 -> timeout
    end
.

% %%%%%%%%% DEMO
% frequency:start().
% frequency:list_free().
% frequency:allocate_me(10).
% frequency:list_free().
% 1/0.
% frequency:list_free().
% frequency:stop().