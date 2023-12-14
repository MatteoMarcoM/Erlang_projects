-module(star).
-export([
    init/1
]).

% creo una struttura a stella di processi
% tutti collegati tra loro tramite links
init(N) ->
    Iteratore = lists:seq(1, N),
    % genero una lista di pid dei processi spawnati
    ListaPid = lists:map(
        % creo l'i-esimo processo con spawn_link
        fun(Index) ->
            spawn_link(
                fun() ->
                    receive
                        {go} ->
                            % solo il primo termina in modo anormale
                            case Index == 1 of
                                true -> 
                                    % faccio fallire il nodo 
                                    % throw dell'eccezione
                                    exit(abnormal_termination);
                                false ->
                                    % termino in modo 'normal'
                                    ok
                            end
                    end
                end
            )
        end,
        Iteratore
    ),
    % setto il trap exit per non fallire io se muoiono i processi spawnati
    process_flag(trap_exit, true),
    % ora posso far partire tutti i processi (che terminano subito)
    lists:foreach(
        fun(Pid) ->
            Pid!{go}
        end,
        ListaPid
    )
.

% %%%%%%%%%%%%%% DEMO
% star:init(5).
% flush().