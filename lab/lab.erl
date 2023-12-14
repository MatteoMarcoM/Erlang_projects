% laboratory exercize with concurrency
-module(lab).
-export([
    crea_anello/3,
    init/2,
    foreward/2,
    processo_i/3,
    init_central/2,
    primo/2
]).

% rete ad anello
% un attore genera l'attore successivo
% N_act attori che si mandano N_msg Msg ad anello
% un messaggio alla volta
% A_1 -> A_2 -> ... -> A_N -> A_1 (Msg #X)
crea_anello(N_act, N_msg, PrimoPid) ->
    % creo la rete ad anello (ricorsione)
    % ogni attore genera l'attore successivo
    % torna SUBITO un Pid
    PidSuccessivo = case N_act of
        % all'ultimo attore devo conoscere il Pid del primo attore
        % che e' l'attore successivo all'ultimo (anello)
        % e dirgli che ho finito di creare attori
        0 -> 
            PrimoPid!{all_created},
            % dopo l'ultimo c'e' il primo (anello)
            % PidSuccessivo <- PrimoPid 
            PrimoPid;
        % altrimenti creo il prossimo attore
        % mi ritorna il Pid dell'attore immediatamente succesivo
        % NB il processo N_act spawna un processo N_act-1 che 
        % esegue crea_anello(N_act-1, ...)
        _ -> spawn(
                fun() -> 
                    % ricorsione nel nuovo attore
                    % non e' una vera ricorsione
                    % simile alla fork()
                    lab:crea_anello(N_act-1, N_msg, PrimoPid)
                end
            )
    end,
    % stampa di I/O
    % se sono l'ultimo non creo la shell (primo nodo)
    case N_act > 0 of
        true -> io:format("~p: ho creato il processo n: ~p, con PID ~p\n", 
            [self(), N_act, PidSuccessivo]
        );
        % sono l'ultimo nodo (non stampo nulla)
        % altrimenti continua
        false -> true
    end, 
    % inoltro il messaggio all'attore successivo
    lab:foreward(N_msg, PidSuccessivo)
.

% funzione di forward del messaggio
% fa ricorsione su se stessa nel numero di messaggi
foreward(N_msg, NextNode) ->  
    case N_msg > 0 of
        % ho ancora messaggi da inviare
        % aspetto il prossimo messaggio e lo inoltro 
        % all'attore successivo
        true ->
            % invia il messaggio al prossimo attore
            NextNode!{messaggio},
            % si mette in attesa del messaggio precedente (anello)
            % poi inizia il prossimo round del messaggio
            receive
                {messaggio} -> 
                    io:format("~p: Inoltro il messaggio appena arrivato n: ~p al nodo: ~p\n", 
                        [self(), N_msg, NextNode]
                    ),
                    lab:foreward(N_msg-1, NextNode);
                % se ricevo stop fermo il nodo successivo
                {stop} -> NextNode!{stop}
            end;
        % i messaggi da inviare sono finiti percio'
        % fermo il nodo successivo 
        false -> 
            io:format("~p : fermo ~p inviando stop!\n", 
                [self(), NextNode]
            ),
            NextNode!{stop},
            io:format("~p: termino con successo!\n", [self()])
    end
.

% comportamento primo processo
% essendo la shell non termina con {stop}
% per il resto e' uguale a foreward()
primo(N_msg, NextNode) -> 
    case N_msg > 0 of
        true -> 
            NextNode!{messaggio}, 
            receive
                % se ricevo il messagio indietro posso iniziare il prossimo round 
                {messaggio} -> 
                    io:format("~p: Inoltro il messaggio appena arrivato n: ~p al nodo: ~p\n", 
                        [self(), N_msg, NextNode]
                    ),
                    primo(N_msg-1, NextNode)
                % NOTA: qui non c'e' il MSG di stop!!!
            end;  
        % i messaggi da inviare sono finiti percio'
        % fermo il nodo successivo 
        false -> 
            io:format("~p : fermo ~p inviando stop!\n", 
                [self(), NextNode]
            ),
            NextNode!{stop},
            io:format("~p: termino con successo!\n", [self()])      
    end
.

% inizializza l'anello (ricorsiva)
% chi invoca fa parte dell'anello
init(N_act, N_msg) ->
    io:format("NOTA 1: la concorrenza delle stampe non e' gestita!\n"),
    io:format("NOTA 2: gli indici progressivi (processi e messaggi) sono in ordine inverso!\n"),
    % Secondo e' sotto-anello di N_act-1 nodi 
    % creo l'anello di cui io sono il primo nodo
    io:format("Il PID del primo nodo (shell) e': ~p\n", [self()]),
    PidSecondo = spawn(
        lab, 
        crea_anello, 
        [N_act-1, N_msg, self()]
    ),
    % l'indice del processo creato e' N_act che esegue crea_anello(N_act-1, ...)
    io:format("~p: ho creato il processo n: ~p, con PID ~p\n", 
            [self(), N_act, PidSecondo]
        ),
    % aspetto che l'anello sia completato
    receive
        {all_created} -> io:format("Creato l'anello completo!\n")
    end,
    % avvio la sequenza di messaggi
    % eseguendo il comportamento del primo nodo sul secondo
    % nodo appena generato
    primo(N_msg, PidSecondo)
.

% VERSIONE PIU SEMPLICE CENTRALIZZATA

% da fare con i registered per pid processi
% usiamo un processo padre che fa spawn di N_act processi
% chi invoca non fa parte dell'anello
init_central(N_act, N_msg) ->
    io:format("NOTA 1: la concorrenza delle stampe non e' gestita!\n"),
    io:format("NOTA 2: gli indici progressivi dei messaggi sono in ordine inverso!\n"),
    % invece di usare PrimoPid registro il Pid della shell
    register(shell, self()),
    RegisteredList = lists:map(
        fun(Index) -> 
            spawn(
                lab,
                processo_i,
                [Index, N_act, N_msg]
            ) 
        end,
        lists:seq(1, N_act)
    ),
    % devo creare una barriera di sincronizzazione
    % i processi potrebbero non aver fatto in tempo a registrarsi
    % mi metto in attesa di N_act registrazioni
    lists:foreach(
        fun(_Elem) ->
            receive
                {created} -> ok
            end
        end,
        % come se fosse: lists:seq(1, N_act)
        RegisteredList
    ),
    io:format("Anello completato!\n"),
    % prendo il Pid del primo attore
    PrimoAttore = lists:nth(1, RegisteredList),
    % avvio la sequenza di messaggi
    PrimoAttore!{messaggio},
    receive
        % aspetto che tutti abbiano terminato e termino
        {completed} -> ok
    end
.

% non serve piu' il primo pid
% nessuno sa di essere l'ultimo
% devo passare N_act e N_msg
% creo il processo i esimo
processo_i(MioIndex, N_act, N_msg) ->
    % NB una stringa e' una lista di char
    MioID = "processo_" ++ integer_to_list(MioIndex),
    AtomID = list_to_atom(MioID),
    register(AtomID, self()),
    % notifico di essermi registrato alla shell
    shell!{created},
    io:format("Shell (~p): creato il processo ~p: ~p\n", 
        [whereis(shell), MioID, self()]
    ),

    % sono l'ultimo?
    NextIndex = 
        case MioIndex < N_act of
            true -> MioIndex + 1;
            % sono l'ultimo, il prossimo e' index 1
            false -> 1
        end
    ,
    NextIDStr = "processo_" ++ integer_to_list(NextIndex),
    NextID = list_to_atom(NextIDStr),
    foreward_central(MioIndex, NextID, N_act, N_msg) 
.

% funzione di foreward di messaggi
foreward_central(MioIndex, NextID, N_act, N_msg) ->
    case N_msg > 0 of
        true -> 
            NextID!{messaggio}, 
            receive
                % se ricevo il messagio indietro posso iniziare il prossimo round 
                {messaggio} -> 
                    io:format("~p: Messaggio n: ~p ricevuto. Inoltro il messaggio a: ~p\n", 
                        [self(), N_msg, whereis(NextID)]
                    ),
                    % ricorsione su me stesso (con N_msg-1)
                    foreward_central(MioIndex, NextID, N_act, N_msg-1);
                % terminazione
                {stop} -> ok
            end;  
        % i messaggi da inviare sono finiti percio'
        % fermo il nodo successivo 
        false ->
            % i messaggi sono finiti
            % se sono l'ultimo attore notifico la shell
            case N_act == MioIndex of
                false -> 
                    io:format("~p : fermo ~p inviando stop!\n", 
                        [self(), whereis(NextID)]
                    ),
                    NextID!{stop},
                    io:format("~p: termino con successo!\n", [self()]);
                true -> io:format("~p: notifico il completamento dello script inviando ~p!\n", 
                        [self(), {completed}]
                    ),
                    % termino la shell usando il suo registered
                    shell!{completed},
                    io:format("~p: termino con successo!\n", [self()])
            end      
    end
.