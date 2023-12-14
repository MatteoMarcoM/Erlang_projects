% distributed erlang programming
-module(distributed).

-export([
    remote_init_pluto/1,
    do_remote_job/1,
    somma/2
]).

% NB i nodi devono essere inizializzati da shell (per forza)
% con una password condivisa (che identifica il cluster di nodi)
% nodo -> VM erlang indipendente, esegue in locale o in remoto

% far partire un nodo da erlang shell (con password)
% erl -sname nome_nodo_atomo -setcookie password

% carica il codice da nodo remoto con c(distributed).
% devi eseguire elang nello stesso path (nella stessa cartella) 
% dove c'e' il codice o devi copiare il codice sul path dove
% esegue il nodo remoto (in modo che abbia il codice da eseguire)
% quindi basta fare delle repliche del codebase nei vari nodi
% oppure si può anche inviare moduli e funzioni a nodi remoti
% NB devi usare la stessa versione di erlang VM (BEAM) in tutti nodi

% inizializza il server
% Node sara' il riferimento al nodo remoto pluto@host
remote_init_pluto(Node) -> 
    % registro il processo pippo nel nodo pippo@host 
    % che chiamera' il processo pluto in pluto@host
    % pippo e' un processo e va registrato con self()
    register(pippo, self()),
    % faccio spawn sul nodo remoto pluto@host
    spawn(
        % riferimento al nodo remoto pluto@host
        Node, 
        % modulo da cui prendere la funzione da eseguire
        distributed,
        % funzione da eseguire nel nodo remoto pluto@host 
        do_remote_job,
        % riferimento a me (pippo@host) da inviare a pluto@host 
        [node()]
    ),
    % aspetto che pluto@host si sia registrato
    receive
        % PlutoNodeId e' il riferimento al nodo remoto pluto@host 
        {registered, PlutoNodeId} ->
            % invio il messaggio per eseguire il job remoto
            % {nomeregistrato, nodoremoto}!{messaggio}
            % {pid_processo, id_macchina}!{msg} 
            {pluto, PlutoNodeId}!{distributed, somma, 10, 2}
    end,
    % aspetto il risultato
    receive
        {result, Res} -> 
            io:format("~p: I've received: ~p\n",
                % self() e' il processo pippo del nodo pippo@host
                % node() e' il nodo pippo@host 
                [{self(), node()}, Res]
            )
    end
.

% demo function
somma(X, Y) -> X + Y.

% faccio eseguire una funzione ad un nodo remoto (load balancing)
% ParentNodeId e' il riferimento al nodo pippo@host
do_remote_job(ParentNodeId) ->
    % registro  il processo pluto nel nodo pluto@host
    register(pluto, self()),
    % notifico che mi sono registrato 
    % al processo pippo nel nodo pippo@host
    % {pid_processo, nodo_remoto}!{messaggio}
    {pippo, ParentNodeId}!{registered, node()},
    receive 
        % API del servizio pluto nel nodo pluto@host
        {Module, Fun, Param1, Param2} ->
            % invoco la funzione sui parametri
            % NB deve essere presente il codice compilato
            % nel mio path per poterlo eseguire
            Result =  Module:Fun(Param1, Param2),
            io:format("~p: I've calculated and send: ~p\n",
                % self() e' il processo pluto del nodo pluto@host
                % node() e' il nodo pluto@host 
                [{self(), node()}, Result]
            ),
            % {pid_processo, nodo_remoto}!{messaggio}
            {pippo, ParentNodeId}!{result, Result} 
    end
.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NB DEMO DEL CODICE !!!

% fai partire un nodo eseguendo su una shell erlang:
% erl -sname pippo -setcookie password

% NB il path deve essere quello dove c'e' il codice
% oppure il codice deve essere presente in entambi i nodi

% fai partire un altro nodo eseguendo su una altra shell erlang:
% erl -sname pluto -setcookie password

% NB il path deve essere quello dove c'e' il codice
% oppure il codice deve essere presente in entambi i nodi

% sulla entrambe le shell compila il codice
% c(distributed).

% sulla shell di pluto stampa il riferimento al nodo col comando
% node(). % -> 'pluto@host'

% sulla shell di pippo esegui
% distributed:remote_init_pluto('pluto@host').