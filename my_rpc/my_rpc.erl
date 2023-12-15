% Remote Procedure Call (RPC) in erlang
% WRAPPER per le send e spawn di erlang (più comode)
% ma sono LIMITANTI per il formato che devi utilizzare
-module(my_rpc).

-export([
    fib/1,
    do_remote_job_async/2,
    do_remote_job_async_rpc/2,
    do_remote_job_sync_rpc/2,
    spread_codebase/1
]).

% fibonacci e' data-intensive se N grande
fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2). 

% load balancing senza RPC
% ASINCRONA e DISTRIBUITA
do_remote_job_async(RemoteNode, N) ->
    MioPid = self(),
    spawn(
        RemoteNode,
        fun() -> 
            Res = my_rpc:fib(N),
            % mi invio il risultato calcolato nel nodo remoto
            MioPid!{Res} 
        end)
.

% load balancing con RPC
% ASINCRONA
do_remote_job_async_rpc(RemoteNode, N) ->
    % faccio eseguire la chiamata a fib al nodo remoto
    % NB NON BLOCCANTE !
    Key = rpc:async_call(RemoteNode, my_rpc, fib, [N]),
    % la chiave e' un riferimento al risultato (quando sara' presente)
    % aspetto che il nodo remoto esegua la funzione
    timer:sleep(1000),
    % ora dovrebbe essere pronto il valore della call remota
    % yield e' una promessa (Future)
    rpc:yield(Key)
.

% SICRONA
do_remote_job_sync_rpc(RemoteNode, N) ->
    % questa e' BLOCCANTE (SINCRONA) per al massimo 10 sec
    rpc:call(RemoteNode, my_rpc, fib, [N], 10000)
.

% usare la rpc:multicall() per distribuire il codebase sui nodi remoti
spread_codebase(RemoteNodeList) ->
    {Mod, Bin, File} = code:get_object_code(my_rpc),
    % come se facessi una spawn per ogni nodo dove carico il modulo my_rpc
    rpc:multicall(RemoteNodeList, code, load_binary, [Mod, File, Bin])
.

% %%%%%%%%%%%%%%% DEMO
% primo nodo pippo@host sul path con CODICE:
% erl -sname pippo -setcookie disney
% secondo nodo pluto@host in un path DIVERSO:
% erl -sname pluto -setcookie disney
% (pippo@host)> my_rpc:fib(15).
% la riga seguente crasha perche non ho diffuso la codebase
% (pluto@host)> my_rpc:fib(10).
% (pippo@host)> my_rpc:spread_codebase([pluto@host]).
% (pluto@host)> my_rpc:fib(10).
% (pippo@host)> my_rpc:do_remote_job_async(pluto@host, 16).
% (pippo@host)> flush().
% (pippo@host)> my_rpc:do_remote_job_async_rpc(pluto@host, 18).
% (pippo@host)> my_rpc:do_remote_job_async_rpc(pluto@host, 26).