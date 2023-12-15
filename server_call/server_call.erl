% modulo per testare le funzioni rpc:server_call() e rpc:multi_server_call()
-module(server_call).

-export([
  init_call/1,
  loop_call/0,
  init_multi/1, 
  loop_multi/1 
]).

% creo un server per testare la rpc:server_call()

% inizializzazione server per rpc:server_call()
% il test lo faccio in LOCALE
% per il DISTRIBUITO usero' la spawn/4 e passero' il nodo remoto come parametro alla init() 
init_call(ServerName) -> register(ServerName, spawn(server_call, loop_call, [])).

% codice del server per rpc:server_call()
loop_call() ->
    receive
      {Pid, {add, A, B}} -> Pid!{add, node(), A+B}, loop_call();
      {Pid, {sub, A, B}} -> Pid!{sub, node(), A-B}, loop_call();
      {Pid, {mul, A, B}} -> Pid!{mul, node(), A*B}, loop_call();
      {Pid, {divid, A, B}} -> Pid!{divid, node(), A/B}, loop_call();
      {Pid, stop} -> Pid!{stop, node()}
    end
.

% creo un server per testare la rpc:multi_server_call()

% inizializzazione server per rpc:server_call()
% il test lo faccio in LOCALE
% per il DISTRIBUITO usero' la spawn/4 e passero' il nodo remoto come parametro alla init()
init_multi(ServerName) -> register(ServerName, spawn(server_call, loop_multi, [ServerName])).

% codice del server per rpc:multi_server_call()
loop_multi(ServerName) ->
    receive
      {Pid, {add, A, B}} -> Pid!{ServerName, node(), A+B}, loop_multi(ServerName);
      {Pid, {sub, A, B}} -> Pid!{ServerName, node(), A-B}, loop_multi(ServerName);
      {Pid, {mul, A, B}} -> Pid!{ServerName, node(), A*B}, loop_multi(ServerName);
      {Pid, {divid, A, B}} -> Pid!{ServerName, node(), A/B}, loop_multi(ServerName);
      {Pid, stop} -> Pid!{ServerName, node(), stop}
    end
.

% %%%%%%%%%% DEMO
% server_call:init_call(calculator1).
% rpc:server_call(node(), calculator1, add, {add, 1, 1}).
% server_call:init_multi(calculator2).
% rpc:multi_server_call(calculator2, {sub, 1, 1}).