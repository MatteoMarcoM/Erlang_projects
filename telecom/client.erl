% CLIENT DELL'APPLICAZIONE TELECOM

% creo un modulo per gestire la parte client
-module(client).

-export([
    print_user/3,
    print_user/4,
    add_user/3,
    add_user/4,
    disable_num/3,
    disable_num/4,
    delete_user/3,
    delete_user/4,
    delete_disabled/2,
    delete_disabled/3,
    set_plan/4,
    set_plan/5,
    set_services/4,
    set_services/5
]).

% DISTRIBUITO O CONCORRENTE
% uso pid locali per concorrente
% uso pid globali per distribuito
print_user(pid, Pid, ID) ->
    % self e' il Pid locale / globale
    Pid!{print, id, ID, self()},
    % aspetto il messaggio (sincrono)
    receive
        Msg -> Msg
    after
        3000 -> timeout_reached
    end
.

% DISTRIBUITO
% uso i nomi registrati
print_user(name, Nodo, ServerName, ID) ->
    % NB nel DISTRIBUITO
    % {nome_registrato, nome_nodo}!{msg} 
    % E' UGUALE A FARE: PidGlobale!{msg} 
    {ServerName, Nodo}!{print, id, ID, self()},
    receive
        Msg -> Msg
    after
        3000 -> timeout_reached
    end
.

% DISTRIBUITO O CONCORRENTE
add_user(pid, Pid, User) ->
    Pid!{add_user, User}
.

% DISTRIBUITO
add_user(name, Nodo, ServerName, User) ->
    {ServerName, Nodo}!{add_user, User}
.

% DISTRIBUITO O CONCORRENTE
disable_num(pid, Pid, Number) ->
    Pid!{disable_num, Number}
.

% DISTRIBUITO
disable_num(name, Nodo, ServerName, Number) ->
    {ServerName, Nodo}!{disable_num, Number}
.

% DISTRIBUITO O CONCORRENTE
delete_user(pid, Pid, UserId) ->
    Pid!{delete_user, UserId}
.

% DISTRIBUITO
delete_user(name, Nodo, ServerName, UserId) ->
    {ServerName, Nodo}!{delete_user, UserId}
.

% DISTRIBUITO O CONCORRENTE
delete_disabled(pid, Pid) ->
    Pid!{delete_disabled}
.

% DISTRIBUITO
delete_disabled(name, Nodo, ServerName) ->
    {ServerName, Nodo}!{delete_disabled}
.

% DISTRIBUITO O CONCORRENTE
set_plan(pid, Pid, Number, Plan) ->
    Pid!{set_plan, Number, Plan}
.

% DISTRIBUITO
set_plan(name, Nodo, ServerName, Number, Plan) ->
    {ServerName, Nodo}!{set_plan, Number, Plan}
.

% DISTRIBUITO O CONCORRENTE
set_services(pid, Pid, Number, Services) ->
    Pid!{set_services, Number, Services}
.

% DISTRIBUITO
set_services(name, Nodo, ServerName, Number, Services) ->
    {ServerName, Nodo}!{set_services, Number, Services}
.