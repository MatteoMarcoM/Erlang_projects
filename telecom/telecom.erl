% API DELL'APPLICAZIONE DI TELECOM

-module(telecom).

% utilizzo il modulo provider per gestire il servizio
% importo il modulo provider (backend del servizio)
-import(provider, [
    create_table/0,
    close_table/0,
    add_user/1,
    update_user/1,
    get_user_shell/5,
    restore_backup_table/0,
    populate/1,
    disable_num_user/1,
    delete_user/1,
    loop_delete_disabled/1,
    delete_disabled/0,
    delete_disabled_smart/0,
    print_user/2,
    set_plan/2,
    set_services/2
]).

% funzioni da esportare
-export([
    loop/0,
    init_server/1,
    start_local_server/2,
    start_remote_server/3
]).

% API del servizio
loop() ->
    receive
        {stop} -> provider:close_table();
        {print, id, ID, Pid} ->
            % NB nel DISTRIBUITO
            % {nome_registrato, nome_nodo}!{msg} 
            % E' UGUALE A FARE: PidGlobale!{msg} 
            Pid!{provider:print_user(id, ID)},
            telecom:loop();
        {print, num, Num, Pid} -> 
            Pid!{provider:print_user(num, Num)},
            telecom:loop();
        {add_user, User} -> 
            provider:add_user(User),
            telecom:loop();
        {disable_num, Number} -> 
            provider:disable_num_user(Number),
            telecom:loop();
        {delete_user, UserId} -> 
            provider:delete_user(UserId),
            telecom:loop();
        {delete_disabled} ->
            provider:delete_disabled(),
            telecom:loop();
        {set_plan, Number, Plan} ->
            provider:set_plan(Number, Plan),
            telecom:loop();
        {set_services, Number, Services} ->
            provider:set_services(Number, Services),
            telecom:loop()
    end
.

% creare un server loop che risponde ai messaggi (API)
init_server(N) ->
    % inizializzo il server e lo eseguo
    % le tablelle le carico solo all'inizio
    provider:restore_backup_table(),
    % popolo le tablelle solo una volta
    provider:populate(N),
    telecom:loop()
.

%%%%%%%%%%%%% CONCORRENZA
% spawno il server in un nodo locale (SPAWN/1)
% lui si auto-registra
start_local_server(ServerName, N) ->
    % spawno il server (SPAWN/1)
    spawn(
        fun() ->
            % registro il server
            register(ServerName, self()),
            telecom:init_server(N)
        end
    )
.

%%%%%%%%%%%%% DISTRIBUZIONE
% spawno il server in un nodo remoto (SPAWN/2)
% lui si auto-registra
start_remote_server(Node, ServerName, N) ->
    % spawno il server (SPAWN/2)
    spawn(
        Node,
        fun() ->
            % registro il server
            register(ServerName, self()),
            telecom:init_server(N)
        end
    )
.