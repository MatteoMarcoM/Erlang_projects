% BACKEND DELL'APPLICAZIONE TELECOM

% telecom provider
-module(provider).

% da includere per usare ets:fun2ms
-include_lib("stdlib/include/ms_transform.hrl").

% record dell'utente
% per caricarlo da shell:
% rd(usr, {num, id, status = enabled, plan, service = []}).
% plan = prepaid | flat
% status = enabled | disabled
% service = data | roaming | sms | voice 
-record(usr, {num, id, status = enabled, plan, service = []}).

-export([
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

% utilizziamo 3 tabelle
% una ets che contiene i record degli utenti
% una dets che fa il backup degli utenti
% una ets che mantiene il mapping telefono - utente
create_table() ->
    Res1 = ets:new(userRam, [named_table, {keypos, #usr.num}]),
    % la ets userIndex prende una tupla {num, id}
    Res2 = ets:new(userIndex, [named_table]),
    % la tabella su disco e' il backup della tabella utenti
    {ok, Res3} = dets:open_file(userDisk, [{file, "utenti"}, {keypos, #usr.num}]),
    {Res1, Res2, Res3}
.

close_table() ->
    % le ets vanno ricreate da zero
    ets:delete(userRam),
    ets:delete(userIndex),
    % la dets rimane su disco
    dets:close(userDisk)
.

% una volta chiuse le ets i dati sono andati persi
% vanno ripresi dalla dets che e' sul disco
restore_backup_table() ->
    % ricreo / riapro le tabelle
    provider:create_table(),
    % prendo ogni riga della dets e la mappo sulle ets
    PrendiRigaDets = 
        fun(
            #usr{
                num = Num,
                id = ID
                % il resto non mi interessa
            } = User) ->
                % ripopolo le ets con i dati della dets
                ets:insert(userIndex, {Num, ID}),
                ets:insert(userRam, User),
                % NB per la dets:traverse
                continue
        end
    ,
    % come una foreach sulla dets
    dets:traverse(userDisk, PrendiRigaDets),
    ok
.

% per caricare #usr{} da shell:
% rd(usr, {num, id, status = enabled, plan, service = []}).
get_user_shell(Num, ID, Status, Plan, Service) ->
    #usr{num=Num, id=ID, status=Status, plan=Plan, service=Service}
.

% NB pattern matching
add_user(
    #usr{
        num = Num, 
        id = ID
    } = User) ->
    % aggiorno userIndex
    ets:insert(userIndex, {Num, ID}),
    % aggiorno il resto delle tabelle
    update_user(User)
.

% inserisce l'intero record User
update_user(User) ->
    % ets degli utenti
    ets:insert(userRam, User),
    % dets di backup
    dets:insert(userDisk, User)
.

populate(N) ->
    lists:foreach(
        fun(Index) ->
            provider:add_user(#usr{
                num=5000+Index, 
                id=Index,
                % un piano su 3 e' prepayed 
                plan = 
                    case (Index rem 3) of
                        0 -> prepayed;
                        _ -> flat
                    end
                })
        end,
        lists:seq(1, N)
    )
.

% dato un numero disabilita la linea (status)
disable_num_user(Number) ->
    MS = ets:fun2ms(
        fun(#usr{
                num = Num,
                id = ID,
                status = _Status,
                plan = Plan,
                service = Service
            }) when (Num == Number) ->
                #usr{
                    num = Num,
                    id = ID,
                    % disabilito la linea
                    status = disabled,
                    plan = Plan,
                    service = Service
                }
        end
    ),
    % disabilito la linea
    ets:select_replace(userRam, MS),
    % la ets userIndex rimane ivariata
    % aggiorno la dets (sovrascrivo)
    % NB dets:select_replace() non esiste
    % ho un unico match perche' Number e' la chiave univoca
    Record = ets:lookup(userRam, Number),
    case Record == [] of
        true -> user_not_present;
        false ->
            % Record ha un unico elemento 
            [NewUser | _] = Record,
            % la chiave Number e' unica (sovrascritto)
            dets:insert(userDisk, NewUser)
    end
.    

% dato un utente lo cancella
delete_user(UserId) ->
    % ottengo il numero associato all'ID dell'utente
    % l'ID utente e' univoco
    Record = ets:match(
        userIndex,
        % {num_user, user_id}
        {'$1', UserId}
    ),
    case Record == [] of
        true -> user_not_present;
        false ->
            % Record ha un unico elemento 
            [[NumUser | _] | _] = Record,
            % elimino l'utente dalle tabelle
            ets:delete(userIndex, NumUser),
            ets:delete(userRam, NumUser),
            % la dets e' gia' su disco (modifica salvata)
            dets:delete(userDisk, NumUser)
    end
.

% elimina tutti gli utenti con status = disabled
delete_disabled() ->
    % metto a true il flag per "fissare" la tabella
    % così posso attraversare la tabella con un 
    % iteratore per avere consistenza in caso di concorrenza
    ets:safe_fixtable(userRam, true),
    % posso attraversare la tabella con l'iteratore
    % per eliminare gli utenti con numeri disabilitati
    % itero sui numeri di telefono
    provider:loop_delete_disabled(ets:first(userRam)),
    % rimetto il flag per "liberare" la tabella
    ets:safe_fixtable(userRam, false)
.

% funzione ausiliaria per traversare la ets userRam
% caso base della ricorsione (come EOF)
loop_delete_disabled('$end_of_table') -> ok;

% la chiave e' il numero
% caso generale della ricorsione (iterazione tabella)
loop_delete_disabled(Number) ->
    Record = ets:lookup(userRam, Number),
    case Record of
        [#usr{id = ID, status = disabled} | _] -> 
            provider:delete_user(ID);
        _ -> true
    end,
    % ricorsione sull'iteratore !
    provider:loop_delete_disabled(ets:next(userRam, Number))
.

% delete_disable versione "smart"
delete_disabled_smart() ->
    MS = ets:fun2ms(
        fun(#usr{
            id = ID, 
            num = Num, 
            status = disabled}) -> ID 
        end
    ),
    ToDelete = ets:select(userRam, MS),
    lists:foreach(
        fun(ID) ->
            provider:delete_user(ID)
        end,
        ToDelete
    )
.

% funzioni di utilita' per server esterno
% uso del polimorfismo per le funzioni 
% (passo degli atomi per distinguerle)
% definire tutti i casi (usare ;)
print_user(num, Number) -> ets:lookup(userRam, Number);

print_user(id, ID) ->
    Return = ets:match(
        userIndex,
        % {num_user, user_id}
        {'$1', ID}
    ),
    case Return of
        [] -> user_not_found;
        [[NumUser | _] |_] ->
            % Record ha un unico elemento
            ets:lookup(userRam, NumUser)
    end     
.

% funzione che modifica il piano di una utenza
% plan = prepaid | flat
set_plan(Number, Plan) ->
    MS = ets:fun2ms(
        fun(#usr{
                num = Num,
                id = ID,
                status = Status,
                plan = _P,
                service = Service
            }) when (Num == Number) ->
                #usr{
                    num = Num,
                    id = ID,
                    status = Status,
                    % modifico il piano
                    plan = Plan,
                    service = Service
                }
        end
    ),
    % disabilito la linea
    ets:select_replace(userRam, MS),
    % la ets userIndex rimane ivariata
    % aggiorno la dets (sovrascrivo)
    % NB dets:select_replace() non esiste
    % ho un unico match perche' Number e' la chiave univoca
    Record = ets:lookup(userRam, Number),
    case Record == [] of
        true -> user_not_present;
        false ->
            % Record ha un unico elemento 
            [NewUser | _] = Record,
            % la chiave Number e' unica (sovrascritto)
            dets:insert(userDisk, NewUser)
    end
.    

% funzione che modifica i servizi di una utenza
% service = data | roaming | sms | voice 
% NB service è una lista
set_services(Number, Services) ->
    MS = ets:fun2ms(
        fun(#usr{
                num = Num,
                id = ID,
                status = Status,
                plan = Plan,
                service = _S
            }) when (Num == Number) ->
                #usr{
                    num = Num,
                    id = ID,
                    status = Status,
                    plan = Plan,
                    % modifico i servizi
                    service = Services
                }
        end
    ),
    % disabilito la linea
    ets:select_replace(userRam, MS),
    % la ets userIndex rimane ivariata
    % aggiorno la dets (sovrascrivo)
    % NB dets:select_replace() non esiste
    % ho un unico match perche' Number e' la chiave univoca
    Record = ets:lookup(userRam, Number),
    case Record == [] of
        true -> user_not_present;
        false ->
            % Record ha un unico elemento 
            [NewUser | _] = Record,
            % la chiave Number e' unica (sovrascritto)
            dets:insert(userDisk, NewUser)
    end
.