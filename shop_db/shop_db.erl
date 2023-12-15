% creo uno shop con un DB distrbuito (Mnesia DBMS)
-module(shop_db).

% per fare query complesse
-include_lib("stdlib/include/qlc.hrl").

-export([
    init_only_once/0,
    init_only_once_distrib/1,
    start/0,
    start_distrib/1,
    start_remote/1,
    stop_remote/1,
    populate/0,
    print_fruit/1,
    delete_fruit/1,
    delete_all/2,
    read_stats/1,
    add_more/2,
    select_join/2,
    print_all_shop/0,
    print_all_shop_smart/0,
    print_shop_value/0,
    buy_fruit/1,
    increase_all/1
]).

% definisco i record che utilizzero'
% costo per il pubblico
-record(shop, {item, quantity, cost}).
% costo per il venditore (che compra dal fornitore)
-record(cost, {name, price}).

% LOCALE
% da chiamare SOLO UNA VOLTA per creare il DB Mnesia LOCALE
% serve per creare i file del DB sui vari nodi (installare il DB)
init_only_once() ->
    % creo il DB solo in locale -> node()
    Nodelist = [node()],
    mnesia:create_schema(Nodelist),
    % faccio partire Mnesia
    mnesia:start(),
    % creo lo schema delle due tablelle del DB coi campi che prendo dai records
    ShopFields = record_info(fields, shop),
    CostFields = record_info(fields,cost),
    % NB il nome della tabella e' == al nome dei records che essa ospita
    % specifico i parametri opzionali per avere una copia del DB
    % su disco e in RAM anche nei nodi distribuiti
    mnesia:create_table(shop,[
        {attributes, ShopFields},
        {disc_copies, Nodelist}
    ]),
    mnesia:create_table(cost,[
        {attributes, CostFields},
        {disc_copies, Nodelist}
    ]),
    mnesia:stop(),
    ok
.

% LOCALE
% da utilizzare OGNI VOLTA che si vuole usare il DB
% va invocato su ogni nodo del sistema
start() ->
    % faccio partire il DBMS Mnesia
    mnesia:start(),
    % aspetto (5 sec) che vengano caricate le tabelle del DB (distribuito)
    mnesia:wait_for_tables([shop, cost], 5000)
.

% DISTRIBUITA
% da chiamare SOLO UNA VOLTA per creare il DB Mnesia DISTRIBUITO
% serve per creare i file del DB sui vari nodi (installare il DB)
init_only_once_distrib(RemoteNodeList) ->
    % creo il DB in locale -> node() e in remoto 
    % nei nodi che presenti in RemoteNodeList
    Nodelist = [node()] ++ RemoteNodeList,
    mnesia:create_schema(Nodelist),
    % faccio partire Mnesia nei nodi remoti e da me
    shop_db:start_remote(RemoteNodeList),
    % come nella versione non distribuita (locale)
    ShopFields = record_info(fields, shop),
    CostFields = record_info(fields,cost),
    % specifico i parametri opzionali per avere una copia del DB
    % su disco e in RAM anche nei nodi distribuiti
    mnesia:create_table(shop,[
        {attributes, ShopFields},
        {disc_copies, Nodelist}
    ]),
    mnesia:create_table(cost,[
        {attributes, CostFields},
        {disc_copies, Nodelist}
    ]),
    % stop per ogni nodo remoto e per me
    shop_db:stop_remote(RemoteNodeList)
.

% DISTRIBUITA
% fa partire Mnesia e carica le tabelle
start_distrib(RemoteNodeList) ->
    % start per ogni nodo remoto e per me
    shop_db:start_remote(RemoteNodeList),
    % aspetto (5 sec) che vengano caricate le tabelle del DB (distribuito)
    mnesia:wait_for_tables([shop, cost], 5000)
.

% DISTRIBUITA
% fa partire Mnesia ma non carica le tabelle
start_remote(RemoteNodeList) ->
    % NB nel DISTRIBUITO devo far partire Mnesia da dentro
    % ciascun nodo remoto e poi lo faccio partire da me in locale
    MioPid = self(),
    lists:foreach(
        fun(Node) ->
            spawn(
                Node, 
                fun() ->
                    % NB devo farlo "dentro" ogni nodo remoto 
                    mnesia:start(),
                    MioPid!{mnesia_started} 
                end
            ) 
        end,
        RemoteNodeList
    ),
    % mi metto in attesa che Mnesia sia partito in tutti i nodi
    % remoti e poi lo eseguo nel mio nodo locale (master)
    lists:foreach(
        fun(_Node) -> 
            receive 
                {mnesia_started} -> ok 
            end
        end,
        RemoteNodeList
    ),
    % faccio partire Mnesia in locale
    mnesia:start()
.

% DISTRIBUITA
% stop per ogni nodo remoto e per me
stop_remote(RemoteNodeList) ->
    % NB nel DISTRIBUITO devo far terminare Mnesia da dentro
    % ciascun nodo remoto e poi lo faccio terminare da me in locale
    MioPid = self(),
    lists:foreach(
        fun(Node) ->
            spawn(
                Node, 
                fun() ->
                    % NB devo farlo "dentro" ogni nodo remoto 
                    mnesia:stop(),
                    MioPid!{mnesia_stopped} 
                end
            ) 
        end,
        RemoteNodeList
    ),
    % mi metto in attesa che Mnesia sia terminato in tutti i nodi
    % remoti e poi lo termino nel mio nodo locale (master)
    lists:foreach(
        fun(_Node) -> 
            receive 
                {mnesia_stopped} -> ok 
            end
        end,
        RemoteNodeList
    ),
    % faccio terminare Mnesia in locale
    mnesia:stop()
.

% per popolare il DB
populate() ->
    MyShop = [
        #shop{item = banana, quantity = 100, cost = 1.1},
        #shop{item = apple, quantity = 200, cost = 1.2},
        #shop{item = orange, quantity = 300, cost = 1.3},
        #shop{item = potato, quantity = 400, cost = 1.4},
        #shop{item = pear, quantity = 500, cost = 1.5}
    ],
    % faccio due cose separate per le due tabelle del DB
    MyCost = [
        #cost{name = banana, price = 0.1},
        #cost{name = apple, price = 0.2},
        #cost{name = orange, price = 0.3},
        #cost{name = potato, price = 0.4},
        #cost{name = pear, price = 0.5}
    ],
    % le operazioni sul DB vanno fatte con delle transazioni (atomiche e consistenti)
    % che prendono solo funzioni lambda: fun() -> ... end
    % altrimenti abbiamo race condition, deadlock e starvation
    InsertValue =
        fun() ->
            % inserisco i vari elementi
            lists:foreach(
                fun(Record) ->
                    % NB la mnesia:write() CAPISCE in quale tabella inserire i records
                    % usando il tipo del record == nome della tabella
                    mnesia:write(Record)
                end,
                % per entrambe le tabelle
                MyShop ++ MyCost
            )
        end
    ,
    % eseguo la transazione in maniera sicura sul DB
    mnesia:transaction(InsertValue)
.

% operazione di lettura da tabella shop
print_fruit(Fruit) ->
    % ID oggetto da cercare == {nome_tab, key}
    ObjectID = {shop, Fruit},
    {atomic, Return} = mnesia:transaction(
        fun() ->
            % la read vuole l'ID (chiave e tabella)
            mnesia:read(ObjectID)
        end
    ),
    % ritorno il valore restituito dalla mnesia:read()
    Return
.

% operazione di delete (entrambe le tabelle)
delete_fruit(Fruit) ->
    % ID oggetto da eliminare == {nome_tab, key}
    % NB per cancellarla da entrambe le tabelle
    % servono due diversi object ID
    ObjectID1 = {shop, Fruit},
    ObjectID2 = {cost, Fruit},
    {atomic, Return} = mnesia:transaction(
        fun() ->
            % la delete vuole l'ID (chiave e tabella)
            mnesia:delete(ObjectID1),
            mnesia:delete(ObjectID2)
        end
    ),
    Return
.

% ListToDel e' una lista di chiavi
delete_all(ListToDel, TableName) ->
    % mi creo una lista di Object ID
    % un ID per ogni entry da eliminare
    ObjectIDList = lists:map(
        fun(Fruit) ->
            % la delete vuole l'ID (chiave e tabella)
            % ID oggetto da eliminare == {nome_tab, key}
            {TableName, Fruit}
        end,
        ListToDel
    ),
    DeleteFun =
        fun() ->
            % elimino tutti i record tramite gli ID creati
            lists:foreach(
                fun(ObjectID) ->
                    mnesia:delete(ObjectID)
                end,
                ObjectIDList
            )
        end
    ,
    mnesia:transaction(DeleteFun)
.

% NB operazioni DIRTY sono VELOCI ma non garantiscono CONSISTENZA (LOCK)
% le uso SOLO SE sono sicuro che non ho race condition ecc.
% NON uso la mnesia:transaction() ma USO invece:
% mnesia:dirty_write(), mnesia:dirty_read(), ecc.

% provo una operazione di lettura nelle due modalita' e le cronometro
read_stats(Fruit) ->
    ObjectID = {shop, Fruit},
    % conometro una lettura dirty: mnesia:dirty_read(ObjectID)
    {DirtyTime, DirtyResult} = timer:tc(mnesia, dirty_read, [ObjectID]),
    % conometro una lettura standard: mnesia:read(ObjectID)ù
    ReadFun = fun() -> mnesia:read(ObjectID) end,
    {Time, {atomic, Result}} = timer:tc(mnesia, transaction, [ReadFun]),
    {DirtyTime, Time, Result == DirtyResult}
.

% UPDATE con read e write
% leggo e aggiorno la quantita'
add_more(Fruit, NewQuantity) ->
    % entrambe le operazioni (read e write) le includo nella stessa transaction
    Fun =
        fun() ->
            ObjectID = {shop, Fruit},
            % essendo shop un set, la chiave e' univoca (ritorna un elemento)
            [Entry] = mnesia:read(ObjectID),
            % accedo al record Entry, di tipo shop, campo quantity
            OldQuantity = Entry#shop.quantity,
            % aggiorno il valore quantity del record Entry (modifico la variabile)
            NewFruit = Entry#shop{quantity = OldQuantity + NewQuantity},
            % sovrascrivo il nuovo valore
            mnesia:write(NewFruit)
        end
    ,
    % verifico il successo della transazione e termino
    {atomic, _Result} = mnesia:transaction(Fun),
    ok
.

% operazione di JOIN su tabelle
% faccio una query similmente a una Match Specification (MS)
% importare libreria qlc

% SELECT shop.*, cost.*
% FROM shop, cost
% JOIN shop.item = cost.name
% WHERE  shop.quantity > Number AND cost.price < Price
select_join(Number, Price) ->
    % creo la query sul DB (SELECT)
    Query = qlc:q(
        % list comprehension
        [ {X, Y} ||
            % seleziona tutte righe tabella shop
            X <- mnesia:table(shop),
            % seleziona tutte righe tabella cost
            Y <- mnesia:table(cost),
            % condizione di JOIN
            Y#cost.name =:= X#shop.item,
            % condizione di WHERE #1
            X#shop.quantity > Number,
            % condizione di WHERE #2
            Y#cost.price < Price
        ]
    ),
    % invoco la query dentro una transazione e ritorno il risultato
    Fun = fun() -> qlc:e(Query) end,
    {atomic, Result} = mnesia:transaction(Fun),
    Result
.

% uso la foldl per ottenere un aggregato attraversando la tabella
print_all_shop() ->
    % funzione da passare alla foldl/foldr per ottenere una lista
    % di valori [{Item, Quantity, Shop} | ...] da stampare a video alla fine
    ListToPrintFun =
        fun(
            #shop{item = I, quantity = Q, cost = C},
            Acc
        ) ->
            % NON VOGLIO SIDE EFFECT quando uso il DB (stampo a fine transazione) !
            Acc ++ [{I, Q, C}]
        end
    ,
    % eseguo la transazione
    Transaction = fun() -> mnesia:foldl(ListToPrintFun, [], shop) end,
    % Result e' il risultato della foldl (lista di {I, Q, C} da stampare)
    {atomic, Result} = mnesia:transaction(Transaction),
    % stampo a video SOLO a transazione terminata (atomicita')
    lists:foreach(
        fun({I, Q, C}) ->
            io:format("Fruit: ~p, quantity: ~p, cost: ~p\n", [I, Q, C])
        end,
        Result
    )
.

% versione smart
print_all_shop_smart() ->
    Query = qlc:q(
        % list comprehension
        [ X ||
            % seleziona tutte righe shop
            X <- mnesia:table(shop)
        ]
    ),
    Fun = fun() -> qlc:e(Query) end,
    {atomic, Result} = mnesia:transaction(Fun),
    % stampo a video SOLO a transazione terminata (atomicita')
    lists:foreach(
        % NB record #shop{item, quantity, cost} in memoria e' cosi':
        % {shop, item, quantity, cost}
        fun({shop, I, Q, C}) ->
            io:format("Fruit: ~p, quantity: ~p, cost: ~p\n", [I, Q, C])
        end,
        Result
    )
.

% stampare per ogni frutto il valore presente in magazzino
% inteso come prezzo * numero di item
print_shop_value() ->
    % funzione da passare alla foldl/foldr per ottenere una lista
    % di valori [{Item, Quantity, Shop} | ...]
    ListToPrintFun =
        fun(
            #shop{item = I, quantity = Q, cost = C},
            Acc
        ) ->
            % NON VOGLIO SIDE EFFECT quando uso il DB (stampo a fine transazione) !
            Acc ++ [{I, Q, C}]
        end
    ,
    % eseguo la transazione
    Transaction = fun() -> mnesia:foldl(ListToPrintFun, [], shop) end,
    % IQCListResult e' il risultato della foldl (lista di {I, Q, C} da stampare)
    {atomic, IQCListResult} = mnesia:transaction(Transaction),
    % calcolo il valore totale dello shop
    TotalValue = lists:foldl(
        fun({_I, Q, C}, Acc) ->
            Acc + Q*C
        end,
        0,
        IQCListResult
    ),
    % stampo a video SOLO a transazione terminata (atomicita')
    lists:foreach(
        fun({I, Q, C}) ->
            io:format("Fruit: ~p, quantity: ~p, cost: ~p, total fruit value: ~p\n", [I, Q, C, Q*C])
        end,
        IQCListResult
    ),
    io:format("Total shop value is: ~p\n", [TotalValue])
.

% evadere la lista della spesa se e' possibile acquistare la frutta
% 1) verificare che la frutta e' disponibile (per ogni frutto)
% 2) sottrare dal DB la quantita' da acquistare (per ogni frutto)
% INVECE di effettuare 1) per ogni frutto POI 2) per ogni frutto
% faccio tutto in una UNICA TRANSAZIONE (altrimenti abortisco l'intera transazione)
buy_fruit(CartList) ->
    % CartList e' il carrello della spesa
    % CartList = [{F, Q}, ...]
    BuyFun = 
        fun({F, Q}) ->
            % mi creo l'Object ID del prossimo frutto F
            ObjectID1 = {shop, F},
            % leggo dal DB il frutto F
            [Fruit] = mnesia:read(ObjectID1),
            % controllo la giacenza nel DB del frutto F
            GiacenzaFruit = Fruit#shop.quantity,
            case GiacenzaFruit >= Q of
                true ->
                    % creo un nuovo frutto uguale a Fruit ma con
                    % quantita' modificata (acquisto)  
                    NewFruit = Fruit#shop{quantity = GiacenzaFruit - Q},
                    % sovrascrivo (la chiave primaria e' univoca)
                    mnesia:write(NewFruit);
                false ->
                    % DEVO ABORTIRE L'INTERA TRANSAZIONE !
                    mnesia:abort("ERROR: Not enough item in shop: " ++ atom_to_list(F))
            end
        end
    ,
    % eseguo la transazione in una unica volta su tutto il carrello
    % se non riesco ad acquistare tutto il carrello abortisco la transazione
    Transaction = fun() -> lists:foreach(BuyFun, CartList) end,
    % Result contiene l'esito della transazione -> atomic / aborted
    Result = mnesia:transaction(Transaction),
    % se posso acquistere ritorno il costo complessivo del carrello
    case Result of
        % la transazione e' andata a buon fine
        {atomic, _ResultOfFun} ->
            TotalCostFun = 
                % per restituire il costo totale mi serve sapere il
                % costo unitario che trovo all'interno del DB
                fun({Fruit, Qt}, Acc) ->
                    ObjectID2 = {shop, Fruit},
                    [Fr] = mnesia:read(ObjectID2),
                    Acc + (Fr#shop.cost * Qt)
                end
            ,
            % NB TotalCostFun opera sul DB -> transazione!
            Transaction2 = fun() -> lists:foldl(TotalCostFun, 0, CartList) end,
            {atomic, Res} = mnesia:transaction(Transaction2),
            % ritorno il prezzo
            Res;
        % la transazione e' fallita
        {aborted, Reason} -> {aborted, Reason}
    end
.

% aumenta il prezzo di tutti i prodotti ad es. se aumenta l'IVA
increase_all(Ratio) ->
    IncreaseFun =
        % simile a una foreach (non uso Acc)
        fun(#shop{cost = C} = Frutto, _Acc) ->
            NewPrice = C + C * Ratio,
            mnesia:write(Frutto#shop{cost = NewPrice})
        end
    ,
    % NB non esiste mnesia:foreach() quindi uso mnesia:foldl()
    Transaction = fun() -> mnesia:foldl(IncreaseFun, 0, shop) end,
    mnesia:transaction(Transaction)
.

% %%%%%%%%%%% DEMO
% %%%%%%%%%%% LOCALE
% shop_db:init_only_once().
% shop_db:start().
% shop_db:populate().
% shop_db:print_all_shop().
% shop_db:add_more(banana, 10).
% shop_db:print_fruit(banana).
% shop_db:delete_fruit(banana).
% shop_db:print_all_shop_smart().
% ...
% mnesia:stop().
% %%%%%%%%%%% DISTRIBUITA
% primo nodo pippo@host:
% erl -sname pippo -setcookie disney
% secondo nodo pluto@host:
% erl -sname pluto -setcookie disney
% (pippo@host)> shop_db:init_only_once_distrib(['pluto@host']).
% (pippo@host)> shop_db:start_distrib(['pluto@host']).
% (pippo@host)> shop_db:populate().
% (pluto@host)> shop_db:print_all_shop_smart().
% ...
% (pippo@host)> shop_db:stop_remote('pluto@host).