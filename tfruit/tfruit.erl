% fruit shop con le tabelle
-module(tfruit).
-include_lib("stdlib/include/ms_transform.hrl").
-import(timer, [sleep/1]).
-export([
    shop/1,
    init/3,
    start/2,
    populate/0,
    demo_shop/2,
    removeCart/2,
    verifyCart/2
]).

% shop server
shop(TableName) ->
    receive
        {print, Pid} -> 
            Pid!{print_res, 
                    ets:match_object(
                        TableName, 
                        {'$1', '$2', '$3'}
                    )
                },
            tfruit:shop(TableName);
        {price, FruitType, Pid} ->
            % la chiave primaria e' la frutta ed e' univoca
            Riga = ets:lookup(TableName, FruitType),
            case Riga of
                [] -> Pid!{price_res, fruit_not_present};
                [Head | _Tail] -> 
                    {_F, _Q, Prezzo} = Head,
                    Pid!{price_res, Prezzo}
            end,
            tfruit:shop(TableName);
        {add, Frutto, Pid} ->
            {FruitType, Quantity, _Price} = Frutto,
            Riga = ets:lookup(TableName, FruitType),
            case Riga of
                [] -> ets:insert(TableName, Frutto);
                [Head | _Tail] -> 
                    {_F, Qt, Pr} = Head,
                    % la chiave FruitType e' univoca quindi l'insert
                    % aggiorna il valore se trova corispondenza
                    % senza aggiungere un altro elemento con stessa chiave
                    % cosa che succederebbe con una tabella di tipo bag
                    ets:insert(TableName, {FruitType, Quantity + Qt, Pr})
            end,
            Pid!{add_res, fruit_added},
            tfruit:shop(TableName);
        {delete, FruitType, Pid} ->
            Flag = ets:match_delete(
                TableName, 
                {FruitType, '$2', '$3'}
            ),
            Pid!{deleted, Flag},
            tfruit:shop(TableName);
        {total_price, Pid} ->
            % foldl su tabella
            TotalPrice = ets:foldl(
                fun(Elem, Acc) ->
                    {_Fr, Qt, Pr} = Elem,
                    (Qt * Pr) + Acc
                end,
                0,
                TableName    
            ),
            Pid!{total_price_res, TotalPrice},
            tfruit:shop(TableName);
        {buy, Cart, Pid} ->
            % Cart è una lista con elementi della forma: 
            % {frutta, quantita, prezzo} 
            % che il cliente vuole comprare
            % 0) verificare prima che Cart può essere acquistato 
            % 1) calcolare il costo della spesa
            % 2) modificare lo store di conseguenza
            % 3) tornare al cliente il prezzo da pagare

            % 0) verificare prima che Cart può essere acquistato
            Condition = verifyCart(Cart, TableName),
            
            case Condition of
                % se ok continua
                {ok, _} -> true;
                % display error message
                {error, Msg} -> io:format("ERROR: ~p \n", [Msg])
            end,

            % 1) calcolare il costo della spesa
            TotalCartPrice = lists:foldl(
                fun(Elem, Acc) ->
                    {_Fr, Qt, Pr} = Elem,
                    (Qt * Pr) + Acc
                end,
                0,
                Cart    
            ),

            % 2) modificare lo store di conseguenza 
            tfruit:removeCart(Cart, TableName),
            
            % 3) tornare al cliente il prezzo da pagare
            Pid!{price_cart_res, TotalCartPrice},

            % chiamata ricorsiva con variabile NewTable modificata
            tfruit:shop(TableName);
        {stop, Pid} ->
            % salvo la tabella su file
            ets:tab2file(TableName, "miodump"),
            io:format("DUMP COMPLETED!\n"),
            % cancello dalla RAM
            ets:delete(TableName),
            % notifico la terminazione
            Pid!{server_stopped}
    end 
.

% verify Cart
verifyCart(Cart, StoreTable) ->
    % Cart = [{fruit1, quantity1, price1}, ...]
    % 0) verificare prima che Cart può essere evaso
    case Cart of
        % formato conforme al pattern {error, _DetailList}
        [] -> {ok, [purchase_allowed]};
        [CartHead | CartTail] ->
            {FrCart, QtCart, _PrCart} = CartHead,
            NextElem = ets:lookup(StoreTable, FrCart),
            case NextElem of
                % non ho trovato occorrenze di Fruit gia' presenti
                % non posso acquistare frutta non presente
                [] -> {error, [fruit_not_present, FrCart]};
                % verifico che le quantita' siano rispettate
                [{_FrStore, QtStore, _PrStore} | _Tail] -> 
                    Condition =  (QtStore >= QtCart),
                    case Condition of
                        true -> tfruit:verifyCart(CartTail, StoreTable);
                        false -> {error, [not_enought_pices, FrCart]}
                    end
            end;
        % se arrivo qua il pattern e' sbagliato
        _ -> {error, [wrong_cart_structure, Cart]}
    end
.

% remove Cart
removeCart(Cart, StoreTable) ->
    % Cart = [{fruit1, quantity1, price1}, ...]
    % 0) verificare prima che Cart può essere evaso
    case Cart of
        % formato conforme al pattern {error, _DetailList}
        [] -> {ok, [purchase_done]};
        [CartHead | CartTail] ->
            {FrCart, QtCart, _PrCart} = CartHead,
            % NB ho gia' controllato con verifyCart() 
            RemoveQuantityMS = ets:fun2ms(
                fun({F, Q, _P}) 
                    when F == FrCart ->
                      % aggiorno la quantita'
                      {F, Q - QtCart, _P}
                end
            ),
            ets:select_replace(StoreTable, RemoveQuantityMS),
            % chiamata ricorsiva con store aggiornato
            tfruit:removeCart(CartTail, StoreTable);
        % se arrivo qua il pattern e' sbagliato
        _ -> {error, [wrong_cart_structure, Cart]}
    end
.

% funzione per popolare il DB di frutta
populate() -> [
    % {frutta, quantita', prezzo}
    {banana, 100, 1},
    {orange, 50, 1},
    {apple, 10, 3},
    {wallnuts, 400, 4}
].

% inizializzazione
init(TableName, ServerName, ShellPid) -> 
    % registro il server se il nome non e' gia' usato
    % altrimenti esco con un errore
    case whereis(ServerName) == undefined of
        % se il nome e' gia' in uso per registrare
        % di nuovo quel nome il server deve terminare
        false -> ShellPid!{error, server_is_running};
        true ->
            % posso eseguire init() 
            register(ServerName, self()),
            % verifico se la tabella e' gia' stata creata
            % nel caso la carico altrimenti la creo
            % NB il nome e' sempre quello di prima
            {TableExist, _TableName} = ets:file2tab("miodump"),
            case TableExist of
                ok -> true;
                % la tabella non esiste, deve essere creata da nuovo
                error -> 
                    % creo la tabella per la frutta con nome 
                    ets:new(TableName, [named_table, public]),
                    % popolo la tabella
                    lists:foreach(
                        fun(Elem) -> 
                            ets:insert(TableName, Elem) 
                        end, 
                        tfruit:populate()
                    )
            end,
            % ho finito di registrare TableName e ServerName
            ShellPid!{registration_completed},
            tfruit:shop(TableName)
    end
.

% lancio del server (da shell)
start(TableName, ServerName) ->
    ShellPid = self(),
    spawn(tfruit, init, [TableName, ServerName, ShellPid])
.

% demo di utilizzo
% implementa la funzione buy
% invoca con demo_shop(t1, s1)
demo_shop(TableName, ServerName) ->
    tfruit:start(TableName, ServerName),
    % attendo che la registrazione sia completata
    receive
        {error, Error} -> io:format(">>> ERROR: ~p\n", [Error]);
        {registration_completed} -> 
            % posso eseguire il resto della funzione 
            ServerName!{print, self()},
            receive
                {print_res, ShopList} -> io:format("Shop list is: ~p\n", [ShopList])
            end,
            % chiedo il prezzo delle banane
            ServerName!{price, banana, self()},
            receive
                {price_res, Price} -> io:format("Bananas price is: ~p\n", [Price])
            end,
            % elimino le banane
            io:format("Deleting bananas...\n"),
            ServerName!{delete, banana, self()},
            % stampa il DB
            ServerName!{print, self()},
            receive
                {print_res, ShopList2} -> io:format("Shop list is: ~p\n", [ShopList2])
            end,
            % riaggiungo le banane
            io:format("Adding 20 bananas...\n"),
            ServerName!{add, {banana, 20, 1}, self()},
            % stampa il DB
            ServerName!{print, self()},
            receive
                {print_res, Price2} -> io:format("Shop list is: ~p\n", [Price2])
            end,
            % chiedo il prezzo totale
            ServerName!{total_price, self()},
            receive
                {total_price_res, TotalPrice} -> io:format("Total price is: ~p\n", [TotalPrice])
            end,
            % compro 5 banane
            io:format("Buying 5 bananas...\n"),
            ServerName!{buy, [{banana, 5, 1}], self()},
            receive
                {price_cart_res, TotalCartPrice} -> io:format("Total Cart price is: ~p\n", [TotalCartPrice])
            end,
            % stampa il DB
            ServerName!{print, self()},
            receive
                {print_res, ShopList3} -> io:format("Shop list is: ~p\n", [ShopList3])
            end,
            % stopping the server
            io:format("Stoppig the server (dump)...\n"),
            % fermo il  server (e viene deregistrato)
            ServerName!{stop, self()},
            % attendo la teminazione
            receive
                {server_stopped} -> io:format("SERVER STOPPED!\n")
            end
            % ora posso far ripartire il server
    end
.