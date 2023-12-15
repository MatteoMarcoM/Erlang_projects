-module(fruit).
-export([
    price/1,
    shop/1,
    init/0,
    verifyCart/2,
    populate/0,
    removeCart/2,
    demo/0
]).

% database
populate() -> [{banana, 10}, {orange, 10}].

% funzione per il prezzo della frutta
price(orange) -> 5;
price(chestnut) -> 5;
price(banana) -> 5;
% default price
price(_) -> 123.

% fruit shop server
% StoreList = {frutta, quantita}
shop(StoreList) ->
    % i pattern della receive sono gli endpoint del sever (API)
    receive
        % messaggio per fermare il server
        {stop} -> ok;
        {print, Pid} -> 
            PrettyPrintFun = fun({Fruit, Quantity}) -> io:format("~p: ~p \n", [Fruit, Quantity]) end, 
            lists:foreach(PrettyPrintFun, StoreList),
            % uso il Pid per inviare un ACK per gestire la concorrenza sulla 
            % io:format della shell
            Pid!{ack, io_terminated},
            % chiamata ricorsiva con variabile StoreList non modificata
            fruit:shop(StoreList);
        % NB se e' già presente aumento la quantita', altrimenti aggiungo
        {add, Fruit, Quantity} ->
            % controllo prima se Fruit è presente nello store
            % 1) creo un filtro che mi torna una lista di tuple 
            % il cui primo elemento è == Fruit (ricevuto dal messaggio)
            % 2) la lista filtro è formata da 1 elemento o è vuota
            % 3) verifico che il risultato della filter non sia la lista vuota. 
            % In questo caso vuol dire che il frutto da aggiungere è già presente
            % nel mio store e devo solo AGGIORNARE la quantità
                             
            % filtro lista con tuple {F, _} t.c. Fruit == F 
            FlagList = lists:filter(fun({F, _}) -> Fruit == F end, StoreList),
            % restituisce una tupla con un unico elemento o la lista vuota
            case FlagList of
                % lista con un elemento (ho trovato la frutta che cercavo)
                [_H | _T] -> 
                    AddQt = fun(X) ->
                        case X of
                            % Fruit e' la frutta che cercavo
                            {Fruit, Qt} -> {Fruit, Qt + Quantity};
                            % altrimenti funzione identita' (nessuna modifica)
                            _ -> X
                        end
                    end,
                    NuovoStore = lists:map(AddQt, StoreList),
                    % chiamata ricorsiva con store aggiornato
                    fruit:shop(NuovoStore);
                % non ho trovato occorrenze di Fruit gia' presenti
                % aggiungo frutto e quantita' nuovi
                [] -> fruit:shop(StoreList ++ [{Fruit, Quantity}])
            end;
        % Cart è una lista di tuple {frutta, quantita} che il cliente vuole comprare
        % 0) verificare prima che Cart può essere acquistato 
        % 1) calcolare il costo della spesa
        % 2) modificare lo store di conseguenza
        % 3) tornare al cliente il prezzo da pagare
        {buy, Cart, Pid} ->
            
            % 0) verificare prima che Cart può essere evaso
            Condition = verifyCart(Cart, StoreList),
            
            case Condition of
                % se ok continua
                {ok, _} -> true;
                % display error message
                {error, Msg} -> io:format("ERROR: ~p \n", [Msg])
            end,

            % 1) calcolare il costo della spesa
            PricesList = lists:map(fun({Fr, Qt}) -> price(Fr) * Qt end, Cart),
            TotalPrice = lists:foldl(
                fun(Elem, Acc) -> Elem + Acc end, 
                0, 
                PricesList
            ),

            % 2) modificare lo store di conseguenza 
            NewStore = removeCart(Cart, StoreList),
            
            % 3) tornare al cliente il prezzo da pagare
            Pid!{price, TotalPrice},

            % chiamata ricorsiva con variabile StoreList modificata
            fruit:shop(NewStore)
    end
.

% FUNZIONA
% verify Cart
verifyCart(Cart, StoreList) ->
    % Cart = [{fruit1, quantity1}, {fruit2, quantity2}, ...]
    % 0) verificare prima che Cart può essere evaso
    case Cart of
        % formato conforme al pattern {error, _DetailList}
        [] -> {ok, [purchase_allowed]};
        [CartHead | CartTail] ->
            case CartHead of 
                {FrCart, QtCart} ->
                    FlagStoreL = lists:filter(fun({F, _}) -> FrCart == F end, StoreList),
                    case FlagStoreL of
                        % non ho trovato occorrenze di Fruit gia' presenti
                        % non posso acquistare frutta non presente
                        [] -> {error, [fruit_not_present, FrCart]};
                        % verifico che le quantita' siano rispettate
                        [{_FrStore, QtStore} | _Tail] -> 
                            Condition =  (QtStore >= QtCart),
                            case Condition of
                                true -> verifyCart(CartTail, StoreList);
                                false -> {error, [not_enought_pices, FrCart]}
                            end
                    end;
                % se arrivo qua il pattern e' sbagliato
                _ -> {error, [wrong_cart_structure, Cart]}
            end;
        % se arrivo qua il pattern e' sbagliato
        _ -> {error, [wrong_cart_structure, Cart]}
    end
.

% FUNZIONA
% remove from store (buy)
removeCart(Cart, StoreList) ->
    case Cart of
        [] -> StoreList;
        [CartHead | CartTail] ->
            case CartHead of 
                {FrCart, QtCart} ->
                    FlagStoreL = lists:filter(fun({F, _}) -> FrCart == F end, StoreList),
                    case FlagStoreL of
                        % non ho trovato occorrenze di Fruit gia' presenti
                        % non posso acquistare frutta non presente
                        [] -> {error, [fruit_not_present, FrCart]};
                        [{FrStore, _QtStore} | _Tail] ->
                            % NB so gia' che QtStore >= QtCart (funz. verfiyCart()) 
                            % rimuovo frutta comprata
                            RemoveQuantity = fun(X) ->
                                case X of
                                    {FrStore, QtStore} -> {FrStore, QtStore - QtCart};
                                    % altrimenti funzione identita' (nessuna modifica)
                                    _ -> X
                                end
                            end,
                        NuovoStore = lists:map(RemoveQuantity, StoreList),
                        % chiamata ricorsiva con store aggiornato
                        fruit:removeCart(CartTail, NuovoStore)
                    end;
                % se arrivo qua il pattern e' sbagliato
                _ -> {error, [wrong_cart_structure, Cart]}
            end;
        % se arrivo qua il pattern e' sbagliato
        _ -> {error, [wrong_cart_structure, Cart]}
    end
.

% inizializza il server
init() -> spawn(fruit, shop, [populate()]).

% demo funzionamento
demo() ->
    % NOTA: la concorrenza sulla shell e' gestita tramite !ACK e receive()
    
    % salvo il PID dello shop
    PidShop = fruit:init(),
    io:format(">>> Shell Pid: ~p\n", [self()]),
    io:format(">>> Fruit Shop Pid: ~p\n", [PidShop]),
    
    % stampo il DB
    io:format(">>> Initial DB: ~p\n", 
        [PidShop!{print, self()}]
    ),
    % aspetto che shop stampi a video per gestire la concorrenza sulla 
    % console per l'I/O
    receive
        {ack, _} -> true    
    end,

    % aggiungo un item
    io:format(">>> With apples: ~p\n", 
        [PidShop!{add, apple, 12}]
    ),
    PidShop!{print, self()},
    % aspetto che shop stampi a video per gestire la concorrenza sulla 
    % console per l'I/O
    receive
        {ack, _} -> true    
    end,
    
    % compro un item
    io:format(">>> I bought 2 apples: ~p\n", 
        [PidShop!{buy, [{apple, 2}], self()}]
    ),
    PidShop!{print, self()},
    % aspetto che shop stampi a video per gestire la concorrenza sulla 
    % console per l'I/O
    receive
        {ack, _} -> true    
    end,
    
    % aspetto di ricevere il conto e lo stampo a video
    receive
        {price, ToPay} -> io:format(">>> I must pay: ~p \n", [ToPay])
    end,
    
    % termino il server shop
    PidShop!{stop}
.    