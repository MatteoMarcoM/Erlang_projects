-module(seq2).
-export([
    filter/2, 
    squared/1, 
    sumFoldl/1, 
    sumFoldr/1,
    lenList/1,
    reverser/1,
    reversel/1,
    printList/1,
    for/4,
    creaN/2,
    power/1,
    hello10/0,
    foreach/2,
    map/2
]).

% list comprehension
filter(List, FilterFunc) -> [X || X <- List, FilterFunc(X)].

% funzione che ritorna il quadrato dei primi n numeri
squared(N) -> List = lists:seq(0, N), [X*X || X <- List].

% foldl e foldr (NB haskell) -> valore aggregato

% somma i valori nella lista con la foldl
sumFoldl(List) -> 
    Function = fun(Element, Accumulator) -> Element + Accumulator end,
    Accumulator0 = 0,
    lists:foldl(Function, Accumulator0, List)
.

% somma i valori nella lista con la foldr
sumFoldr(List) -> 
    Function = fun(Element, Accumulator) -> Element + Accumulator end,
    Accumulator0 = 0,
    lists:foldr(Function, Accumulator0, List)
.

% lunghezza di una lista
lenList(List) -> lists:foldl(
    fun(_, Accumulator) -> 1 + Accumulator end, 
    0, 
    List)
.

% lista inversa foldr
reverser(List) -> lists:foldr(
    fun(Elem, Acc) -> Acc ++ [Elem] end,
    [],
    List
).

% lista inversa foldl
reversel(List) -> lists:foldl(
    fun(Elem, Acc) -> [Elem] ++ Acc end,
    [],
    List
).

% foreach applica funzione a una lista
printList(List) -> lists:foreach(fun(X) -> io:format("~p \n", [X]) end, List).

% NB CONCORRENZA
% funzione che crea N attori che mi mandano un ACK
creaN(Msg, N) ->
    Iteratore = lists:seq(1, N),
    PidConsole = self(),
    FunAttore = fun(Index) -> 
                    spawn(fun() -> PidConsole!{Msg, Index, self()} end) 
                end, 
    lists:foreach(FunAttore, Iteratore)
.

% data una lista creo una lista di potenze
% applico una funzione ad ogni elem della lista
power(List) -> lists:map(fun(X) -> {X, X*X} end, List).

% funzione map
map(Fun, List) -> [Fun(X) || X <- List].

% ciclo for con foreach
for(Fun, Start, Stop, Step) -> lists:foreach(Fun, lists:seq(Start, Stop, Step)).

% list foreach function
foreach(_, []) -> ok;
foreach(Fun, List) -> [Head | Tail] = List, Fun(Head), foreach(Fun, Tail).

% scrivi hello 10 volte
hello10() -> for(fun(Index) -> io:format("Hello ~p\n", [Index]) end, 1, 10, 1).