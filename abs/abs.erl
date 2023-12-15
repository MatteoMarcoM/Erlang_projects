% abs sono i tipi di dati astratti
-module(abs).
-export([
    empty/0,
    sum/1,
    insert/2,
    find/2,
    test/0,
    test_insert/0,
    test_find/0,
    battery/1,
    concurrent_battery/1,
    demo_test/0
]).

% implementiamo alberi binari
% BT ::= {} | {BT, Value, BT}
empty() -> {}.

% sommo i valori dentro i nodi dell'albero
sum({}) -> 0;
% Nodo = {Left, Value, Right}
sum({Left, Value, Right}) -> Value + sum(Left) + sum(Right).

% inserimento in albero binario di ricerca
insert({}, NewVal) -> {{}, NewVal, {}};
insert({Left, Value, Right}, NewVal) -> 
    case NewVal >= Value of
        % restituisco l'albero originale modificato
        % ricorsivamente per inserire il nuovo valore 
        % nel sottoalbero destro o sinistro
        % NB l'inserimento avviene sempre a livello 
        % delle foglie (caso base della ricorsione)
        true -> {Left, Value, insert(Right, NewVal)};
        false -> {insert(Left, NewVal), Value, Right}
    end
.

% funzione ricerca albero binario di ricerca
find({{}, Value, {}}, Val) -> Value == Val;
find({Left, Value, Right}, Val) -> 
    case Val >= Value of
        true -> find(Right, Val);
        false -> find(Left, Val)
    end
.

% genero una batteria di test per testare il codice
% il test e' una coppia:
% {nome del test, confronto tra funzione di test e valore atteso}
test() -> [
    {test1, insert(empty(), 5) == {{}, 5, {}}},
    {test2, insert({{}, 5, {}}, 6) == {{}, 5, {{}, 6, {}}}},
    {test3, sum({{}, 5, {{}, 6, {}}}) == 11}
].

% mi creo una serie di Unit test (risultato booleano)
test_insert() -> insert(empty(), 5) == {{}, 5, {}}.

test_find() -> find({{}, 5, {{}, 7, {{}, 10, {}}}}, 10).

% batteria di test
% TestList e' la lista di nomi di funzioni di test (atomi) da eseguire 
battery(TestList) -> lists:map(fun(F) -> {F, abs:F()} end, TestList).

% batteria concorrente
concurrent_battery(TestList) ->
    Master = self(),
    Spawn = fun(F) -> spawn(fun() -> Master!{F, abs:F()} end) end,
    PidList = lists:map(Spawn, TestList),
    lists:map(fun(_) -> receive M -> M end end, PidList)
.

% avvio tutti i test in contemporanea 
% %%%%%%%%%%%% DEMO
demo_test() -> abs:concurrent_battery([test, test_insert, test_find]).