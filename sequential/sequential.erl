-module(sequential).
-export([
    fibTotal/1, 
    fib/1, 
    area/1, 
    invertiLista/1, 
    invertiLista2/1, 
    sumList/1, 
    listAdder/1,
    perimeter/1,
    fact/1])
.

% funzione totale per calcolare i numeri di fibonacci %
fibTotal(X) ->
    if
        X < 0 -> err;
        X  == 0 -> 1;
        X == 1 -> 1;
        X > 1 -> fibTotal(X-1) + fibTotal(X-2)
    end
.

% funzione definita per casi per calcolare fibonacci %
fib(X) when X < 0 -> err;
fib(X) when (X>-1) and (X<2) -> 1; % X == 0 OR X == 1 %
fib(X) -> fib(X-1) + fib(X-2).

% simile al concetto di ereditarieta' in OOP
area({square, X}) -> [Lato | _] = X, Lato * Lato;
area({rectangle, X}) -> [Base | Coda] = X, [Altezza | _] = Coda, Base * Altezza;
area({circle, X}) -> [Raggio | _] = X, Raggio * Raggio * 3.14;
area({triangle, X}) -> [Base | Coda] = X, [Altezza | _] = Coda, 0.5 * Base * Altezza;
area({_, _}) -> not_implemented. % atomo


invertiLista([Head | Tail]) -> 
    invertiLista(Tail) ++ [Head];
    invertiLista([]) -> []
.

% altra versione stesso risultato
invertiLista2(X) -> 
    case X of
        [Head | Tail] -> invertiLista(Tail) ++ [Head];
        [] -> []
    end
.

% sommare elementi di una lista ricorsivamente
sumList([]) -> 0;
sumList([A | B]) -> A + sumList(B).

% altra versione stesso risultato
listAdder(X) ->
  case X of
    [H | T] -> H + listAdder(T);
    [] -> 0
  end
.

% funzione calcolo perimetro e circonferenza
perimeter({square, X}) -> [Lato | _] = X, 4 * Lato;
perimeter({rectangle, X}) -> [Base | Coda] = X, [Altezza | _] = Coda, 2 * (Base + Altezza);
perimeter({circle, X}) -> [Raggio | _] = X, 2 * Raggio * 3.14;
perimeter({triangle, X}) -> [L1 | Coda1] = X, [L2 | Coda2] = Coda1, [L3 | _] = Coda2, L1 + L2 + L3;
perimeter({poligon, X}) -> case X of
                                [] -> 0;
                                [Lato | Coda] -> Lato + perimeter({poligon, Coda})
                            end;
perimeter({_, _}) -> not_implemented. % atomo

% calcolo del fattoriale
fact(X) when X < 0 -> not_implemented;
fact(0) -> 1;
fact(N) -> N * fact(N-1).