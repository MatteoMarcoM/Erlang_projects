-module(seq).
-import(sub_module, [add/2]).
-export([
    somma/2, 
    filtraPari/1, 
    filtraPari2/1, 
    anagrafica/0, 
    filtraSufficiente/0, 
    filtraSuff/0, 
    filtraVotiAlti/2])
.

% utilizzo sottomoduli
somma(X, Y) -> sub_module:add(X,Y).

% funzione che data una lista di interi da una lista di pari
filtraPari(X) ->
    case X of
        [H | T] -> case H rem 2 of
                        0 -> [H] ++ filtraPari(T);
                        1 -> filtraPari(T)
                    end;
        [] -> []
    end
.

% con libreria lists
filtraPari2(X) -> lists:filter(fun(Y) -> (Y rem 2) =:= 0 end, X).

anagrafica() -> [
    {pippo, 30},
    {pluto, 34},
    {paperino, 10}
].

% filtro voti esami >= 18
filtraSufficiente() -> lists:filter(fun({_, Voto}) -> Voto >= 18 end, anagrafica()).

% C sequential style
filtraSuff() -> Anagrafica = [
    {pippo, 30},
    {pluto, 34},
    {paperino, 10}
    ],
    Filtro = fun({_, Voto}) -> Voto >= 18 end,
    lists:filter(Filtro, Anagrafica)
.

filtraVotiAlti(Voto, Materia) -> 
    Anagrafica = [
        {pippo, 30, logica},
        {pluto, 34, programmazione},
        {paperino, 10, 'IoT'}
    ],
    Filtro = fun({_, Esito, M}) -> (Esito > Voto) and (Materia == M) end,
    Lista = lists:filter(Filtro, Anagrafica),
    io:format("Stringa formattazione Tilde = (ALT+126) \n Materia: ~p \n Lista: ~p", [Materia, Lista])
.
