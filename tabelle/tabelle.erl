-module(tabelle).
% da includere per usare ets:fun2ms
-include_lib("stdlib/include/ms_transform.hrl").
-export([
    init/0,
    demo_tabella/0,
    stampa_tabella/2,
    demo_ordered_table/0,
    register_table/1,
    shared/1,
    populate/0,
    crea_agenda/1,
    filtra_nazionalita/2,
    filtra_professione/2,
    escludi_nazionalita/2,
    select_nation_prof/3,
    select_all_prof/1,
    delete_all_prof/1
]).

init() ->
    % creo una tabella con valori di default ovvero un
    % set di tuple con primo elemento come chiave primaria
    % la visibilita' è protected di default
    % i miei figli possono leggerla ma non scriverci
    % tabella con valori di default -> ets:new(name, [])
    MiaTab = ets:new(miaTab, [protected]),
    % inserisco un elemento nella tabella
    % ETS: Erlang Term Storage
    ets:insert(MiaTab, {1, pippo, disney}),
    % simile a un DB NoSQL !!!
    % come fosse una lista di tuple anche di diversa dimensione
    % NB il primo campo e' la chiave primaria
    ets:insert(MiaTab, {2, pippo, disney, "altri campi"}),
    % tabella dinamica
    ets:insert(MiaTab, {3, [tabella_dinamica]}),

    % creo un Bag: come una tabella ma con chiavi duplicate
    MiaBag = ets:new(miaBag, [bag]),
    ets:insert(MiaBag, {1, pippo}),
    ets:insert(MiaBag, {1, pluto}),

    % creo un processo che può leggere la mia tabella locale
    Shell = self(),
    spawn(
        fun() ->
            % cerco il valore con chiave 1
            Shell!{ets:lookup(MiaTab, 1)},
            Shell!{ets:lookup(MiaTab, 2)},
            Shell!{ets:lookup(MiaTab, 3)},
            Shell!{ets:lookup(MiaBag, 1)}
        end
    )
    % faccio flush() da shell per vedere i valori ottenuti
.

% stampare elementi in una tablella
demo_tabella() ->
    io:format("NOTA: la tabella non e' ordinata di default!\n"),
    MiaTab = ets:new(miaTab, []),
    % creo la tabella
    lists:foreach(
        fun(Index) ->
            ets:insert(
                MiaTab,
                {Index, "valore " ++ integer_to_list(Index)}
            ) 
        end,
        lists:seq(1, 10)
    ),
    % stampo la tabella
    % devo iniziare dal primo elemento
    % lo gestisco come un iteratore (Future)
    First = ets:first(MiaTab),
    io:format("Primo elemento: ~p\n", 
        [ets:lookup(MiaTab, First)]
    ),
    stampa_tabella(MiaTab, First) 
.

% stampa tabella
% dopo il primo elemento posso fare la ricorsione
% NB la tabella e' un set (non e' ordinata)
stampa_tabella(Tabella, Next) ->
    case Next of
        % '$end_of_table' e' un atomo (come EOF)
        '$end_of_table' -> ok;
        _ -> 
            % ottengo il prossimo elemento (iteratore)
            % come se fosse un Future
            NextElemKey = ets:next(Tabella, Next),
            case NextElemKey /= '$end_of_table' of
                true -> 
                    io:format("i-esimo elemento: ~p\n", 
                        [ets:lookup(Tabella, NextElemKey)]
                    );
                false -> 
                    io:format("Raggiunto fine tabella!\n")
            end,
            % ricorsione resto tabella
            tabelle:stampa_tabella(Tabella, NextElemKey)
    end
.

% NB tabella ordinata
% MOLTO COSTOSA (DEVE ORDINARE AD OGNI INSERT)
demo_ordered_table() ->
    io:format("NOTA: la tabella e' ordinata!\n"),
    % NB parametro per tabella ordinata: ordered_set
    MiaTab = ets:new(miaTab, [ordered_set]),
    
    % creo la tabella
    lists:foreach(
        fun(Index) ->
            ets:insert(
                MiaTab,
                {Index, "valore " ++ integer_to_list(Index)}
            ) 
        end,
        lists:seq(1, 10)
    ),
    % stampo la tabella
    % devo iniziare dal primo elemento
    % lo gestisco come un iteratore (Future)
    First = ets:first(MiaTab),
    io:format("Primo elemento: ~p\n", 
        [ets:lookup(MiaTab, First)]
    ),
    tabelle:stampa_tabella(MiaTab, First) 
.

% tabella con nome (registrata)
register_table(Name) ->
    
    % registro la tabella col nome Name
    ets:new(Name, [named_table]),
    
    % creo la tabella
    lists:foreach(
        fun(Index) ->
            ets:insert(
                Name,
                {Index, "valore " ++ integer_to_list(Index)}
            ) 
        end,
        lists:seq(1, 10)
    ),
    % stampo la tabella
    % devo iniziare dal primo elemento
    % lo gestisco come un iteratore (Future)
    First = ets:first(Name),
    io:format("Primo elemento: ~p\n", 
        [ets:lookup(Name, First)]
    ),
    tabelle:stampa_tabella(Name, First)

    % NB la tabella muore col processo che l'ha creata
    % se la shell crasha muoiono le tabelle che ha creato
.


shared(Nome) ->
    % tutti possono accedere alla tabella
    % NB race condition !
    % register nome tabella
    ets:new(Nome, [set, named_table, public]),
    % creo la tabella
    lists:foreach(
        fun(Index) ->
            ets:insert(
                Nome,
                {Index, "valore " ++ integer_to_list(Index)}
            ) 
        end,
        lists:seq(1, 10)
    ),
    % un processo figlio puo' accedere alla tabella
    % col nome registrato
    MioPid = self(),
    spawn(
        fun() ->
            ets:insert(
                Nome,
                {pluto, "valore pluto"}
            ),
            MioPid!{ok}
        end
    ),
    % attendo che abbia finito di scrivere
    receive
        {ok} -> true
    end,
    % stampo la tabella
    First = ets:first(Nome),
    io:format("Primo elemento: ~p\n", 
        [ets:lookup(Nome, First)]
    ),
    tabelle:stampa_tabella(Nome, First)
.

% fare select sulle tabelle
populate() -> [
    % {nazionalita, nome, professione}
    {italia, claudio, prof},
    {italia, marco, prof},
    {italia, matteo, studente},
    {portogallo, vasco, prof},
    {germania, claudio, prof},
    {italia, luca, prof},
    {italia, matteo, collaboratore},
    {portogallo, gigio, prof}
].

crea_agenda(Nome) ->
    % la chiave (nazionalita) può essere ripetuto
    ets:new(Nome, [bag, named_table]),
    lists:foreach(
        fun(Elem) ->
            ets:insert(Nome, Elem)
        end,
        populate()
    )
.

% select per nazionalita'
filtra_nazionalita(Nazionalita, Tabella) ->
    ets:match(
        Tabella,
        % ultimi due campi qualsiasi
        {Nazionalita, '$2', '$3'}
    )
.

filtra_professione(Professione, Tabella) ->
    ets:match(
        Tabella,
        % ultimi due campi qualsiasi
        {'$1', '$2', Professione}
    )
.

% simile SELECT SQL
escludi_nazionalita(Nazionalita, Tabella) ->
    % SELECT nome, nazionalita 
    % FROM Tabella
    % WHERE Tabella.nazionalita /= Nazionalita
    ets:select(Tabella, [
            % NB pattern
            {
                % formato della tabella (riga)
                % {nazionalita, nome, professione}
                {'$1', '$2', '$3'},
                % escludo la nazionalita' (PATTERN)
                % le clausole vanno in AND
                [{'/=', '$1', Nazionalita}],
                % scelgo i campi da ritornare
                % {nome, nazionalita}
                [['$2', '$1']]
            }
        ]
    )
.

% ets function to mathcing specification
select_nation_prof(Nation, Prof, Tabella) ->
    MS = ets:fun2ms(
        fun({N, Name, P})
            when (Nation == N), (Prof == P) ->
              [Name, Prof]
        end
    ),
    ets:select(Tabella, MS)
.

% SELECT *
select_all_prof(Tabella) ->
    ets:match_object(
        Tabella,
        {'$1','$2', prof}
    )
.

delete_all_prof(Tabella) ->
    ets:match_delete(
        Tabella,
        {'$1','$2', prof}
    )
.