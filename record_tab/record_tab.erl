% using records and tables
-module(record_tab).

-include_lib("stdlib/include/ms_transform.hrl").

% miei records
-record(person, {name, surname, age, street=no_street}).

-export([
    init/1,
    add_person/4,
    populate/2,
    find_person/2,
    find_age/2,
    dets_demo/0
]).

% per popolare un record di tipo person
add_person(TableName, Name, Surname, Age) ->
    ets:insert(TableName, 
        #person{
            name = Name, 
            surname = Surname, 
            age = Age
        }
    )
.

% popolo la tabella
populate(N, TableName) ->
    lists:foreach(
        fun(Index) ->
            add_person(
                TableName,
                "utente_" ++ integer_to_list(Index), 
                "cognome_" ++ integer_to_list(Index),
                Index + 10
            )
        end,
        lists:seq(1, N)
    )
.

% creo una tabella composta da persone
init(TableName) ->
    ets:new(
        TableName, 
        [
            named_table,
            % la chiave primaria e' il nome della persona
            % nota la struttura dei record in memoria
            % {person, person.name, person.surname, person.age, person.street}
            % person.name e' pari a 2 
            {keypos, #person.name}
        ]
    ),
    % popolo la tabella
    record_tab:populate(20, TableName)
.

% non uso piu' i '$1'
find_person(TableName, Name) ->
    ets:match_object(TableName,
        % mi interessa solo il nome 
        #person{name = Name, _ = '_'}
    )
.

find_age(TableName, Age) ->
    MS = ets:fun2ms(
            fun(#person{
                    name = N, 
                    age = A
                }) when A >= Age -> {N, A}
            end     
        )
    ,
    ets:select(TableName, MS)
.

% DETS: Disk ETS, come le ETS ma su file (piu' lente)
% con salvataggio automatico su disco
dets_demo() ->
    % NB dets salvate su disco !
    % provo a caricare il file da disco se esiste
    {Condition, X} = dets:open_file("./food"),
    % Tab e' la referenza della dets
    Tab = case Condition of
        % e' andata a buon fine (l'ho caricata)
        % X e' la mia tabella (Tab = X)
        ok -> X;
        % non ho trovato il file
        error ->
            % X e' il messaggio di errore
            % creo dets nuova di nome T
            {ok, T} = dets:open_file(
                % NB non esistono le named dets
                food, 
                [
                    % tipologia di dets
                    {type, bag},
                    % specifico il path 
                    {file, "./food"}
                ]
            ), 
            % popolo la nuova dets
            dets:insert(T, {italy, spaghetti}),
            dets:insert(T, {italy, pizza}),
            dets:insert(T, {sweden, meatballs}),
            dets:insert(T, {japan, ramen}),
            % T e' il riferimento alla tabella
            % Tab = T
            T             
    end,
    % queries su dets
    % NB ETS:fun2ms
    NotItalianMS = ets:fun2ms(
        fun({Nation, Food}) 
            when Nation /= italy -> Food       
        end
    ),
    NotItalianFood = dets:select(Tab, NotItalianMS),
    io:format("Not italian foods: ~p\n", [NotItalianFood]),
    % alla fine la chiudo !
    dets:close(Tab)
.