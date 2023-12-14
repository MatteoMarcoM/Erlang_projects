-module(prova_record).

% importo il file che definisce il record
-include("record.hrl").

-export([
    create_person/4,
    crea_corso/5,
    crea_corso2/5,
    populate/1,
    createDB/0,
    select/2,
    print_prova/1,
    print_corso/1
]).

% crea un record per una persona
create_person(Name, Surname, Age, Street) ->
    #person{
        name = Name,
        surname = Surname,
        age = Age,
        street = Street
    }
.

% record annidati
crea_corso(NomeCorso, Name, Surname, Age, Street) ->
    #corso{
        nome = NomeCorso,
        studente = #person{
            name = Name,
            surname = Surname,
            age = Age,
            street = Street
        }
    }
.

crea_corso2(NomeCorso, Name, Surname, Age, Street) ->
    #corso{
        nome = NomeCorso,
        studente = create_person(Name, Surname, Age, Street)
    }
.

% funzione per popolare il DB
% X e' il contatore da 1 a 10
% restituisce una lista di record
populate(Corso) -> 
    lists:map(
        % NB nella map() non posso chiamare direttamente crea_corso()
        % perche devo usare una lambda e passare X
        fun(X) ->
            crea_corso(
                Corso,
                X,
                X, 
                X+18,
                no_street
            )
        end,
        lists:seq(1, 10)
    )
.

% creare il DB
% restituisce una lista di record
createDB() ->
    populate(adcc) ++ populate(iot)
.

% funzione che seleziona tutti gli studenti di un corso con certa eta'
select(NomeCorso, Eta) -> 
    lists:filter(
        fun(X) -> 
            case X of
                #corso{
                    nome = NomeCorso,
                    studente = Record     
                } -> case Record of
                        #person{
                            name = _,
                            surname = _,
                            age = Age,
                            street = _
                        } -> Age >= Eta
                        % altrimenti e' false automatico
                    end;
                % se il corso non e' quello non lo prendo
                _ -> false 
            end
        end,
        createDB()
    )
.

% prova di stampa di un record
print_prova(
    #prova{
        primo = P,
        secondo = S
    }
) -> io:format("Primo Campo: ~p\nSecondo Campo: ~p\n", [P, S]).

% il record più interno deve essere passato come una tupla 
% di n+1 elementi il cui primo elemento è l'atomo che da il tipo al record
print_corso(
    #corso{
        nome = _Course, 
        studente = {person, _Name, _Surname, _Age, _Street} 
    }
) -> io:format("{~p, ~p, ~p, ~p, ~p} \n",[_Course, _Name, _Surname, _Age, _Street]).