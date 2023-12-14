% intestazione del file per creare un record
% simili alle tuple ma con nomi dei campi e della tupla

% per usare i record da shell bisogna precaricarli col comando
% rr("file_definizione_record.hrl").

% un record e' come una riga di un DB
-record(person, {
    name, 
    surname, 
    age,
    street
}).

% person1 e' il tipo di dato (nome del record)
% #person1{} e' il costruttore del record
-record(person1, {
    name, 
    surname,
    % default value 
    age = 18,
    street = no_street
}).

% per ottenere la lista dei campi di un record basta usare
% record_info(fields, record_name).
-record(corso, {
    nome, 
    studente = #person{} 
}).

% record di prova
-record(prova, {
    primo,
    secondo 
}).