% Riferimenti:
% https://riptutorial.com/erlang/example/24705/using-gen-server-behavior
% http://20bits.com/article/erlang-a-generic-server-tutorial

% creo un server tramite il behaviur di erlang chiamato gen_server
% voglio implementare un server che mi mantiene il valore di un contatore intero
-module(simple_server).
% un behaviour e' simile a un modulo erlang, e' un componente di OTP
% un behaviour e' un pattern di comunicazione per sistemi distribuiti
% (simile ai design pattern per l'OOP)
% MOLTO COMODO
-behaviour(gen_server).

-export([
  start_link/0,
  init/1,
  handle_call/3, 
  handle_cast/2,
  handle_info/2,
  terminate/2, 
  stop/0
]).
% wrapping gen_server functions for client side application
-export([
  add_sync/0, 
  add_async/0,
  add_sync_n/1,
  add_async_n/1, 
  dec_sync/0,
  dec_async/0,
  dec_sync_n/1,
  dec_async_n/1
]).

% wrapper per far partire il server
start_link() ->
  % gen_server:start_link/4 prende 4 parametri
  % 1) -> tupla {local, server_name} o {global, server_name}
  % NB per convenzione server_name == ?MODULE
  % 2) -> il modulo su cui verra' chiamata la init() per inizializzare il server
  % 3) -> parametri di inizializzaione (passati alla init) 
  % e' il nostro loopData per mantenere lo stato interno al server
  % 4) -> lista di opzioni da passare per settare dimensione heap, debug mode
  % spesso questo parametro e' lasciato come lista vuota []
  Return = gen_server:start_link(
    % il nome del server locale e' == ?MODULE (simple_server)
    {local, ?MODULE},
    ?MODULE,
    % il contatore parte da 0
    0,
    []
  ),
  {ok, Pid} = Return,
  io:format("~p (shell): Faccio partire il server con pid ~p\n",[self(), Pid]),
  Return
.

% la init() viene chiamata dalla start_link per inizializzare il server
% io devo implementare SOLO LA INIT (simile a interfaccie OOP)
% NB funzioni di callback (es. JavaScript)
% DEVE ritornare una coppia del tipo
% {ok, State}
% {ok, State, Timeout}
% {ok, State, hibernate}
% {stop, Reason}
% ignore
init(LoopData) ->
  io:format("~p: Chiamata init con contatore ~p\n",[self(), LoopData]),
  % non ho bisogno di inizializzare nulla quindi ritorno LoopData
  {ok, LoopData}
.

% handle_call() viene chiamata dal server quando un processo fa una richiesta 
% di tipo call al server tramite gen_server:call() 
% handle_call ritorna un messaggio di reply (mi interessa il risultato!)
% {reply, Risultato, NuovoStato}
% CALL e' una chiamata SINCRONA (ASPETTO IL RISULTATO BLOCCANDOMI)
handle_call(add, _From, State) ->
  NewState = State + 1,
  {Pid, _Ref} = _From,
  io:format("~p: Chiamata handle call add con stato ~p da ~p \n",[self(), NewState, Pid]),
  {reply, {ok, NewState}, NewState};

handle_call(dec, _From, State) ->
  NewState = State - 1,
  {Pid, _Ref} = _From,
  io:format("~p: Chiamata handle call dec con stato ~p da ~p \n",[self(), NewState, Pid]),  {reply, {ok, NewState}, NewState};

handle_call({add, N}, _From, State) ->
  NewState = State + N,
  {Pid, _Ref} = _From,
  io:format("~p: Chiamata handle call {add, N} con stato ~p da ~p \n",[self(), NewState, Pid]),
  {reply, {ok, NewState}, NewState};

handle_call({dec, N}, _From, State) ->
  NewState = State - N,
  {Pid, _Ref} = _From,
  io:format("~p: Chiamata handle call {dec, N} con stato ~p da ~p \n",[self(), NewState, Pid]),
  {reply, {ok, NewState}, NewState};

handle_call(stop, _From, State) ->
  {stop, normal, State};

handle_call(_Message, _From, State) ->
  io:format("~p: Ignoro handle_call\n", [self()]),
  {reply, {ok, State}, State}
.

% handle_cast() viene chiamata dal server quando un processo fa una richiesta 
% di tipo cast al server tramite gen_server:cast() 
% CAST e' una chiamata ASINCRONA (NON ASPETTO IL RISULTATO E VADO AVANTI)
% handle_cast ritorna un messaggio di noreply (NON mi interessa il risultato!)
% {noreply, NuovoStato}
handle_cast(add, State) ->
  NewState = State + 1,
  Return = {noreply, NewState},
  io:format("~p: Chiamata handle cast add con stato ~p\n", [self(), NewState]),
  Return;

handle_cast(dec, State) ->
  NewState = State - 1,
  Return = {noreply, NewState},
  io:format("~p: Chiamata handle cast dec con stato ~p\n", [self(), NewState]),
  Return;

handle_cast({add, N}, State) ->
  NewState = State + N,
  Return = {noreply, NewState},
  io:format("~p: Chiamata handle cast {add, N} con stato ~p\n", [self(), NewState]),
  Return;

handle_cast({dec, N}, State) ->
  NewState = State - N,
  Return = {noreply, NewState},
  io:format("~p: Chiamata handle cast {dec, N} con stato ~p\n", [self(), NewState]),
  Return;

handle_cast(stop, State) ->
  % termino in modo normale quando ricevo il messaggio stop
  {stop, normal, State};

handle_cast(_Message, State) ->
  io:format("~p: Ignoro handle cast:\n", [self()]),
  {noreply, State}
.

% handle_info() gestisce i messaggi NON standard (OTP) provenienti dall'esterno
handle_info(_Info, State) ->
  io:format("~p: Received message ~p .....\n",[self(), _Info]),
  {noreply, State}
.

% viene chiamata se il server termina
terminate(Reason, State) -> 
  io:format("SERVER STOPPED : ~p, last state was: ~p\n", [Reason, State]),
  ok
.

% wrapping gen_server functions for client side application
add_sync() -> gen_server:call(?MODULE, add).

add_async() -> gen_server:cast(?MODULE, add).

add_sync_n(N) -> gen_server:call(?MODULE, {add, N}).

add_async_n(N) -> gen_server:cast(?MODULE, {add, N}).

dec_sync() -> gen_server:call(?MODULE, dec).

dec_async() -> gen_server:cast(?MODULE, dec).

dec_sync_n(N) -> gen_server:call(?MODULE, {dec, N}).

dec_async_n(N) -> gen_server:cast(?MODULE, {dec, N}).

% asincrona
stop() -> gen_server:cast(?MODULE, stop).

% %%%%%%%%%% DEMO
% simple_server:start_link().
% simple_server:add_sync().
% simple_server:add_async().
% simple_server:add_sync_n(10).
% simple_server:dec_sync().
% simple_server:stop().