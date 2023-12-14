% esempio di publish-subscribe pattern
% es. iscrizione a canali YT -> vengo notificato per nuovi contenuti
-module(pub_sub).
-export([
    loop/2,
    init/1,
    add_topic/2,
    delete_topic/2,
    print_topics/1,
    add_event/3,
    subscribe/2,
    stop_server/1,
    create_demo_process/2
]).

% inizializzo il server
init(ServerName) ->
    % i dizionari sono chiamati mappe (maps), la mappa vuota e' #{}
    register(ServerName, spawn(pub_sub, loop, [[], #{}]))
.

% questo e' il mio server
% assumo che TopicList sia una lista di atomi
% Subs e' un dizionario: topic => lista di subscribers (lista di PID)
% Subs = #{topic1 => [pid1, pid2, ...], ...}
loop(TopicList, Subs) ->
    receive
        % es. creazione di un canale YT per l'argomento Topic
        {publish, Topic} -> 
            NewTopic = TopicList ++ [Topic],
            % inizialmente non ho subscriber per Topic -> []
            NewSubs = maps:put(Topic, [], Subs),
            pub_sub:loop(NewTopic, NewSubs);
        {delete, Topic} -> 
            % cancelliamo il topic
            NewTopic = lists:filter(fun(Top) -> Top =/= Topic end, TopicList),
            NewSubs = maps:remove(Topic, Subs),
            pub_sub:loop(NewTopic, NewSubs);
        {topic_list, Pid} -> Pid!{available_topics, TopicList}, pub_sub:loop(TopicList, Subs);
        % il processo Pid si vuole iscrivere all'argomento Topic
        {subscribe, Topic, Pid} ->
            Result = maps:find(Topic, Subs),
            case Result of
                error -> Pid!{topic_not_found, Topic}, pub_sub:loop(TopicList, Subs);
                {ok, OldSubs} -> 
                    % put(Chiave, Valore, Mappa)
                    % Subs e' un dizionario: topic => lista di subscribers (PID)
                    NewSubs = maps:put(Topic, OldSubs ++ [Pid], Subs),
                    pub_sub:loop(TopicList, NewSubs)
            end;
        % un Event può essere qualsiasi cosa (es. stringa)
        {add_event, Event, Topic, Pid} ->
            % per ogni Topic ci sono più possibili eventi
            % se c'e' un nuovo evento gli iscritti devono essere notificati
            Result = maps:find(Topic, Subs),
            case Result of
                error -> Pid!{topic_not_found, Topic}, pub_sub:loop(TopicList, Subs);
                {ok, TopicSubscribers} ->
                    Pid!{event_added, Event, Topic}, 
                    lists:foreach(fun(P) -> P!{new_event, Event, Topic} end, TopicSubscribers),
                    pub_sub:loop(TopicList, Subs)
            end;
        {stop} -> io:format("~p: server stopped!\n", [self()])
    end
.

% wrappers (API)
add_topic(ServerName, Topic) -> ServerName!{publish, Topic}, ok.

delete_topic(ServerName, Topic) -> ServerName!{delete, Topic}, ok.

print_topics(ServerName) -> 
    ServerName!{topic_list, self()}, 
    receive {available_topics, TopicList} -> io:format("La topic list e': ~p\n", [TopicList]) end,
    ok
.

subscribe(ServerName, Topic) -> ServerName!{subscribe, Topic, self()}, ok.

add_event(ServerName, Event, Topic) -> ServerName!{add_event, Event, Topic, self()}, ok.

stop_server(ServerName) -> ServerName!{stop}.

% process demo
create_demo_process(ServerName, Topic) ->
    spawn(fun() -> 
        pub_sub:subscribe(ServerName, Topic), 
        receive 
            {new_event, Event, _Topic} -> io:format("~p: ho ricevuto l'evento ~p\n", [self(), Event]); 
            {topic_not_found, Topic} -> io:format("~p: ERROR: Topic (~p) not found!\n", [self(), Topic])
        end
    end)
.

% %%%%%%%%%% DEMO
% pub_sub:init(youtube).
% pub_sub:add_topic(youtube, attack_on_titan).
% pub_sub:add_topic(youtube, one_piece).
% pub_sub:print_topics(youtube).
% pub_sub:create_demo_process(youtube, attack_on_titan).
% pub_sub:create_demo_process(youtube, attack_on_titan).
% pub_sub:create_demo_process(youtube, one_piece).
% pub_sub:create_demo_process(youtube, naruto).
% pub_sub:add_event(youtube, "Last Episode AoT", attack_on_titan).