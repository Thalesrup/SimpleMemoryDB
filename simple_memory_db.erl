%% simple_memory_db.erl

-module(simple_memory_db).
-export([start/0, stop/0, set/2, get/1, del/1, rpush/2, lpop/1, publish/2, subscribe/1, unsubscribe/1]).

-define(SERVER, ?MODULE).

start() ->
    register(?SERVER, spawn(fun loop/0)).

stop() ->
    ?SERVER ! stop.

set(Key, Value) ->
    ?SERVER ! {set, Key, Value}.

get(Key) ->
    ?SERVER ! {get, self(), Key},
    receive
        {response, Value} -> Value
    end.

del(Key) ->
    ?SERVER ! {del, Key}.

rpush(Key, Value) ->
    ?SERVER ! {rpush, Key, Value}.

lpop(Key) ->
    ?SERVER ! {lpop, self(), Key},
    receive
        {response, Value} -> Value
    end.

publish(Channel, Message) ->
    ?SERVER ! {publish, Channel, Message}.

subscribe(Pid, Channel) ->
    ?SERVER ! {subscribe, Pid, Channel}.

unsubscribe(Pid) ->
    ?SERVER ! {unsubscribe, Pid}.

loop() ->
    loop(dict:new(), dict:new(), dict:new()).

loop(Db, Lists, Subs) ->
    receive
        {set, Key, Value} ->
            loop(dict:store(Key, Value, Db), Lists, Subs);
        {get, From, Key} ->
            From ! {response, dict:find(Key, Db)},
            loop(Db, Lists, Subs);
        {del, Key} ->
            loop(dict:erase(Key, Db), dict:erase(Key, Lists), Subs);
        {rpush, Key, Value} ->
            NewList = lists:append(dict:find(Key, Lists) orelse [], [Value]),
            loop(Db, dict:store(Key, NewList, Lists), Subs);
        {lpop, From, Key} ->
            case dict:find(Key, Lists) of
                undefined -> From ! {response, undefined}, loop(Db, Lists, Subs);
                [H|T] -> From ! {response, H}, loop(Db, dict:store(Key, T, Lists), Subs)
            end;
        {publish, Channel, Message} ->
            case dict:find(Channel, Subs) of
                undefined -> loop(Db, Lists, Subs);
                Subscribers -> lists:foreach(fun(Pid) -> Pid ! {message, Channel, Message} end, Subscribers),
                               loop(Db, Lists, Subs)
            end;
        {subscribe, Pid, Channel} ->
            case dict:find(Channel, Subs) of
                undefined -> loop(Db, Lists, dict:store(Channel, [Pid], Subs));
                Subscribers -> loop(Db, Lists, dict:store(Channel, [Pid | Subscribers], Subs))
            end;
        {unsubscribe, Pid} ->
            NewSubs = dict:map(fun(_, Subs) -> lists:delete(Pid, Subs) end, Subs),
            loop(Db, Lists, NewSubs);
        stop ->
            ok
    end.
