-module(simple_memory_serve).
-export([start/0, stop/0]).

-define(PORT, 6579).

start() ->
    case gen_tcp:listen(?PORT, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]) of
        {ok, ListenSocket} ->
            spawn(fun() -> accept(ListenSocket) end),
            io:format("Server started on port ~p~n", [?PORT]),
            {ok, ListenSocket};
        {error, Reason} ->
            io:format("Error starting server: ~p~n", [Reason]),
            {error, Reason}
    end.

stop() ->
    %% Adicionar lógica para parar servidor
    ok.

accept(ListenSocket) ->
    io:format("Waiting for a connection...~n", []),
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            io:format("Accepted connection~n", []),
            spawn(fun() -> accept(ListenSocket) end),
            loop(Socket);
        {error, closed} ->
            io:format("Listen socket closed~n", []);
        {error, Reason} ->
            io:format("Error accepting connection: ~p~n", [Reason])
    end.

loop(Socket) ->
    receive_data(Socket).

receive_data(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, BinData} ->
            io:format("Received data: ~p~n", [BinData]),
            case decode_term(BinData) of
                {ok, Data} ->
                    io:format("Decoded data: ~p~n", [Data]),
                    Response = handle_request(Data),
                    gen_tcp:send(Socket, term_to_binary(Response)),
                    receive_data(Socket);
                {error, Reason} ->
                    io:format("Error decoding data: ~p~n", [Reason]),
                    gen_tcp:send(Socket, term_to_binary({error, Reason})),
                    receive_data(Socket)
            end;
        {error, closed} ->
            io:format("Socket closed~n", [])
    end.

decode_term(BinData) ->
    try
        {ok, binary_to_term(BinData)}
    catch
        _:_ -> {error, invalid_format}
    end.

handle_request({set, Key, Value}) ->
    simple_memory_db:set(Key, Value),
    {ok, {set, Key, Value}};
handle_request({get, Key}) ->
    simple_memory_db:get(Key);
handle_request({del, Key}) ->
    simple_memory_db:del(Key),
    {ok, {del, Key}};
handle_request({rpush, Key, Value}) ->
    simple_memory_db:rpush(Key, Value),
    {ok, {rpush, Key, Value}};
handle_request({lpop, Key}) ->
    {ok, simple_memory_db:lpop(Key)};
handle_request({publish, Channel, Message}) ->
    simple_memory_db:publish(Channel, Message),
    {ok, {publish, Channel, Message}};
handle_request({subscribe, Pid, Channel}) ->
    simple_memory_db:subscribe(Pid, Channel),
    {ok, {subscribe, Pid, Channel}};
handle_request({unsubscribe, Pid}) ->
    simple_memory_db:unsubscribe(Pid),
    {ok, {unsubscribe, Pid}};
handle_request(_) ->
    {error, unknown_request}.
