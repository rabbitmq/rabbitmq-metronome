%% Copyright (c) 2007-2013 VMware, Inc.
%% You may use this code for any purpose.

-module(rabbit_metronome_worker).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([fire/0]).

-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {channel}).

-define(RKFormat,
        "~4.10.0B.~2.10.0B.~2.10.0B.~1.10.0B.~2.10.0B.~2.10.0B.~2.10.0B").

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%---------------------------
% Gen Server Implementation
% --------------------------

init([]) ->
    {ok, Connection} = amqp_connection:start(#amqp_params_direct{}),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    amqp_channel:call(Channel, #'exchange.declare'{exchange = <<"metronome">>,
                                                   type = <<"topic">>}),
    fire(),
    {ok, #state{channel = Channel}}.

handle_call(_Msg, _From, State) ->
    {reply, unknown_command, State}.

handle_cast(fire, State = #state{channel = Channel}) ->
    Properties = #'P_basic'{content_type = <<"text/plain">>, delivery_mode = 1},
    {Date={Year,Month,Day},{Hour, Min,Sec}} = erlang:universaltime(),
    DayOfWeek = calendar:day_of_the_week(Date),
    RoutingKey = list_to_binary(
                   io_lib:format(?RKFormat, [Year, Month, Day,
                                             DayOfWeek, Hour, Min, Sec])),
    Message = RoutingKey,
    BasicPublish = #'basic.publish'{exchange = <<"metronome">>,
                                    routing_key = RoutingKey},
    Content = #amqp_msg{props = Properties, payload = Message},
    amqp_channel:call(Channel, BasicPublish, Content),
    timer:apply_after(1000, ?MODULE, fire, []),
    {noreply, State};

handle_cast(_, State) ->
    {noreply,State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_, #state{channel = Channel}) ->
    amqp_channel:call(Channel, #'channel.close'{}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%---------------------------

fire() ->
    gen_server:cast({global, ?MODULE}, fire).
