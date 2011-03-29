-module(rabbit_metronome_worker).
-behaviour(gen_server).

-export([start/0, start/2, stop/0, stop/1, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {channel}).
-define(RKFormat, "~4.10.0B.~2.10.0B.~2.10.0B.~1.10.0B.~2.10.0B.~2.10.0B.~2.10.0B").

start() ->
  start_link(),
  ok.

start(normal, []) ->
  start_link().

stop() ->
  ok.

stop(_State) ->
  stop().

start_link() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%---------------------------
% Gen Server Implementation
% --------------------------

init([]) ->
  {ok, Connection} = amqp_connection:start(direct),
  {ok, Channel} = amqp_connection:open_channel(Connection),
  amqp_channel:call(Channel, #'exchange.declare'{exchange = <<"metronome">>,
                                                 type = <<"topic">>}),

  timer:apply_after(1000, gen_server, call, [{global, ?MODULE}, fire]),
  {ok, #state{channel = Channel}}.

handle_call(Msg,_From,State = #state{channel = Channel}) ->
  case Msg of
    fire ->
      Properties = #'P_basic'{content_type = <<"text/plain">>, delivery_mode = 1},
      {Date={Year,Month,Day},{Hour, Min,Sec}} = erlang:universaltime(),
      DayOfWeek = calendar:day_of_the_week(Date),
      RoutingKey = list_to_binary(io_lib:format(?RKFormat, [Year, Month, Day,
                                                            DayOfWeek, Hour, Min, Sec])),
      Message = RoutingKey,
      BasicPublish = #'basic.publish'{exchange = <<"metronome">>,
                                      routing_key = RoutingKey},
      Content = #amqp_msg{props = Properties, payload = Message},
      amqp_channel:call(Channel, BasicPublish, Content),

      timer:apply_after(1000, gen_server, call, [{global, ?MODULE}, fire]),
      {reply, ok, State};
    _ ->
      {reply, unknown_command, State}
  end.

handle_cast(_,State) ->
    {noreply,State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_,#state{channel = Channel}) ->
    amqp_channel:call(Channel, #'channel.close'{}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
