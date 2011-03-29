-module(rabbit_metronome).

-behaviour(application).

-export([start/2, stop/1]).

start(normal, []) ->
    rabbit_metronome_sup:start_link().

stop(_State) ->
    ok.
