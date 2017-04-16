%% Copyright (c) 2007-2016 Pivotal Software, Inc.
%% You may use this code for any purpose.

-module(rabbit_pgauth).

-behaviour(application).

-export([start/2, stop/1]).

start(normal, []) ->
    rabbit_pgauth_sup:start_link().

stop(_State) ->
    ok.
