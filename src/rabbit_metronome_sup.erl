-module(rabbit_metronome_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []).

init([]) ->
    {ok, {{one_for_one, 3, 10},
          [{rabbit_metronome_worker,
            {rabbit_metronome_worker, start_link, []},
            permanent,
            10000,
            worker,
            [rabbit_metronome_worker]}
          ]}}.
