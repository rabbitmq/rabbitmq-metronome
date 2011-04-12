{application, rabbit_metronome,
 [{description, "Embedded Rabbit Metronome"},
  {vsn, "0.01"},
  {modules, [
    rabbit_metronome,
    rabbit_metronome_sup,
    rabbit_metronome_worker
  ]},
  {registered, []},
  {mod, {rabbit_metronome, []}},
  {env, []},
  {applications, [kernel, stdlib, rabbit, amqp_client]}]}.
