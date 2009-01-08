{application, meson_core,
  [{description, "Meson core"},
  {vsn, "1.0"},
  {modules, [meson_core_supervisor, meson_core_util, goal_server, goal_server_consumer, erlang_couchdb]},
  {registered, [goalsender]},
  {applications, [kernel, stdlib]},
  %%%  Modify this line for config options
  {mod, {meson_core, ["localhost", [] ]}}
]}.