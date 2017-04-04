{application, exercise1,
 [{description, "Message of the day app"},
  {vsn, "0.1.0"},
  {registered, []},
  {mod, { exercise1_app, []}},
  {applications,
   [kernel,
    stdlib
   ]},
  {env,[]},
  {modules, [exercise1_app, dlq]},

  {maintainers, []},
  {licenses, []},
  {links, []}
 ]}.
