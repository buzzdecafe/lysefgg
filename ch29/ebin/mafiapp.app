{application, mafiapp,
 [{description, "Help the boss keep track of his freinds"},
  {vsn, "1.0.0"},
  {modules, [mafiapp, mafiapp_sup]},
  {applications, [stdlib, kernel, mnesia]}
 ]
}.

