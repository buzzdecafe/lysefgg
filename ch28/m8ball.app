{application, m8ball,
 [{vsn, "1.0.0"},
  {description, "Answer vital questions"},
  {modules, [m8ball, m8ball_sup, m8ball_server]},
  {applications, [stdlib, kernel, crypto]},
  {registered, [m8ball, m8ball_sup, m8ball_server]},
  {mod, {m8ball, []}},
  {env, [
    {answers, {
      <<"It is certain">>,
      <<"It is decidedly so">>,
      <<"Without a doubt">>,
      <<"Yes definitely">>,
      <<"You may rely on it">>,
      <<"As I see it, yes">>,
      <<"Most likely">>,
      <<"Outlook good">>,
      <<"Yes">>,
      <<"Signs point to yes">>,
      <<"Reply hazy try again">>,
      <<"Ask again later">>,
      <<"Better not tell you now">>,
      <<"Cannot predict now">>,
      <<"Concentrate and ask again">>,
      <<"Don't count on it">>,
      <<"My reply is no">>,
      <<"My sources say no">>,
      <<"Outlook not so good">>,
      <<"Very doubtful">>
    }}
   ]
  }
 ]}.
    
