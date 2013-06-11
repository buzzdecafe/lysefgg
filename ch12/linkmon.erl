-module(linkmon).
-compile(export_all).

myproc() ->
  timer:sleep(5000),
  exit(reason).

chain(0) -> 
  receive
    _ -> ok
  after 2000 ->
    exit("chain dies here")
  end;
chain(N) ->
  Pid = spawn(fun() -> chain(N-1) end),
  link(Pid),
  receive
    _ -> ok
  end.


start_critic() ->
  spawn(?MODULE, critic, []).

judge(Pid, Band, Album) -> 
  Pid ! {self(), {Band, Album}},
  receive
    {Pid, Criticism} -> Criticism
  after 2000 -> timeout
  end.

critic() ->
  receive
    {From, {"Sex Pistols", "Never Mind the Bollocks"}} ->
      From ! {self(), "Great"};
    {From, {"Reducers", "Let's Go"}} ->
      From ! {self(), "Good rockin' band"};
    {From, {"Elliot Smith", "XO"}} ->
      From ! {self(), "Very good indeed"};
    {From, {_Band, _Album}} ->
      From ! {self(), "Sucks"}
  end,
  critic().

start_critic2() ->
  spawn(?MODULE, restarter, []).

restarter() -> 
  process_flag(trap_exit, true),
  Pid = spawn_link(?MODULE, critic2, []),
  register(critic, Pid),
  receive
    {'EXIT', Pid, normal} -> % not a crash
      ok;
    {'EXIT', Pid, shutdown} -> % deliberately terminated, not a crash
      ok;
    {'EXIT', Pid, _} -> % all other exits
      restarter()
  end.

judge2(Band, Album) -> 
  Ref = make_ref(),
  critic ! {self(), Ref, {Band, Album}},
  receive
    {Ref, Criticism} -> Criticism
  after 2000 -> timeout
  end.

critic2() ->
  receive
    {From, Ref, {"Sex Pistols", "Never Mind the Bollocks"}} ->
      From ! {Ref, "Great"};
    {From, Ref, {"Reducers", "Let's Go"}} ->
      From ! {Ref, "Good rockin' band"};
    {From, Ref, {"Elliot Smith", "XO"}} ->
      From ! {Ref, "Very good indeed"};
    {From, Ref, {_Band, _Album}} ->
      From ! {Ref, "Sucks"}
  end,
  critic2().



