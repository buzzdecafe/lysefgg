-module(evserv).
-compile(export_all).
%-export([]).

-record(state, {events, clients}).
-record(event, {name="", description="", pid, timeout={{1970,1,1},{0,0,0}}}).

init() ->
  % load events from file?
  % then pass file uri as argument.
  % alternately, pass events into this function
  loop(#state{events=orddict:new(), clients=orddict:new()}).

valid_datetime({Date, Time}) ->
  try
    calendar:valid_date(Date) andalso valid_time(Time)
  catch
    error:function_clause -> false
  end;
valid_datetime(_) -> false.

valid_time({H,M,S}) -> valid_time(H,M,S).
valid_time(H,M,S) when H >= 0, H < 24, M >= 0, M < 60, S >= 0, S < 60 -> true;
valid_time(_,_,_) -> false.

send_to_clients(Msg, ClientDict) ->
  orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).

loop(S = #state{}) ->
  receive
    
    % keep a list of subscribers
    {Pid, MsgRef, {subscribe, Client}} ->
      Ref = erlang:monitor(process, Client),
      NewClients = orddict:store(Ref, Client, S#state.clients),
      Pid ! {MsgRef, ok},
      loop(S#state{clients=NewClients});
    
    % add an event to the list
    {Pid, MsgRef, {add, Name, Description, TimeOut}} ->
      case valid_datetime(TimeOut) of
        true ->
          EventPid = event:start_link(Name, TimeOut),
          NewEvents = orddict:store(Name, 
                                    #event{name=Name, 
                                           description=Description,
                                           pid=EventPid,
                                           timeout=TimeOut},
                                    S#state.events),
          Pid ! {MsgRef, ok},
          loop(S#state{events=NewEvents});
        false ->
          Pid ! {MsgRef, {error, bad_timeout}},
          loop(S)
      end;
    
    % cancel an event
    {Pid, MsgRef, {cancel, Name}} ->
      Events = case orddict:find(Name, S#state.events) of
        {ok, E} -> 
          event:cancel(E#event.pid),
          orddict:erase(Name, S#state.events);
        error ->
          S#state.events
      end,
      Pid ! {MsgRef, ok},
      loop(S#state{events=Events});

    % an event is finished
    {done, Name} ->
      case orddict:find(Name, S#state.events) of
        {ok, E} ->
          send_to_clients({done, E#event.name, E#event.description}, S#state.clients),
          NewEvents = orddict:erase(Name, S#state.events),
          loop(S#state{events=NewEvents});
        error ->
          % in case we cancel an event and it fires at the same time
          loop(S)
      end;

    % let the process die; do any cleanup here
    shutdown ->
      exit(shutdown);

    % we got a message that a client died, remove it from state
    {'DOWN', Ref, process, _Pid, _Reason} ->
      loop(S#state{clients=orddict:erase(Ref, S#state.clients)});
    
    % start the loop in the freshest code out there, with 
    % a fully qualified call to ?MODULE:loop
    code_change ->
      ?MODULE:loop(S);

    % trap anything else and log somewhere
    Unknown ->
      io:format("Unknown message: ~p~n", [Unknown]),
      loop(S)

  end.


% since there should only be one running at a time, it's ok
% to register the evserver:
start() ->
  register(?MODULE, Pid=spawn(?MODULE, init, [])),
  Pid.

start_link() ->
  register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
  Pid.

terminate() ->
  ?MODULE ! shutdown.

subscribe(Pid) ->
  Ref = erlang:monitor(process, whereis(?MODULE)),
  ?MODULE ! {self(), Ref, {subscribe, Pid}},
  receive
    {Ref, ok} ->
      {ok, Ref};
    {'DOWN', Ref, process, _Pid, Reason} ->
      {error, Reason}
  after 5000 ->
      {error, timeout}
  end.


add_event(Name, Description, TimeOut) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {add, Name, Description, TimeOut}},
  receive
    {Ref, Msg} -> Msg
  after 5000 ->
      {error, timeout}
  end.

cancel(Name) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {cancel, Name}},
  receive
    {Ref, ok} -> ok
  after 5000 ->
      {error, timeout}
  end.

% accumulate messages
listen(Delay) ->
  receive
    M = {done, _Name, _Description} ->
      [M | listen(0)]
  after Delay*1000 ->
      []
  end.







