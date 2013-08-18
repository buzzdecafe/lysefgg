-module(musicians).
-behaviour(gen_server).

-export([start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {name="", role, skill=good}).
-define(DELAY, 750).

start_link(Role, Skill) -> 
  gen_server:start_link({local, Role}, ?MODULE, [Role, Skill], []).

stop(Role) ->
  gen_server:call(Role, stop).

init([Role, Skill]) ->
  process_flag(trap_exit, true), % when parent exits ...
  random:seed(now()),
  TimeToPlay = random:uniform(3000),
  Name = pick_name(),
  StrRole = atom_to_list(Role),
  io:format("Musician ~s, playing the ~s, entered the room~n", [Name, StrRole]),
  {ok, #state{name=Name, role=StrRole, skill=Skill}, TimeToPlay}.

pick_name() ->
  lists:nth(random:uniform(10), firstnames()) 
  ++ " " ++
  lists:nth(random:uniform(10), lastnames()).

firstnames() ->
  ["F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10"].

lastnames() ->
  ["L1", "L2", "L3", "L4", "L5", "L6", "L7", "L8", "L9", "L10"].


% the only call is to stop the musician server.
% unexpected messages get no reply, let the caller crash.
handle_call(stop, _From, S=#state{}) ->
  {stop, normal, ok, S};
handle_call(_Message, _From, S) ->
  {noreply, S, ?DELAY}.

handle_cast(_Message, S) ->
  {noreply, S, ?DELAY}.

handle_info(timeout, S=#state{name=N, skill=good}) ->
  io:format("~s made sound~n", [N]),
  {noreply, S, ?DELAY};
handle_info(timeout, S=#state{name=N, skill=bad}) ->
  case random:uniform(5) of
    1 -> 
      io:format("~s made a horrible noise~n", [N]),
      {stop, bad_note, S};
    _ ->
      io:format("~s made sound~n", [N]),
      {noreply, S, ?DELAY}
  end;
handle_info(_Message, S) -> 
  {noreply, S, ?DELAY}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(normal, S) ->
  io:format("~s left the room (~s)~n", [S#state.name, S#state.role]);
terminate(bad_note, S) ->
  io:format("~s sucks. He's fired. (~s)~n", [S#state.name, S#state.role]);
terminate(shutdown, S) ->
  io:format("Manager just fired everybody. ~s back to playing n subway~n", [S#state.name]);
terminate(_Reason, S) ->
  io:format("~s has been kicked out (~s)~n", [S#state.name, S#state.role]).



