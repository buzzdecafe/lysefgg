-module(regis_server).
-behavior(gen_server).
-include_lib("stdlib/include/ms+transform.hrl").

-export([start_link/0, stop/0, register/2, unregister/1, whereis/1, get_names/0,
         init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).
  
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:call(?MODULE, stop).

register(Name, Pid) -> gen_server:call(?MODULE, {register, Name, Pid}).

unregister(Name) -> gen_server:call(?MODULE, {unregister, Name}).

whereis(Name) -> ok.

get_names() -> ok.

% gen_server callbacks

init([]) ->
  ?MODULE = ets:new(regis, [set, named_table, protected]),
  {ok, ?MODULE}.

handle_call({register, Name, Pid}, _From, Tid) ->
  MatchSpec = ets:fun2ms(fun({N, P, _Ref}) when N == Name; P == Pid -> {N, P} end),
  case ets:select(Tid, MatchSpec) of 
    [] -> 
      Ref = erlang:monitor(process, Pid),
      ets:insert(Tid, {Name, Pid, Ref}),
      {reply, ok, Tid};
    [{Name, _} | _] ->
      {reply, {error, name_taken}, Tid};
    [{_, Pid} | _] ->
      {reply, {error, already_named}, Tid}
  end;

handle_call({unregister, Name}, _, Tid) ->
  case ets:lookup(Tid, Name) of
    [{Name, _, Ref}] ->
      erlang:demonitor(Ref, [flush]),
      ets:delete(Tid, Name),
      {reply, ok, Tid};
    [] ->
      {reply, ok, Tid}
  end;

handle_call(stop, _, Tid) ->
  ets:delete(Tid),
  {stop, normal, ok, Tid};

handle_call(_Evt, _From, State) ->
  {noreply, State}.


handle_cast(_Evt, State) ->
  {noreply, State};

handle_info({'DOWN', Ref, process, _Pid, _Reason}, Tid) ->
  ets:match_delete(Tid, {'_', '_', Ref}),
  {noreply, Tid};

handle_info(_Evt, State) ->
  {noreply, State}.

code_change(_Old, State, _Extra) ->
  {ok, State}.

terminate(_Why, _State) -> ok.

whereis(Name) -> 
  case ets:lookup(?MODULE, Name) of
    [{Name, Pid, _Ref}] -> Pid;
    [] -> undefined
  end.

get_names() ->
  MatchSpec = ets:fun2ms(fun({Name, _, _}) Name end),
  ets:select(?MODULE, MatchSpec).


