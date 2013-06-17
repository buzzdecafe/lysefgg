-module(kitty_gen_server).
-behavior(gen_server).

%% gen_server calls with MODULE, args to init/1, and debug options
%% also takes optional fourth arg {local, Name} which registers the server
%% with a name.
%% returns {ok, Pid}
start_link() -> gen_server:start_link(?MODULE, [], []).

order_cat(Pid, Name, Color, Description) ->
  gen_server:call(Pid, {order, Name, Color, Description}).

% async
return_cat(Pid, Cat=#cat{}) ->
  gen_server:cast(Pid, {return, Cat}).

% sync
close_shop(Pid) ->
  gen_server:call(Pid, terminate).


init([]) -> {ok, []}.

handle_call({order, Name, Color, Description}, _From, Cats) ->
  if Cats =:= [] ->
      {reply, make_cat(Name, Color, Description), _From, Cats};
    Cats =/= [] ->
      {reply, hd(Cats), tl(Cats)}
  end;

handle_call(terminate, _From, Cats) ->
  {stop, normal, ok, Cats}.

handle_cast({return, Cat=#cat{}}, Cats) ->
  {noreply, [Cat|Cats]}.

handle_info(Msg, Cats) ->
  io:format("Unexpected message: ~p~n", [Msg]),
  {noreply, Cats}.

terminate(normal, Cats) ->
  [io:format("~p was set free.~n", [C#cat.name]) || C <- Cats],
  ok.

code_change(_OldVsn, State, _Extra) ->
  % overridden to satisfy behavior, but not getting used
  {ok, State}.

make_cat(N, C, D) ->
  #cat{name=N, color=C, description=D}.


