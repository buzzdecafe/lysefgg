-module(m8ball_server).
-behavior(gen_server).
-export([start_olink/0, stop/0, ask/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

% interface
start_link() -> gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:call({global, ?MODULE}, stop).

ask(_Q) -> gen_server:call({global, ?MODULE}, question).

% callbacks
init([]) ->
  <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
  random_seed(A, B, C),
  {ok, []}.

handle_call(question, _From, State) -> 
  {ok, Answers} = application:get_env(m8ball, answers),
  Answer = element(random:uniform(tuple_size(Answers)), Answers),
  {reply, Answer, State};
handle_call(stop, _, State) -> {stop, normal, ok, State};
handle_call(_, _, State) -> {noreply, State}.

handle_cast(_, State) -> {noreply, State}.

handle_info(_, State) -> {noreply, State}.

code_change(_, State, _) -> {ok, State}.

terminate(_, _) -> ok.



