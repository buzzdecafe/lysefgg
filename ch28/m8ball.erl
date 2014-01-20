-module(m8ball).
-behavior(application).
-export([start/2, stop/1, ask/1]).

start(normal, []) -> m8ball_sup:start_link();
start({takeover, OtherNode}, []) -> m8ball_sup:start_link();

stop(_) -> ok.

ask(Question) -> m8ball_server:ask(Question).




