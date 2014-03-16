-module(mafiapp_sup).
-behavior(supervisor).
-export([start_link/1,init/1]).

start_link(Tables) -> supervispr:start_link(?MODULE, Tables).

init(Tables) -> {ok, {{one_for_one, 1, 1}, []}}.


