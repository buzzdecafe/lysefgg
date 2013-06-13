-module(hotload).
-export([server/1, upgrade/1]).

server(State) ->
  receive

    % loop in the new version of the module
    update ->
      NewState = ?MODULE:upgrade(State),
      ?MODULE:server(NewState);

    % otherwise, stay in same state
    _ ->
      server(State)
  end.

% transform and return state
upgrade(OldState) -> 
  io:format("hoge~p", [OldState]).


