-module(functions).
-compile(export_all). % TODO: replace with -export()

head([H|_]) -> H.
second([_,X|_]) -> X.

same(X,X) -> 
  true;
same(_,_) ->
  false.

valid_time({Date = {Y,M,D}, Time = {H,Min,S}}) ->
  io:format("The Date tuple (~p) says today is: ~p/~p/~p, ~n", [Date,Y,M,D]),
  io:format("The Time tuple (~p) says today is: ~p:~p:~p. ~n", [Time,H,Min,S]);
valid_time(_) ->
  io:format("Stop feeding me wrong data!~n").

right_age(X) when X >= 16, x =< 104 ->
  true;
right_age(_) ->
  false.
