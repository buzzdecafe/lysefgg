-module(useless).
-export([add/2, hello/0, greet_and_add_2/1]).

add(A, B) -> 
  A + B.

%% comment!
%% io:format/1 is the standard function used to output text
hello() ->
  io:format("Hello, world!~n").

greet_and_add_2(X) ->
  hello(),
  add(X, 2).


