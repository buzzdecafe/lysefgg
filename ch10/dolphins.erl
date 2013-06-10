-module(dolphins).
-compile(export_all).


dolphin1() -> 
  receive
    do_a_flip -> io:format("How about no?~n");
    fish -> io:format("So long and thanks for all the fish.~n");
    _ -> io:format("We dolphins are smarter than you.~n")
  end.

dolphin2() ->
  receive
    {From, do_a_flip} -> From ! "How about no?";
    {From, fish} -> From ! "So long and thanks for all the fish.";
    _ -> io:format("We dolphins are smarter than you.~n")
  end.

dolphin3() ->
  receive
    {From, do_a_flip} -> From ! "How about no?", dolphin3();
    {From, fish} -> From ! "Thanks for the fish", dolphin3();
    _ -> io:format("whatever.~n"), dolphin3()
  end.



