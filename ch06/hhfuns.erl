-module(hhfuns).
-compile(export_all).

one() -> 1.
two() -> 2.
add(X, Y) -> X() + Y().

increment([]) -> [];
increment([H|T]) -> [H + 1 | increment(T)].

decrement([]) -> [];
decrement([H|T]) -> [H - 1 | decrement(T)].

map(_, []) -> [];
map(F, [H|T]) -> [F(H) | map(F, T)].

incr(X) -> X + 1.
decr(X) -> X - 1.

even(L) -> lists:reverse(even(L, [])).

even([], Acc) -> Acc;
even([H|T], Acc) when H rem 2 == 0 -> even(T, [H|Acc]);
even([_|T], Acc) -> even(T, Acc).

old_men(L) -> old_men(L, []).

old_men([], Acc) -> Acc;
old_men([Person = {male, Age}|People], Acc) when Age > 60 -> old_men(People, [Person|Acc]);
old_men([_|People], Acc) -> old_men(People, Acc).

filter(F, L) -> lists:reverse(filter(F, L, [])).

filter(_, [], Acc) -> Acc;
filter(F, [H|T], Acc) -> 
  case F(H) of 
    true -> filter(F, T, [H|Acc]);
    false -> filter(F, T, Acc)
  end.

fold(_, Start, []) -> Start;
fold(F, Start, [H|T]) -> fold(F, F(H, Start), T).

reverse(L) -> fold(fun(X, Acc) -> [X|Acc] end, [], L).

map2(F, L) -> reverse(fold(fun(X, Acc) -> [F(X)|Acc] end, [], L)).

filter2(Pred, L) -> 
  F = fun(X, Acc) -> 
      case Pred(X) of 
        true -> [X|Acc];
        false -> Acc
      end
  end,
  reverse(fold(F, [], L)).



