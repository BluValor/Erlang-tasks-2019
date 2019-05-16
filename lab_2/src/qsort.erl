%%%-------------------------------------------------------------------
%%% @author bluvalor
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. mar 2019 11:59
%%%-------------------------------------------------------------------
-module(qsort).
-author("bluvalor").

%% API
-export([qs/1, randomElements/3, compareSpeeds/3, compareValues/2]).


lessThan(List, Arg) -> [X || X <- List, X < Arg].


grtEqThan(List, Arg) -> [X || X <- List, X >= Arg].


qs([]) -> [];
qs([Pivot|Tail]) -> qs( lessThan(Tail,Pivot) ) ++ [Pivot] ++ qs( grtEqThan(Tail,Pivot) ).


randomElements(0, _, _) -> [];
randomElements(N, Min, Max) -> [rand:uniform(Max - Min + 1) + Min - 1 | randomElements(N - 1, Min, Max)].


getTime({Time, _}) -> Time.


getComparator(Val1, Val2) ->
  if
    Val1 > Val2 -> ">";
    Val1 < Val2 -> "<";
    true        -> "="
  end.


compareValues(Val1, Val2) -> io:format("~B ~s ~B~n", [Val1, getComparator(Val1, Val2), Val2]).


compareSpeeds(List, Fun1, Fun2) -> compareValues(getTime(timer:tc(Fun1, [List])), getTime(timer:tc(Fun2, [List]))).