%%%-------------------------------------------------------------------
%%% @author bluvalor
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. mar 2019 19:40
%%%-------------------------------------------------------------------
-module(myLists).
-author("bluvalor").

%% API
-export([contains/2, duplicateElements/1, sumFloats/1, sumFloatsTail/2]).

contains([], _) -> false;
contains([H | T], V) -> (H =:= V) or contains(T, V).

duplicateElements([]) -> [];
duplicateElements([H | T]) -> [H, H | duplicateElements(T)].

sumFloats([]) -> 0.0;
sumFloats([H | T]) when is_float(H) -> H + sumFloats(T);
sumFloats([_ | T]) -> sumFloats(T).

sumFloatsTail([], Acc) -> Acc;
sumFloatsTail([H | T], Acc) when is_float(H) -> sumFloatsTail(T, Acc + H);
sumFloatsTail([_ | T], Acc) -> sumFloatsTail(T, Acc).