%%%-------------------------------------------------------------------
%%% @author bluvalor
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. kwi 2019 14:37
%%%-------------------------------------------------------------------
-module(fun_extra).
-author("bluvalor").

%% API
-export([map/2, filter/2, sum/1, sum_digits/1, rem3/1]).


map(Fun, List) -> [Fun(X) || X <- List].


filter(Fun, List) -> [X || X <- List, Fun(X)].


sum(List) -> lists:foldl(fun(X, Y) -> X + Y end, 0, List).


sum_digits(0)   -> 0;
sum_digits(Val) -> Val rem 10 + sum_digits(Val div 10).


rem3(List) -> lists:filter(fun(X) -> sum_digits(X) rem 3 == 0 end, List).

