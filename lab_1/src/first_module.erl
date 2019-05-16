%%%-------------------------------------------------------------------
%%% @author bluvalor
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. mar 2019 19:38
%%%-------------------------------------------------------------------
-module(first_module).
-author("bluvalor").

%% API
-export([power/2]).

power(_, 0) -> 1;
power(X, N) -> X * power(X, N-1).
