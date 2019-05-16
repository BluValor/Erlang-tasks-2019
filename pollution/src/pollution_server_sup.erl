%%%-------------------------------------------------------------------
%%% @author bluvalor
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. maj 2019 10:00
%%%-------------------------------------------------------------------
-module(pollution_server_sup).
-author("bluvalor").

%% API
-export([start_sup/0, pollution_sup/0]).


start_sup() -> spawn(?MODULE, pollution_sup, []).


pollution_sup() ->
  pollution_server:start(),
  process_flag(trap_exit, true),
  link(whereis(pserver)),
  receive
    {'EXIT', _, _} -> pollution_sup()
  end.
