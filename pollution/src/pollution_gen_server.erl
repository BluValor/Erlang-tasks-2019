%%%-------------------------------------------------------------------
%%% @author bluvalor
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. maj 2019 10:50
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("bluvalor").
-behaviour(gen_server).

%% API
-export([start_link/1, init/1, handle_call/3, add_station/2, add_value/2, print/0]).


start_link(_) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_) -> {ok, pollution:create_monitor()}.


%% dodać case, jeżeli moze zwrócić błąd i zwrócić różne rzeczy


add_station(Name, Coords) -> gen_server:call(pollution, {add_station, Name, Coords}).
add_value(Name, Coords) -> gen_server:call(pollution, {add_value, Name, Coords}).
print() -> gen_server:call(?MODULE, {print}).


handle_call({add_station, V1, V2}, _From, LoopData) ->
  try pollution:add_station(V1, V2, LoopData) of
    P          -> {reply, success, P}
  catch
    throw:Text -> {reply, {failure, Text}, LoopData}
  end;

handle_call({add_value, V1, V2, V3, V4}, _From, LoopData) ->
  try pollution:add_value(V1, V2, V3, V4, LoopData) of
    P          -> {reply, success, P}
  catch
    throw:Text -> {reply, {failure, Text}, LoopData}
  end;

handle_call({print}, _From, LoopData) ->
  {reply, LoopData, LoopData}.