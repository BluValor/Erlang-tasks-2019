%%%-------------------------------------------------------------------
%%% @author bluvalor
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. maj 2019 23:41
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("bluvalor").

%% API
-export([start/0, stop/0, init/0, print_monitor/0, add_station/2, add_value/4, remove_value/3, get_one_value/3,
  get_station_mean/2, get_daily_mean/2, get_daily_over_limit/3, crash/0]).


do_action(Action, Args) ->
  pserver ! {self(), Action, Args},
  receive
    {done, Result} -> Result
  end.


add_station(Name, Coords)                -> do_action(add_station, {Name, Coords}).
add_value(V1, Time, Param, Value)        -> do_action(add_value, {V1, Time, Param, Value}).
remove_value(Name, Time, Param)          -> do_action(remove_value, {Name, Time, Param}).
get_one_value(Name, Time, Param)         -> do_action(get_one_value, {Name, Time, Param}).
get_station_mean(Name, Param)            -> do_action(get_station_mean, {Name, Param}).
get_daily_mean(Param, Date)              -> do_action(get_daily_mean, {Param, Date}).
get_daily_over_limit(Param, Date, Limit) -> do_action(get_daily_over_limit, {Param, Date, Limit}).
print_monitor()                          -> do_action(print_monitor, nothing).
crash()                                  -> pserver ! crash.


loop(P) ->
  receive
    {Pid, add_station, {V1, V2}}              ->
                                                 try pollution:add_station(V1, V2, P) of
                                                   P2 -> Pid ! {done, ok}, loop(P2)
                                                 catch
                                                   throw:Text -> Pid ! {done, Text}, loop(P)
                                                 end;
    {Pid, add_value, {V1, V2, V3, V4}}        -> try pollution:add_value(V1, V2, V3, V4, P) of
                                                   P2 -> Pid ! {done, ok}, loop(P2)
                                                 catch
                                                   throw:Text -> Pid ! {done, Text}, loop(P)
                                                 end;
    {Pid, remove_value, {V1, V2, V3}}         -> try pollution:remove_value(V1, V2, V3, P) of
                                                   P2 -> Pid ! {done, ok}, loop(P2)
                                                 catch
                                                   throw:Text -> Pid ! {done, Text}, loop(P)
                                                 end;
    {Pid, get_one_value, {V1, V2, V3}}        -> try pollution:get_one_value(V1, V2, V3, P) of
                                                   Result -> Pid ! {done, Result}, loop(P)
                                                 catch
                                                   throw:Text -> Pid ! {done, Text}, loop(P)
                                                 end;
    {Pid, get_station_mean, {V1, V2}}         -> try pollution:get_station_mean(V1, V2, P) of
                                                   Result -> Pid ! {done, Result}, loop(P)
                                                 catch
                                                   throw:Text -> Pid ! {done, Text}, loop(P)
                                                 end;
    {Pid, get_daily_mean, {V1, V2}}           -> try pollution:get_daily_mean(V1, V2, P) of
                                                   Result -> Pid ! {done, Result}, loop(P)
                                                 catch
                                                   throw:Text -> Pid ! {done, Text}, loop(P)
                                                 end;
    {Pid, get_daily_over_limit, {V1, V2, V3}} -> try pollution:get_daily_over_limit(V1, V2, V3, P) of
                                                   Result -> Pid ! {done, Result}, loop(P)
                                                 catch
                                                   throw:Text -> Pid ! {done, Text}, loop(P)
                                                 end;
    {Pid, print_monitor, _}                   -> Pid ! {done, P}, loop(P);
    stop                                      -> done;
    _                                         -> throw("Stopped!")
  end.


init() -> loop(pollution:create_monitor()).


stop() -> pserver ! stop.


start() -> register(pserver, spawn(?MODULE, init, [])).
