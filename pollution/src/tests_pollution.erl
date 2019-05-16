%%%-------------------------------------------------------------------
%%% @author bluvalor
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. maj 2019 02:44
%%%-------------------------------------------------------------------
-module(tests_pollution).
-author("bluvalor").

%% API
-export([]).
-include_lib("eunit/include/eunit.hrl").


create_monitor_test() ->
  ?assertEqual(pollution:create_monitor(), #{}).


add_station_test() ->
  P = pollution:create_monitor(),
  ?assertEqual(P2 = pollution:add_station("station1", {1, 2}, P),
    #{"station1" => {station,{1,2},#{}}}),
  ?assertException(throw, "Station with this name already exists!",
    pollution:add_station("station1", {1, 3}, P2)),
  ?assertException(throw, "Station with this position already exists!",
    pollution:add_station("station2", {1, 2}, P2)).


add_value_test() ->
  P = pollution:create_monitor(),
  P2 = pollution:add_station("station1", {1, 2}, P),
  ?assertEqual(P3 = pollution:add_value({1, 2}, {{2019,5,9},{1,8,52}}, "CO2", 11.1, P2),
    #{"station1" => {station,{1,2}, #{"CO2" => [{param,11.1,{{2019,5,9},{1,8,52}}}]}}}),
  ?assertEqual(pollution:add_value("station1", {{2019,5,9},{1,8,52}}, "CO2", 11.1, P2),
    #{"station1" => {station,{1,2}, #{"CO2" => [{param,11.1,{{2019,5,9},{1,8,52}}}]}}}),
  ?assertException(throw, "Station with this coordinates doesn't exist!",
    pollution:add_value({100, 200}, {{2019,5,9},{1,8,52}}, "CO2", 11.1, P2)),
  ?assertException(throw, "Station with this name doesn't exist!",
    pollution:add_value("station2", {{2019,5,9},{1,8,52}}, "CO2", 11.1, P2)),
  ?assertException(throw, "Value like this already exists!",
    pollution:add_value("station1", {{2019,5,9},{1,8,52}}, "CO2", 11.1, P3)).


remove_value_test() ->
  P = pollution:create_monitor(),
  P2 = pollution:add_station("station1", {1, 2}, P),
  P3 = pollution:add_value({1, 2}, {{2019,5,9},{1,8,52}}, "CO2", 11.1, P2),
  ?assertException(throw, "Station with this name doesn't exist!",
    pollution:remove_value("station2", {{2019,5,9},{1,8,52}}, "CO2", P3)),
  ?assertException(throw, "This station doesn't have data for this parameter!",
    pollution:remove_value("station1", {{2019,5,9},{1,8,52}}, "POKEMONY", P3)),
  ?assertException(throw, "There is no data for given time!",
    pollution:remove_value("station1", {{3019,5,9},{1,8,52}}, "CO2", P3)),
  ?assertEqual(pollution:remove_value("station1", {{2019,5,9},{1,8,52}}, "CO2", P3),
    #{"station1" => {station,{1,2},#{"CO2" => []}}}).


get_one_value_test() ->
  P = pollution:create_monitor(),
  P2 = pollution:add_station("station1", {1, 2}, P),
  P3 = pollution:add_value({1, 2}, {{2019,5,9},{1,8,52}}, "CO2", 11.1, P2),
  ?assertException(throw, "Station with this name doesn't exist!",
    pollution:get_one_value("station2", {{2019,5,9},{1,8,52}}, "CO2", P3)),
  ?assertException(throw, "This station doesn't have data for this parameter!",
    pollution:get_one_value("station1", {{2019,5,9},{1,8,52}}, "POKEMONY", P3)),
  ?assertException(throw, "There is no data for given time!",
    pollution:get_one_value("station1", {{3019,5,9},{1,8,52}}, "CO2", P3)),
  ?assertEqual(pollution:get_one_value("station1", {{2019,5,9},{1,8,52}}, "CO2", P3), 11.1).


get_station_mean_test() ->
  P = pollution:create_monitor(),
  P2 = pollution:add_station("station1", {1, 2}, P),
  P3 = pollution:add_value({1, 2}, {{2019,5,9},{1,8,52}}, "CO2", 11.1, P2),
  P4 = pollution:add_value({1, 2}, {{2019,5,9},{2,8,52}}, "CO2", 33.3, P3),
  ?assertException(throw, "Station with this name doesn't exist!",
    pollution:get_station_mean("station2", "CO2", P4)),
  ?assertException(throw, "This station doesn't have data for this parameter!",
    pollution:get_station_mean("station1", "POKEMONY", P4)),
  ?assertEqual(pollution:get_station_mean("station1", "CO2", P4), 22.2).


get_daily_mean_test() ->
  P = pollution:create_monitor(),
  P2 = pollution:add_station("station1", {1, 2}, P),
  P3 = pollution:add_value({1, 2}, {{2019,5,9},{1,8,52}}, "CO2", 11.1, P2),
  P4 = pollution:add_station("station2", {1, 3}, P3),
  P5 = pollution:add_value({1, 3}, {{2019,5,9},{2,8,52}}, "CO2", 33.3, P4),
  ?assertException(throw, "There is no data for this parameter!",
    pollution:get_daily_mean("CO2", {2019,5,10}, P5)),
  ?assertException(throw, "There is no data for this parameter!",
    pollution:get_daily_mean("POKEMONY", {2019,5,9}, P5)),
  ?assertEqual(pollution:get_daily_mean("CO2", {2019,5,9}, P5), 22.2).


get_daily_over_limit_test() ->
  P = pollution:create_monitor(),
  P2 = pollution:add_station("station1", {1, 2}, P),
  P3 = pollution:add_value({1, 2}, {{2019,5,9},{1,8,52}}, "CO2", 11.1, P2),
  P4 = pollution:add_station("station2", {1, 3}, P3),
  P5 = pollution:add_value({1, 3}, {{2019,5,9},{2,8,52}}, "CO2", 33.3, P4),
  ?assertEqual(pollution:get_daily_over_limit("CO2", {2019,5,9}, 22.2, P5), ["station2"]),
  ?assertEqual(pollution:get_daily_over_limit("CO", {2019,5,9}, 22.2, P5), []),
  ?assertEqual(pollution:get_daily_over_limit("CO2", {2019,5,10}, 22.2, P5), []),
  ?assertEqual(pollution:get_daily_over_limit("CO2", {2019,5,9}, 10.0, P5), ["station2","station1"]).



