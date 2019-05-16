%%%-------------------------------------------------------------------
%%% @author bluvalor
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. maj 2019 03:47
%%%-------------------------------------------------------------------
-module(tests_pollution_server).
-author("bluvalor").

%% API
-export([]).
-include_lib("eunit/include/eunit.hrl").


run_test() ->

  %% start
  ?assertEqual(pollution_server:start(), true),

  %% add_station
  ?assertEqual(pollution_server:add_station("station1", {1, 2}), ok),
  ?assertEqual("Station with this name already exists!",
    pollution_server:add_station("station1", {1, 3})),
  ?assertEqual("Station with this position already exists!",
    pollution_server:add_station("station2", {1, 2})),

  %% add_value
  ?assertEqual(pollution_server:add_value({1, 2}, {{2019,5,9},{1,8,52}}, "CO2", 11.1), ok),
  ?assertEqual(pollution_server:add_value("station1", {{2019,5,8},{1,8,52}}, "CO2", 12.1), ok),
  ?assertEqual("Station with this coordinates doesn't exist!",
    pollution_server:add_value({100, 200}, {{2019,5,9},{1,8,52}}, "CO2", 11.1)),
  ?assertEqual("Station with this name doesn't exist!",
    pollution_server:add_value("station2", {{2019,5,9},{1,8,52}}, "CO2", 11.1)),
  ?assertEqual("Value like this already exists!",
    pollution_server:add_value("station1", {{2019,5,9},{1,8,52}}, "CO2", 11.1)).

%%remove_value_test() ->
%%  pollution_server:start(),
%%  pollution_server:add_station("station1", {1, 2}, P),
%%  P3 = pollution_server:add_value({1, 2}, {{2019,5,9},{1,8,52}}, "CO2", 11.1, P2),
%%  ?assertEqual("Station with this name doesn't exist!",
%%    pollution_server:remove_value("station2", {{2019,5,9},{1,8,52}}, "CO2", P3)),
%%  ?assertEqual("This station doesn't have data for this parameter!",
%%    pollution_server:remove_value("station1", {{2019,5,9},{1,8,52}}, "POKEMONY", P3)),
%%  ?assertEqual("There is no data for given time!",
%%    pollution_server:remove_value("station1", {{3019,5,9},{1,8,52}}, "CO2", P3)),
%%  ?assertEqual(pollution_server:remove_value("station1", {{2019,5,9},{1,8,52}}, "CO2", P3),
%%    #{"station1" => {station,{1,2},#{"CO2" => []}}}).
%%
%%
%%get_one_value_test() ->
%%  pollution_server:start(),
%%  pollution_server:add_station("station1", {1, 2}, P),
%%  P3 = pollution_server:add_value({1, 2}, {{2019,5,9},{1,8,52}}, "CO2", 11.1, P2),
%%  ?assertEqual("Station with this name doesn't exist!",
%%    pollution_server:get_one_value("station2", {{2019,5,9},{1,8,52}}, "CO2", P3)),
%%  ?assertEqual("This station doesn't have data for this parameter!",
%%    pollution_server:get_one_value("station1", {{2019,5,9},{1,8,52}}, "POKEMONY", P3)),
%%  ?assertEqual("There is no data for given time!",
%%    pollution_server:get_one_value("station1", {{3019,5,9},{1,8,52}}, "CO2", P3)),
%%  ?assertEqual(pollution_server:get_one_value("station1", {{2019,5,9},{1,8,52}}, "CO2", P3), 11.1).
%%
%%
%%get_station_mean_test() ->
%%  pollution_server:start(),
%%  pollution_server:add_station("station1", {1, 2}, P),
%%  P3 = pollution_server:add_value({1, 2}, {{2019,5,9},{1,8,52}}, "CO2", 11.1, P2),
%%  P4 = pollution_server:add_value({1, 2}, {{2019,5,9},{2,8,52}}, "CO2", 33.3, P3),
%%  ?assertEqual("Station with this name doesn't exist!",
%%    pollution_server:get_station_mean("station2", "CO2", P4)),
%%  ?assertEqual("This station doesn't have data for this parameter!",
%%    pollution_server:get_station_mean("station1", "POKEMONY", P4)),
%%  ?assertEqual(pollution_server:get_station_mean("station1", "CO2", P4), 22.2).
%%
%%
%%get_daily_mean_test() ->
%%  pollution_server:start(),
%%  pollution_server:add_station("station1", {1, 2}, P),
%%  P3 = pollution_server:add_value({1, 2}, {{2019,5,9},{1,8,52}}, "CO2", 11.1, P2),
%%  P4 = pollution_server:add_station("station2", {1, 3}, P3),
%%  P5 = pollution_server:add_value({1, 3}, {{2019,5,9},{2,8,52}}, "CO2", 33.3, P4),
%%  ?assertEqual("There is no data for this parameter!",
%%    pollution_server:get_daily_mean("CO2", {2019,5,10}, P5)),
%%  ?assertEqual("There is no data for this parameter!",
%%    pollution_server:get_daily_mean("POKEMONY", {2019,5,9}, P5)),
%%  ?assertEqual(pollution_server:get_daily_mean("CO2", {2019,5,9}, P5), 22.2).
%%
%%
%%get_daily_over_limit_test() ->
%%  pollution_server:start(),
%%  pollution_server:add_station("station1", {1, 2}, P),
%%  P3 = pollution_server:add_value({1, 2}, {{2019,5,9},{1,8,52}}, "CO2", 11.1, P2),
%%  P4 = pollution_server:add_station("station2", {1, 3}, P3),
%%  P5 = pollution_server:add_value({1, 3}, {{2019,5,9},{2,8,52}}, "CO2", 33.3, P4),
%%  ?assertEqual(pollution_server:get_daily_over_limit("CO2", {2019,5,9}, 22.2, P5), ["station2"]),
%%  ?assertEqual(pollution_server:get_daily_over_limit("CO", {2019,5,9}, 22.2, P5), []),
%%  ?assertEqual(pollution_server:get_daily_over_limit("CO2", {2019,5,10}, 22.2, P5), []),
%%  ?assertEqual(pollution_server:get_daily_over_limit("CO2", {2019,5,9}, 10.0, P5), ["station2","station1"]).
%%
