%%%-------------------------------------------------------------------
%%% @author bluvalor
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. kwi 2019 16:35
%%%-------------------------------------------------------------------
-module(pollution).
-author("bluvalor").

%% API
-export([create_monitor/0, add_station/3, add_value/5, remove_value/4, get_one_value/4, get_station_mean/3,
  get_daily_mean/3, get_daily_over_limit/4]).


%% Data structure:
%% Monitor is a map where key is station name, value is station record.
%% Station record contains its position and a map in which its data is stored. In this map key is the name of parameter
%% and value is a list of parameter records.
%% Parameter record contains its value and time.


-record(station, { s_coords = {0, 0}, data = #{} }).
-record(param, { value = 0, time = {{0, 0, 0}, {0, 0, 0}} }).


create_monitor() -> #{}.


get_first_value_from_list([]) -> none;
get_first_value_from_list([H | _]) -> H.


get_station_for_coords(Coords, Monitor) ->
  get_first_value_from_list(maps:keys(maps:filter(fun(_, #station{s_coords = S_coords}) -> Coords == S_coords end, Monitor))).


add_station(Name, Coords, Monitor) ->
  case maps:is_key(Name, Monitor) of
    true -> throw("Station with this name already exists!");
    _    -> case get_station_for_coords(Coords, Monitor) of
              none -> Monitor#{Name => #station{s_coords = Coords}};
              _    -> throw("Station with this position already exists!")
            end
  end.


check_for_param_time(Time, List) ->
  case lists:filter(fun(#param{time = P_time}) -> Time == P_time end, List) of
    [] -> false;
    _   -> true
  end.


add_value({X_coord, Y_coord}, Time, Param, Value, Monitor) ->
  case get_station_for_coords({X_coord, Y_coord}, Monitor) of
    none -> throw("Station with this coordinates doesn't exist!");
    Name -> add_value(Name, Time, Param, Value, Monitor)
  end;

add_value(Name, Time, Param, Value, Monitor) ->
  case maps:is_key(Name, Monitor) of
    false -> throw("Station with this name doesn't exist!");
    _  ->
      Station = maps:get(Name, Monitor),
      Data = Station#station.data,
      case maps:is_key(Param, Data) of
        false -> Monitor#{Name := Station#station{data = maps:put(Param, [#param{value = Value, time = Time}], Data)}};
        _     ->
          case check_for_param_time(Time, maps:get(Param, Data)) of
            true -> throw("Value like this already exists!");
            _    -> Monitor#{Name := Station#station{data = maps:update(Param, [#param{value = Value, time = Time} | maps:get(Param, Data)], Data)}}
          end
      end
  end.


remove_value(Name, Time, Param, Monitor) ->
  case maps:is_key(Name, Monitor) of
    false -> throw("Station with this name doesn't exist!");
    _  ->
      Station = maps:get(Name, Monitor),
      Data = Station#station.data,
      case maps:is_key(Param, Data) of
        false -> throw("This station doesn't have data for this parameter!");
        _     ->
          case check_for_param_time(Time, maps:get(Param, Data)) of
            false -> throw("There is no data for given time!");
            _     -> Monitor#{Name := Station#station{data = maps:update(Param, lists:filter(fun(#param{time = P_time}) -> Time /= P_time end, maps:get(Param, Data)), Data)}}
          end
      end
  end.


get_one_value(Name, Time, Param, Monitor) ->
  case maps:is_key(Name, Monitor) of
    false -> throw("Station with this name doesn't exist!");
    _  ->
      Station = maps:get(Name, Monitor),
      Data = Station#station.data,
      case maps:is_key(Param, Data) of
        false -> throw("This station doesn't have data for this parameter!");
        _     ->
          case check_for_param_time(Time, maps:get(Param, Data)) of
            false -> throw("There is no data for given time!");
            _     ->
              P_value = get_first_value_from_list(lists:filter(fun(#param{time = P_time}) -> Time == P_time end, maps:get(Param, Data))),
              P_value#param.value
          end
      end
  end.


get_station_mean(Name, Param, Monitor) ->
  case maps:is_key(Name, Monitor) of
    false -> throw("Station with this name doesn't exist!");
    _  ->
      Station = maps:get(Name, Monitor),
      Data = Station#station.data,
      case maps:is_key(Param, Data) andalso maps:get(Param, Data) /= [] of
        false -> throw("This station doesn't have data for this parameter!");
        _     -> lists:foldr(fun(#param{value = Value}, Acc) -> Value + Acc end, 0, maps:get(Param, Data)) / length(maps:get(Param, Data))
      end
  end.


data_contains_param(Param, Data) ->
  lists:foldr(fun(K, Acc) -> Acc or (K == Param) end, false, maps:keys(Data)).


count_average({Sum, Count}) ->
  if
    Count == 0 -> throw("There is no data for this parameter!");
    true       -> Sum / Count
  end.


av_data_for_param_for_day(Param, Date, Data, {Start_sum, Start_count}) ->
  lists:foldr(fun(#param{value = Value, time = {P_date, _}}, {Sum, Count}) -> case P_date == Date of
                                                                                false -> {Sum, Count};
                                                                                _     -> {Sum + Value, Count + 1}
                                                                              end end, {Start_sum, Start_count}, maps:get(Param, Data)).


get_daily_mean(Param, Date, Monitor) ->
  count_average(maps:fold(fun(_, #station{data = Data}, {Sum_av, Count}) -> case data_contains_param(Param, Data) of
                                                                              false -> {Sum_av, Count};
                                                                              _     -> av_data_for_param_for_day(Param, Date, Data, {Sum_av, Count})
                                                                            end end, {0, 0}, Monitor)).


get_daily_over_limit_for_station(Param, Date, Limit, #station{data = Data}) ->
  case maps:is_key(Param, Data) of
    false -> false;
    _     ->
      P_data = maps:get(Param, Data),
      lists:foldr(fun(#param{value = Value, time = {P_date, _}}, Result) -> case P_date == Date of
                                                                              false -> Result or false;
                                                                              _     -> Result or (Value > Limit)
                                                                            end end, false, P_data)
  end.


get_daily_over_limit(Param, Date, Limit, Monitor) ->
  maps:fold(fun(Name, Station, Result) -> case get_daily_over_limit_for_station(Param, Date, Limit, Station) of
                                            false -> Result;
                                            _     -> [Name | Result]
                                          end end, [], Monitor).
