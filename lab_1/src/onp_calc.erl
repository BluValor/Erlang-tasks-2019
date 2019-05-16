%%%-------------------------------------------------------------------
%%% @author bluvalor
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. mar 2019 19:58
%%%-------------------------------------------------------------------
-module(onp_calc).
-author("bluvalor").

%% API
-export([onp/1]).


new_stack() -> [].


push_stack(V, Stack) -> [V | Stack].


pop_stack([]) -> empty;
pop_stack([V | _]) -> V.


popped_stack([]) -> empty;
popped_stack([_ | T]) -> T.


check_operator("+") -> double;
check_operator("-") -> double;
check_operator("*") -> double;
check_operator("/") -> double;
check_operator("pow") -> double;
check_operator("sqrt") -> single;
check_operator("sin") -> single;
check_operator("cos") -> single;
check_operator(_) -> none.


get_number(Value) ->
  case string:str(Value, ".") > 0 of
    true -> list_to_float(Value);
    false -> list_to_integer(Value)
  end.


do_double_operation("+", Val1, Val2) -> Val2 + Val1;
do_double_operation("-", Val1, Val2) -> Val2 - Val1;
do_double_operation("*", Val1, Val2) -> Val2 * Val1;
do_double_operation("/", Val1, Val2) -> Val2 / Val1;
do_double_operation("pow", Val1, Val2) -> math:pow(Val2, Val1).


do_single_operation("sqrt", Val) -> math:sqrt(Val);
do_single_operation("sin", Val) -> math:sin(Val);
do_single_operation("cos", Val) -> math:cos(Val).


check_end(Stack) ->
  case popped_stack(Stack) of
    [] -> pop_stack(Stack);
    _     -> error
end.


onp(Input_str) -> onp_calculate(string:tokens(Input_str, " "), []).


onp_calculate(Input, Stack) ->
  case pop_stack(Input) of
    empty ->
      check_end(Stack);
    _     ->
      case check_operator(pop_stack(Input)) of
        none ->
          onp_calculate(popped_stack(Input), push_stack(get_number(pop_stack(Input)), Stack));
        single ->
          onp_calculate(popped_stack(Input), push_stack(do_single_operation(pop_stack(Input), pop_stack(Stack)), popped_stack(Stack)));
        double ->
          onp_calculate(popped_stack(Input), push_stack(do_double_operation(pop_stack(Input), pop_stack(Stack), pop_stack(popped_stack(Stack))), popped_stack(popped_stack(Stack))))
      end
  end.


%% 1 2 3 * + 4 5 / - 6 +
%% 1 2 + 3 + 4 + 5 + 6 7 * +
%% 4 7 + 3 / 2 19 - *
%% 17 31 4 + * 26 15 - 2 * 22 - / 1 -