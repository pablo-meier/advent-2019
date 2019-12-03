-module(day1_app).

-export([start/0]).

start() ->
  InputAsList = read_input_file(),
  Solved = solve(InputAsList),
  io:format("~nPart 1: ~p", [Solved]),
  Solved2 = solve_part_two(InputAsList),
  io:format("~nPart 2: ~p~n", [Solved2]).


%% By convention, I'll be putting puzzle inputs in priv/
read_input_file() ->
  {ok, Contents} = file:read_file(filename:join([code:priv_dir(day1), "input.txt"])),
  Split = binary:split(Contents, <<"\n">>, [global]),
  lists:filter(fun (X) -> X =/= <<"">> end, Split).


solve(InputAsList) ->
  Transformed = lists:map(fun (X) ->
    Base = list_to_integer(binary_to_list(X)),
    fuel_requirements_for_mass(Base)
  end, InputAsList),
  lists:sum(Transformed).


solve_part_two(InputAsList) ->
  Transformed = lists:map(fun (X) ->
    Base = list_to_integer(binary_to_list(X)),
    FirstFuel = fuel_requirements_for_mass(Base),
    fuel_requirements_for_fuel_itself(FirstFuel, 0)
  end, InputAsList),
  lists:sum(Transformed).


fuel_requirements_for_mass(Number) ->
  DivideByThree = Number / 3,
  RoundDown = math:floor(DivideByThree),
  RoundDown - 2.


fuel_requirements_for_fuel_itself(New, Accum) ->
  Additional = fuel_requirements_for_mass(New),
  case Additional of
      Y when Y =< 0 -> New + Accum;
      X -> fuel_requirements_for_fuel_itself(X, Accum + New)
  end.
