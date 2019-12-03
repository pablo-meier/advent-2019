-module(day2_app).
-export([start/0]).
-record(intcode_vm, {intspace, instruction_pointer}).

start() ->
    InputAsArray = read_input_file(),
	Part1Solved = intcode_value_for(12, 2, InputAsArray),
    io:format("~nPart 1: ~p~n", [Part1Solved]),
	Part2Solved = meta_intcode_recursion(0, 0, 19690720, InputAsArray),
    io:format("Part 2: ~p~n", [Part2Solved]).


%% By convention, I'll be putting puzzle inputs in priv/
read_input_file() ->
    {ok, Contents} = file:read_file(filename:join([code:priv_dir(day2), "input.txt"])),
    Split = binary:split(Contents, [<<",">>, <<"\n">>], [global]),
    ExtraDelimitersOut = lists:filter(fun (X) -> X =/= <<"">> end, Split),
    AsInts = lists:map(fun(X) -> list_to_integer(binary_to_list(X)) end, ExtraDelimitersOut),
    array:from_list(AsInts).


intcode_value_for(Noun, Verb, InitialArray) ->
	Adjusted1 = array:set(1, Noun, InitialArray),
	Adjusted2 = array:set(2, Verb, Adjusted1),
    #intcode_vm{intspace=Solved} = solve(make_intcode(Adjusted2)),
	array:get(0, Solved).


%% Runs the Intcode machine.
solve(In = #intcode_vm{intspace=IntSpace, instruction_pointer=Ptr}) ->
    Opcode = array:get(Ptr, IntSpace),
    case Opcode of
        1 ->
            Index1 = array:get(Ptr + 1, IntSpace),
            Index2 = array:get(Ptr + 2, IntSpace),
            Index3 = array:get(Ptr + 3, IntSpace),
            NewArr = add_operation(IntSpace, Index1, Index2, Index3),
            solve(#intcode_vm{intspace=NewArr, instruction_pointer=Ptr + 4});
        2 ->
            Index1 = array:get(Ptr + 1, IntSpace),
            Index2 = array:get(Ptr + 2, IntSpace),
            Index3 = array:get(Ptr + 3, IntSpace),
            NewArr = mult_operation(IntSpace, Index1, Index2, Index3),
            solve(#intcode_vm{intspace=NewArr, instruction_pointer=Ptr + 4});
        99 -> In
    end.


add_operation(Arr, Idx1, Idx2, Idx3) ->
    Elem1 = array:get(Idx1, Arr),
    Elem2 = array:get(Idx2, Arr),
    array:set(Idx3, Elem1 + Elem2, Arr).


mult_operation(Arr, Idx1, Idx2, Idx3) ->
    Elem1 = array:get(Idx1, Arr),
    Elem2 = array:get(Idx2, Arr),
    array:set(Idx3, Elem1 * Elem2, Arr).


make_intcode(IntArray) ->
    #intcode_vm{intspace=IntArray, instruction_pointer=0}.


meta_intcode_recursion(Noun, Verb, Target, Arr) ->
    case intcode_value_for(Noun, Verb, Arr) of
		Target -> (Noun * 100) + Verb;
		_ ->
			case Verb of
				99 -> meta_intcode_recursion(Noun + 1, 0, Target, Arr);
				_ -> meta_intcode_recursion(Noun, Verb + 1, Target, Arr)
			end
    end.
