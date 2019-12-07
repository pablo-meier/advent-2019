-module(day5_app).
-export([start/0]).

%% Okay, we're adding inputs and outputs to this puppy.
-record(intcode_vm, {intspace, instruction_pointer, inputs, outputs}).


start() ->
    InputAsArray = read_input_file(),
    InitialVm = #intcode_vm{intspace=InputAsArray, instruction_pointer=0, inputs=[1], outputs=[]},
    #intcode_vm{outputs=Outputs} = solve(InitialVm),
    [Solved|Zeroes] = Outputs,
    true = lists:all(fun(X) -> X =:= 0 end, Zeroes),
    io:format("~nPart 1: ~p~n", [Solved]),

    %% Equals
    test_run([3,9,8,9,10,9,4,9,99,-1,8], 8, 1),
    test_run([3,9,8,9,10,9,4,9,99,-1,8], 9, 0),

    %% Less than
    test_run([3,9,7,9,10,9,4,9,99,-1,8], 7, 1),
    test_run([3,9,7,9,10,9,4,9,99,-1,8], 8, 0),

    %% Equals (Immediate)
    test_run([3,3,1108,-1,8,3,4,3,99], 8, 1),
    test_run([3,3,1108,-1,8,3,4,3,99], 9, 0),

    %% Less Than (Immediate)
    test_run([3,3,1107,-1,8,3,4,3,99], 7, 1),
    test_run([3,3,1107,-1,8,3,4,3,99], 8, 0),

    %% Jump test 1 (Position mode)
    test_run([3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9], 0, 0),
    test_run([3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9], 8, 1),

    %% Jump test 2 (Immediate mode)
    test_run([3,3,1105,-1,9,1101,0,0,12,4,12,99,1], 0, 0),
    test_run([3,3,1105,-1,9,1101,0,0,12,4,12,99,1], 8, 1),

    Part2Vm = #intcode_vm{intspace=InputAsArray, instruction_pointer=0, inputs=[5], outputs=[]},
    #intcode_vm{outputs=[Solved2|[]]} = solve(Part2Vm),
    io:format("Part 2: ~p~n", [Solved2]).


%% By convention, I'll be putting puzzle inputs in priv/
read_input_file() ->
    {ok, Contents} = file:read_file(filename:join([code:priv_dir(day5), "input.txt"])),
    Split = binary:split(Contents, [<<",">>, <<"\n">>], [global]),
    ExtraDelimitersOut = lists:filter(fun (X) -> X =/= <<"">> end, Split),
    AsInts = lists:map(fun(X) -> list_to_integer(binary_to_list(X)) end, ExtraDelimitersOut),
    array:from_list(AsInts).


%% Runs the Intcode machine.
solve(In = #intcode_vm{}) ->
    case parse_opcode(In) of
        {mult, P1, P2, P3} ->
            V1 = param_at_offset(In, 1, P1),
            V2 = param_at_offset(In, 2, P2),
            StoreAt = param_at_offset(In, 3, P3),
            NewArr = store(In, StoreAt, V1 * V2),
            NewPtr = inc_ptr(In, 4),
            solve(In#intcode_vm{intspace=NewArr, instruction_pointer=NewPtr});
        {add, P1, P2, P3} ->
            V1 = param_at_offset(In, 1, P1),
            V2 = param_at_offset(In, 2, P2),
            StoreAt = param_at_offset(In, 3, P3),
            NewArr = store(In, StoreAt, V1 + V2),
            NewPtr = inc_ptr(In, 4),
            solve(In#intcode_vm{intspace=NewArr, instruction_pointer=NewPtr});
        {load_input, P1} ->
            {Input, NewInputs} = pop_input(In),
            StoreAt = param_at_offset(In, 1, P1),
            NewArr = store(In, StoreAt, Input),
            NewPtr = inc_ptr(In, 2),
            solve(In#intcode_vm{intspace=NewArr, instruction_pointer=NewPtr, inputs=NewInputs});
        {output, P1} ->
            ToOutput = param_at_offset(In, 1, P1),
            NewPtr = inc_ptr(In, 2),
            #intcode_vm{outputs=O} = In,
            solve(In#intcode_vm{instruction_pointer=NewPtr, outputs=[ToOutput|O]});
        {jump_if_true, P1, P2} ->
            NewPtr = case param_at_offset(In, 1, P1) of
                0 -> inc_ptr(In, 3);
                _ -> param_at_offset(In, 2, P2)
            end,
            solve(In#intcode_vm{instruction_pointer=NewPtr});
        {jump_if_false, P1, P2} ->
            NewPtr = case param_at_offset(In, 1, P1) of
                0 -> param_at_offset(In, 2, P2);
                _ -> inc_ptr(In, 3)
            end,
            solve(In#intcode_vm{instruction_pointer=NewPtr});
        {less_than, P1, P2, P3} ->
            V1 = param_at_offset(In, 1, P1),
            V2 = param_at_offset(In, 2, P2),
            StoreAt = param_at_offset(In, 3, P3),
            StoreVal = case V1 < V2 of
                true -> 1;
                false -> 0
            end,
            NewArr = store(In, StoreAt, StoreVal),
            NewPtr = inc_ptr(In, 4),
            solve(In#intcode_vm{intspace=NewArr, instruction_pointer=NewPtr});
        {equals, P1, P2, P3} ->
            V1 = param_at_offset(In, 1, P1),
            V2 = param_at_offset(In, 2, P2),
            StoreAt = param_at_offset(In, 3, P3),
            StoreVal = case V1 =:= V2 of
                true -> 1;
                false -> 0
            end,
            NewArr = store(In, StoreAt, StoreVal),
            NewPtr = inc_ptr(In, 4),
            solve(In#intcode_vm{intspace=NewArr, instruction_pointer=NewPtr});
        terminate -> In
    end.


param_at_offset(#intcode_vm{instruction_pointer=Ptr, intspace=Intspace}, Offset, Type) ->
    Value = array:get(Ptr + Offset, Intspace),
    case Type of
        position_mode -> array:get(Value, Intspace);
        immediate_mode -> Value
    end.


store(#intcode_vm{intspace=Intspace}, Key, Val) ->
    array:set(Key, Val, Intspace).
inc_ptr(#intcode_vm{instruction_pointer=Ptr}, Val) -> Ptr + Val.
pop_input(#intcode_vm{inputs=[H|T]}) -> {H, T}.


%% This is super dumb, but: the instructions state the "position mode" means the number
%% is a *pointer* to the array value, _unless_ it's a write instruction, in which case
%% it represents the address (so, an immediate value!). So I'm having write params
%% be called "immediate_mode" even though they're technically position_mode so they write
%% to their parameters and not the values in them. Whatever.
parse_opcode(#intcode_vm{intspace=IntSpace, instruction_pointer=Ptr}) ->
    Opcode = array:get(Ptr, IntSpace),
    {Ones, Hundreds, Thousands, _TenThousands} = break_down_opcode(Opcode),
    case Ones of
        1 -> {add, parse_param(Hundreds), parse_param(Thousands), immediate_mode};
        2 -> {mult, parse_param(Hundreds), parse_param(Thousands), immediate_mode};
        3 -> {load_input, immediate_mode};
        4 -> {output, parse_param(Hundreds)};
        5 -> {jump_if_true, parse_param(Hundreds), parse_param(Thousands)};
        6 -> {jump_if_false, parse_param(Hundreds), parse_param(Thousands)};
        7 -> {less_than, parse_param(Hundreds), parse_param(Thousands), immediate_mode}; % parse_param(TenThousands)}; % immediate_mode};
        8 -> {equals, parse_param(Hundreds), parse_param(Thousands), immediate_mode}; % parse_param(TenThousands)}; % immediate_mode};
        99 -> terminate
    end.

break_down_opcode(Opcode) ->
    {Opcode rem 100,
     one_or_zero((Opcode rem 1000) >= 100),
     one_or_zero((Opcode rem 10000) >= 1000),
     one_or_zero((Opcode rem 100000) >= 10000)}.

parse_param(0) -> position_mode;
parse_param(_) -> immediate_mode.

one_or_zero(true) -> 1;
one_or_zero(false) -> 0.


%%%%%%%%%%%%% TEST THE DAMN OPCODES LOL
test_run(Code, Input, ExpectedOutput) ->
    #intcode_vm{outputs=[ExpectedOutput]} = solve(#intcode_vm{
        intspace=array:from_list(Code),
        instruction_pointer=0,
        inputs=[Input],
        outputs=[]
    }).
