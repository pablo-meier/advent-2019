-module(day9_app).
-export([start/0]).

%% ERLANG TO THE RESCUE
%% Turns out that arrays are by default extendible (like Java ArrayLists) and
%% can be configured to have a default uninitialized. We also have big integers by
%% default. Dope.

-record(intcode_vm, {
    %% The actual machine instructions.
    intspace,
    %% The pointer to the instruction we're running.
    instruction_pointer=0,
    %% We read from these inputs. If the list is empty, we receive {input, Msg}.
    inputs=[],
    %% We store these outputs. If next_machine is not none, we send to next_machine instead.
    outputs=[],
    %% The relative base for relative mode params.
    relative_base=0,
    %% A PID of a machine we expect to receive our outputs.
    next_machine=none,
    %% The PID of the root process that expects the output of the whole shebang.
    orchestrator=none
}).


start() ->
    TestList = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99],
    TestProgram = #intcode_vm{intspace=array:from_list(TestList, 0)},
    #intcode_vm{outputs=T0} = run_intcode(TestProgram),
    io:format("Test output is ~p~n", [T0]),
    TestList = lists:reverse(T0),

    Test2 = [1102,34915192,34915192,7,4,7,99,0],
    TestProgram2 = #intcode_vm{intspace=array:from_list(Test2, 0)},
    #intcode_vm{outputs=O1} = run_intcode(TestProgram2),
    io:format("Outputs of large program are ~p~n", [O1]),

    Test3 = [104,1125899906842624,99],
    TestProgram3 = #intcode_vm{intspace=array:from_list(Test3, 0)},
    #intcode_vm{outputs=O2} = run_intcode(TestProgram3),
    io:format("Outputs of large program are ~p~n", [O2]),

    InputAsArray = read_input_file(),
    InputProgram = #intcode_vm{intspace=InputAsArray, inputs=[1]},
    #intcode_vm{outputs=[H|[]]} = run_intcode(InputProgram),
    io:format("~nPart 1: ~p~n", [H]),

    Input2 = #intcode_vm{intspace=InputAsArray, inputs=[2]},
    #intcode_vm{outputs=[H2|[]]} = run_intcode(Input2),
    io:format("~nPart 2: ~p~n", [H2]).


%% By convention, I'll be putting puzzle inputs in priv/
read_input_file() ->
    {ok, Contents} = file:read_file(filename:join([code:priv_dir(day9), "input.txt"])),
    Split = binary:split(Contents, [<<",">>, <<"\n">>], [global]),
    ExtraDelimitersOut = lists:filter(fun (X) -> X =/= <<"">> end, Split),
    AsInts = lists:map(fun(X) -> list_to_integer(binary_to_list(X)) end, ExtraDelimitersOut),
    array:from_list(AsInts, 0).


%% Runs the Intcode machine.
run_intcode(In = #intcode_vm{}) ->
    case parse_opcode(In) of
        {mult, P1, P2, P3} ->
            V1 = param_at_offset(In, 1, P1),
            V2 = param_at_offset(In, 2, P2),
            StoreAt = param_at_offset(In, 3, P3),
            NewArr = store(In, StoreAt, V1 * V2),
            NewPtr = inc_ptr(In, 4),
            run_intcode(In#intcode_vm{intspace=NewArr, instruction_pointer=NewPtr});
        {add, P1, P2, P3} ->
            V1 = param_at_offset(In, 1, P1),
            V2 = param_at_offset(In, 2, P2),
            StoreAt = param_at_offset(In, 3, P3),
            NewArr = store(In, StoreAt, V1 + V2),
            NewPtr = inc_ptr(In, 4),
            run_intcode(In#intcode_vm{intspace=NewArr, instruction_pointer=NewPtr});
        {load_input, P1} ->
            {Input, NewInputs} = case pop_input(In) of
                    {A, B} -> {A, B};
                    empty ->
                        receive
                            {input_value, X} -> {X, []}
                        end
                end,
            StoreAt = param_at_offset(In, 1, P1),
            NewArr = store(In, StoreAt, Input),
            NewPtr = inc_ptr(In, 2),
            run_intcode(In#intcode_vm{intspace=NewArr, instruction_pointer=NewPtr, inputs=NewInputs});
        {output, P1} ->
            ToOutput = param_at_offset(In, 1, P1),
            NewPtr = inc_ptr(In, 2),
            #intcode_vm{outputs=O, next_machine=NextPid} = In,
            send_if_nextpid(NextPid, ToOutput),
            run_intcode(In#intcode_vm{instruction_pointer=NewPtr, outputs=[ToOutput|O]});
        {jump_if_true, P1, P2} ->
            NewPtr = case param_at_offset(In, 1, P1) of
                0 -> inc_ptr(In, 3);
                _ -> param_at_offset(In, 2, P2)
            end,
            run_intcode(In#intcode_vm{instruction_pointer=NewPtr});
        {jump_if_false, P1, P2} ->
            NewPtr = case param_at_offset(In, 1, P1) of
                0 -> param_at_offset(In, 2, P2);
                _ -> inc_ptr(In, 3)
            end,
            run_intcode(In#intcode_vm{instruction_pointer=NewPtr});
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
            run_intcode(In#intcode_vm{intspace=NewArr, instruction_pointer=NewPtr});
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
            run_intcode(In#intcode_vm{intspace=NewArr, instruction_pointer=NewPtr});
        {adjust_relative_base, P1} ->
            V1 = param_at_offset(In, 1, P1),
            #intcode_vm{relative_base=OldRelative} = In,
            NewPtr = inc_ptr(In, 2),
            run_intcode(In#intcode_vm{relative_base=OldRelative + V1, instruction_pointer=NewPtr});
        terminate ->
            fire_result_if_has_orchestrator(In),
            In
    end.


param_at_offset(#intcode_vm{instruction_pointer=Ptr, intspace=Intspace, relative_base=RelativeBase}, Offset, Type) ->
    Value = array:get(Ptr + Offset, Intspace),
    case Type of
        position_mode -> array:get(Value, Intspace);
        immediate_mode -> Value;
        relative_mode -> array:get(Value + RelativeBase, Intspace);
        relative_mode_write -> Value + RelativeBase
    end.


store(#intcode_vm{intspace=Intspace}, Key, Val) ->
    array:set(Key, Val, Intspace).
inc_ptr(#intcode_vm{instruction_pointer=Ptr}, Val) -> Ptr + Val.

pop_input(#intcode_vm{inputs=[]}) -> empty;
pop_input(#intcode_vm{inputs=[H|T]}) -> {H, T}.

send_if_nextpid(none, _O) -> ok;
send_if_nextpid(NextPid, O) -> NextPid ! {input_value, O}.

fire_result_if_has_orchestrator(#intcode_vm{orchestrator=none}) -> ok;
fire_result_if_has_orchestrator(#intcode_vm{orchestrator=Orchestrator, outputs=[H|_]}) ->
    Orchestrator ! {final_output, H}.


parse_opcode(#intcode_vm{intspace=IntSpace, instruction_pointer=Ptr}) ->
    Opcode = array:get(Ptr, IntSpace),
    {Ones, Hundreds, Thousands, TenThousands} = break_down_opcode(Opcode),
    case Ones of
        1 -> {add, parse_param(Hundreds), parse_param(Thousands), parse_param_write(TenThousands)};
        2 -> {mult, parse_param(Hundreds), parse_param(Thousands), parse_param_write(TenThousands)};
        3 -> {load_input, parse_param_write(Hundreds)};
        4 -> {output, parse_param(Hundreds)};
        5 -> {jump_if_true, parse_param(Hundreds), parse_param(Thousands)};
        6 -> {jump_if_false, parse_param(Hundreds), parse_param(Thousands)};
        7 -> {less_than, parse_param(Hundreds), parse_param(Thousands), parse_param_write(TenThousands)};
        8 -> {equals, parse_param(Hundreds), parse_param(Thousands), parse_param_write(TenThousands)};
        9 -> {adjust_relative_base, parse_param(Hundreds)};
        99 -> terminate
    end.

break_down_opcode(Opcode) ->
    {Opcode rem 100,
     hundreds(Opcode rem 1000),
     thousands(Opcode rem 10000),
     ten_thousands(Opcode rem 100000)}.

parse_param(0) -> position_mode;
parse_param(1) -> immediate_mode;
parse_param(2) -> relative_mode.

%% This is super dumb, but: the instructions state the "position mode" means the number
%% is a *pointer* to the array value, _unless_ it's a write instruction, in which case
%% it represents the address (so, an immediate value!). So I'm having write params
%% be called "immediate_mode" even though they're technically position_mode so they write
%% to their parameters and not the values in them. Whatever.
%%
%% Relative mode has a similar thing, where when in a write param, it does something slightly
%% different than in a read one, so we'll just give it a new atom.
parse_param_write(0) -> immediate_mode;
parse_param_write(1) -> throw(immediate_mode_cant_write);
parse_param_write(2) -> relative_mode_write.

hundreds(X) when X >= 200 -> 2;
hundreds(X) when X >= 100 -> 1;
hundreds(_) -> 0.

thousands(X) when X >= 2000 -> 2;
thousands(X) when X >= 1000 -> 1;
thousands(_) -> 0.

ten_thousands(X) when X >= 20000 -> 2;
ten_thousands(X) when X >= 10000 -> 1;
ten_thousands(_) -> 0.
