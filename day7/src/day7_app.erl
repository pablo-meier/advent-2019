-module(day7_app).
-export([start/0]).

-record(intcode_vm, {
    %% The actual machine instructions.
    intspace,
    %% The pointer to the instruction we're running.
    instruction_pointer,
    %% We read from these inputs. If the list is empty, we receive {input, Msg}.
    inputs,
    %% We store these outputs. If next_machine is not none, we send to next_machine instead.
    outputs,
    %% A PID of a machine we expect to receive our outputs.
    next_machine=none,
    %% The PID of the root process that expects the output of the whole shebang.
    orchestrator=none
}).


start() ->
    PhaseSettings = make_phase_settings([0,1,2,3,4]),

    43210 = sequence_run_on([4,3,2,1,0], array:from_list([3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0])),
    43210 = solve_for_instructions(array:from_list([3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]), PhaseSettings, fun sequence_run_on/2),
    54321 = sequence_run_on([0,1,2,3,4], array:from_list([3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0])),

    InputAsArray = read_input_file(),

    HighestOutput = solve_for_instructions(InputAsArray, PhaseSettings, fun sequence_run_on/2),
    io:format("~nPart 1: ~p~n", [HighestOutput]),

    NextPhaseSettings = make_phase_settings([5,6,7,8,9]),
    139629729 = solve_for_instructions(
        array:from_list([3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]),
        NextPhaseSettings,
        fun run_interlinked/2
    ),

    18216 = solve_for_instructions(
        array:from_list([3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,
                         55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]),
        NextPhaseSettings,
        fun run_interlinked/2
    ),
    Part2Highest = solve_for_instructions(InputAsArray, NextPhaseSettings, fun run_interlinked/2),
    io:format("Part 2: ~p~n", [Part2Highest]).


solve_for_instructions(InputAsArray, PhaseSettings, Applicator) ->
    AllResults = parallel_map(fun (Phases) -> Applicator(Phases, InputAsArray) end, PhaseSettings),
    lists:max(AllResults).


%% Produces all {a,b,c,d,e} | a,b,c,d,e \in {0..4}
make_phase_settings(Domain) ->
    [ [A, B, C, D, E] ||
      A <- Domain,
      B <- Domain,
      C <- Domain,
      D <- Domain,
      E <- Domain,
      A =/= B, A =/= C, A =/= D, A =/= E,
      B =/= C, B =/= D, B =/= E,
      C =/= D, C =/= E,
      D =/= E
    ].


% Erlangers gotta do it. Doesn't preserve order.
parallel_map(Fun, Lst) ->
    NumResults = length(Lst),
    lists:foreach(fun (Elem) -> self() ! {ok, Fun(Elem)} end, Lst),
    receive_all([], NumResults, 0).

receive_all(Accum, Total, Total) -> Accum;
receive_all(Accum, Total, SoFar) ->
    receive
        {ok, Val} ->
            receive_all([Val|Accum], Total, SoFar + 1)
    end.


%% Per the previous example, run the code on machines, Human Centiped-ing their output with the
%% phase setting, returning the last one.
sequence_run_on(PhaseSettingList, Instructions) ->
    sequence_run_on_recursion(PhaseSettingList, 0, Instructions).


sequence_run_on_recursion([], Result, _) -> Result;
sequence_run_on_recursion([H|T], PrevResult, Instructions) ->
    InputVm = #intcode_vm{
        intspace=Instructions,
        instruction_pointer=0,
        inputs=[H, PrevResult],
        outputs=[]
    },
    #intcode_vm{outputs=[NewOutput|[]]} = run_intcode(InputVm),
    sequence_run_on_recursion(T, NewOutput, Instructions).


%% Bahahahaha for part two were are FUCKED people.
%%
%% Okay, main thing that sucks is that we need inputs and outputs to
%% correspond. We'll use Erlang's mailbox `receive` blocking when awaiting an
%% input and if you're currently empty, and send() on an output. We'll need a toplevel
%% orchestrator process to send PIDs before they all start.
%%
%% Not going to go full-on gen_server, but lord knows I probably should lol.
run_interlinked(PhaseSettingsList, Instructions) ->
    %% Give each Intcode VM the instructions and their phase setting, gather PIDs.
    %% Send PIDs to each for who their next one is, they assign.
    %% Send a start message. Send the first input of 0.
    %% Receive the response.
    ThePids = lists:map(fun (PhaseSetting) ->
        spawn(fun () -> circular_intcode(PhaseSetting, Instructions) end)
    end, PhaseSettingsList),

    [FirstPid, SecondPid, ThirdPid, FourthPid, FifthPid] = ThePids,
    FirstPid ! {next_pid, SecondPid},
    SecondPid ! {next_pid, ThirdPid},
    ThirdPid ! {next_pid, FourthPid},
    FourthPid ! {next_pid, FifthPid},
    FifthPid ! {next_pid, FirstPid},

    FifthPid ! {root_process, self()},

    lists:foreach(fun (Pid) -> Pid ! start end, ThePids),
    FirstPid ! {input_value, 0},
    receive
        {final_output, X} -> X
    end.


circular_intcode(PhaseSetting, Instructions) ->
    InputVm = #intcode_vm{
        intspace=Instructions,
        instruction_pointer=0,
        inputs=[PhaseSetting],
        outputs=[]
    },
    holding_pattern(InputVm).


holding_pattern(In) ->
    receive
        {next_pid, NextPid} -> holding_pattern(In#intcode_vm{next_machine=NextPid});
        {root_process, Root} -> holding_pattern(In#intcode_vm{orchestrator=Root});
        start -> run_intcode(In)
    end.


%% By convention, I'll be putting puzzle inputs in priv/
read_input_file() ->
    {ok, Contents} = file:read_file(filename:join([code:priv_dir(day7), "input.txt"])),
    Split = binary:split(Contents, [<<",">>, <<"\n">>], [global]),
    ExtraDelimitersOut = lists:filter(fun (X) -> X =/= <<"">> end, Split),
    AsInts = lists:map(fun(X) -> list_to_integer(binary_to_list(X)) end, ExtraDelimitersOut),
    array:from_list(AsInts).


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
        terminate ->
            fire_result_if_has_orchestrator(In),
            In
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

pop_input(#intcode_vm{inputs=[]}) -> empty;
pop_input(#intcode_vm{inputs=[H|T]}) -> {H, T}.

send_if_nextpid(none, _O) -> ok;
send_if_nextpid(NextPid, O) -> NextPid ! {input_value, O}.

fire_result_if_has_orchestrator(#intcode_vm{orchestrator=none}) -> ok;
fire_result_if_has_orchestrator(#intcode_vm{orchestrator=Orchestrator, outputs=[H|_]}) ->
    Orchestrator ! {final_output, H}.


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
