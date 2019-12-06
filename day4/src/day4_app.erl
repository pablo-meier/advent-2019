-module(day4_app).
-export([start/0]).


start() ->
    {Lower, Upper} = read_input_file(),
    io:format("~n~p-~p~n", [Lower, Upper]),
    %% Quick tests.
    true = meets_criteria_part_1("111111"),
    false = meets_criteria_part_1("223450"),
    false = meets_criteria_part_1("123789"),
    NumQualifying = brute_forcer(Lower, Upper, 0, fun meets_criteria_part_1/1),
    io:format("~nPart 1: ~p~n", [NumQualifying]),

    true = meets_criteria_part_2("444455"),
    true = meets_criteria_part_2("333445"),
    true = meets_criteria_part_2("112233"),
    false = meets_criteria_part_2("221133"),
    false = meets_criteria_part_2("123444"),
    true = meets_criteria_part_2("111122"),
    NumQualifyingPartTwo = brute_forcer(Lower, Upper, 0, fun meets_criteria_part_2/1),
    io:format("Part 2: ~p~n", [NumQualifyingPartTwo]).


%% By convention, I'll be putting puzzle inputs in priv/
read_input_file() ->
    {ok, Contents} = file:read_file(filename:join([code:priv_dir(day4), "input.txt"])),
    Lines = binary:split(Contents, [<<"-">>,<<"\n">>], [global]),
    NoEmpties = lists:filter(fun (X) -> X =/= <<"">> end, Lines),
    {list_to_integer(binary_to_list(hd(NoEmpties))),
     list_to_integer(binary_to_list(hd(tl(NoEmpties))))}.


meets_criteria_part_1(Candidate) ->
    meet_criteria_recursion(Candidate, false).

%% Boolean is whether or not we see adjacent digits, guards ensure ascending digits
%% are strictly ascending.

%% If we're out of characters or have only one character left and already met
%% criteria, then Yahtzee.
meet_criteria_recursion([], true) -> true;
meet_criteria_recursion([_|[]], true) -> true;

%% If it has two adjacent characters that are identical, then set that bit to true.
meet_criteria_recursion([A|[A|T]], _) -> meet_criteria_recursion([A|T], true);
%% If it has two adjacent characters that aren't identical, ensure they are ascending.
meet_criteria_recursion([A|[B|T]], X) when A =< B -> meet_criteria_recursion([B|T], X);
%% Else.
meet_criteria_recursion(_, _) -> false.



meets_criteria_part_2(Candidate) ->
    meet_criteria_2_recursion(Candidate, false).

%% Finishing conditions remain the same.
meet_criteria_2_recursion([], true) -> true;
meet_criteria_2_recursion([_|[]], true) -> true;

%% If it has two adjacent characters, we have to be a little more creative. We'll do
%% **yet another** recursion method to calculate the length of the adjacent characters,
%% return a new tail, and if it's two, then we've met the criteria.
meet_criteria_2_recursion([A|[A|T]], Truthy) ->
    case length_of_subsection(A, [A|[A|T]]) of
        {2, NewTail}  -> meet_criteria_2_recursion(NewTail, true);
        {_, NewTail}  -> meet_criteria_2_recursion(NewTail, Truthy)
    end;

%% If it has two adjacent characters that aren't identical, ensure they are ascending.
meet_criteria_2_recursion([A|[B|T]], X) when A =< B -> meet_criteria_2_recursion([B|T], X);

meet_criteria_2_recursion(_, _) -> false.


length_of_subsection(Base, Rst) -> length_of_subsection_recursion(Base, Rst, 0).
length_of_subsection_recursion(_, [], Accum) -> {Accum, []};
length_of_subsection_recursion(Base, [Base|T], Accum) -> length_of_subsection_recursion(Base, T, Accum + 1);

%% Gotta have one char of the subsequence there if possible to ensure the guards follow subsequent clause.
length_of_subsection_recursion(Base, Tail, Accum) -> {Accum, [Base|Tail]}.


brute_forcer(Max, Max, Total, _) -> Total;
brute_forcer(Curr, Max, Total, Fun) ->
    NewTotal = case Fun(integer_to_list(Curr)) of
        true -> Total + 1;
        false -> Total
    end,
    brute_forcer(Curr + 1, Max, NewTotal, Fun).
