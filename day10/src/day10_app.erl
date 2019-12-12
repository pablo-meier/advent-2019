-module(day10_app).
-export([start/0]).

start() ->
    Lines = read_input_file("input.txt"),
    Asteroids = lines_to_asteroid_list(Lines),
    VisibilityMap = line_of_sight_map_for_asteriods(Asteroids),
    [MostVisible|_] = lists:sort(fun max_asteroids/2, VisibilityMap),
    {TheMostVisible, NumVisible} = MostVisible,
    io:format("~nPart 1: ~p~n", [NumVisible]),

    Remaining = Asteroids -- [TheMostVisible],
    io:format("Calculating bearings"),
    WithBearings = [{X, bearing_between(TheMostVisible, X)} || X <- Remaining],
    io:format(" ~p~n", [WithBearings]),
    SortedByBearing = lists:sort(fun min_bearings/2, WithBearings),
    io:format(" ~p~n", [SortedByBearing]),
    {X, Y} = laser_cut(SortedByBearing, 200),
    io:format("~nPart 2: ~p~n", [(X * 100) + Y]).


read_input_file(Filename) ->
    {ok, Contents} = file:read_file(filename:join([code:priv_dir(day10), Filename])),
    Split = binary:split(Contents, [<<",">>, <<"\n">>], [global]),
    ExtraDelimitersOut = lists:filter(fun (X) -> X =/= <<"">> end, Split),
    lists:map(fun(Line) -> binary_to_list(Line) end, ExtraDelimitersOut).

%% Reversing the ordering function since we want max on the left.
max_asteroids({_, NumVisible1}, {_, NumVisible2}) -> NumVisible2 =< NumVisible1.

%% Return a set of the points as {row, col}
lines_to_asteroid_list(L) -> lines_to_asteroid_list_accum(L, 0, []).

lines_to_asteroid_list_accum([], _, Accum) -> lists:flatten(Accum);
lines_to_asteroid_list_accum([H|T], Row, Accum) ->
    LineList = parse_line(H, Row, 0, []),
    lines_to_asteroid_list_accum(T, Row + 1, [LineList|Accum]).


parse_line([], _, _, Accum) -> Accum;
parse_line([$.|T], Row, Col, Accum) -> parse_line(T, Row, Col + 1, Accum);
parse_line([$#|T], Row, Col, Accum) -> parse_line(T, Row, Col + 1, [{Row, Col}|Accum]).


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


%% For every asteroid in the list, how many others are in "line of sight"? Performing super
%% naively, since parallelism. This is closure abuse.
line_of_sight_map_for_asteriods(AsteroidList) ->
    parallel_map(fun (Asteroid) ->
            WithoutMe = sets:del_element(Asteroid, sets:from_list(AsteroidList)),
            LineOfSights = lists:filter(fun (X) ->
                    WithoutUs = sets:del_element(X, sets:del_element(X, WithoutMe)),
                    has_line_of_sight(X, Asteroid, sets:to_list(WithoutUs))
                end, sets:to_list(WithoutMe)),

            {Asteroid, length(LineOfSights)}
    end, AsteroidList).


has_line_of_sight(A, B, TheRest) ->
    HasBlockers = lists:any(fun (C) -> is_between(A,C,B) end, TheRest),
    not HasBlockers.


is_between(A, C, B) -> within_epsilon(distance(A,C) + distance(C,B), distance(A, B)).

distance({AY, AX},{BY, BX}) -> math:sqrt(((AX - BX) * (AX - BX)) + ((AY - BY) * (AY - BY))).
within_epsilon(A, B) -> abs(A - B) < 0.00000025.


%%%%%%%% PART 2
%%% For this, we'll take the most-connected point, calculate the bearing between it and the rest of the asteroids,
%%% sort the list entirely by the bearing, then cycle around it until we hit the 200th.

% Thanks SO https://math.stackexchange.com/questions/1596513/find-the-bearing-angle-between-two-points-in-a-2d-space
bearing_between({AX, AY}, {BX, BY}) ->
    ThetaPrime = math:atan2(BX - AX, BY - AY),
    _ReturnRadians = case ThetaPrime > 0 of
        true -> ThetaPrime;
        false -> (math:pi() * 2) + ThetaPrime
    end.
    % ,
    % %% Convert to degrees
    % ReturnRadians * 57.2957795130823209.


min_bearings({_, Bearing1}, {_, Bearing2}) -> Bearing1 =< Bearing2.

%% Run through the list, returning after you've hit Target.
laser_cut(Remaining, Target) ->
    laser_cut_recursion(Remaining, [], Target, 1).


laser_cut_recursion([], TheRest, Target, Curr) -> io:format("resetting! ~p ~p ~p~n", [TheRest, Target, Curr]), laser_cut_recursion(lists:reverse(TheRest), [], Target, Curr);
laser_cut_recursion([{A,_}], _, Target, Target) -> A;

%% Cut the first in a chain: get a new tail, cons the rest for a subsequent cycle.
laser_cut_recursion(Lst = [{_, Bearing1}|[{_, Bearing1}|_]], TheRest, Target, Curr) ->
    case Target =:= Curr of
        true -> io:format("okay lol were here"), erlang:halt();
        false -> ok
    end,
    {NewTail, NewTheRest} = zoom_while_equal(Lst, TheRest),
    laser_cut_recursion(NewTail, NewTheRest, Target, Curr + 1);

laser_cut_recursion([_|T], TheRest, Target, Curr) ->
    case Target =:= Curr of
        true -> io:format("okay lol were here"), erlang:halt();
        false -> ok
    end,
    laser_cut_recursion(T, TheRest, Target, Curr + 1).


zoom_while_equal([H|T], TheRest) ->
    zoom_while_equal_recursion(H, T, TheRest).

zoom_while_equal_recursion({Base, B1}, [{Point, B1}|T], TheRest) ->
    zoom_while_equal_recursion({Base, B1}, T, [{Point, B1}|TheRest]);
zoom_while_equal_recursion(_, NonMatchTail, TheRest) -> {NonMatchTail, TheRest}.
