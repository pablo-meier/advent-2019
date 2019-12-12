-module(day10_app).
-export([start/0]).

-record(comparison, {
    point,
    bearing,
    distance
}).

start() ->
    Lines = read_input_file("input.txt"),
    Asteroids = lines_to_asteroid_list(Lines),
    AsteroidSet = sets:from_list(Asteroids),

    Pairs = pairs_of_asteroids(Asteroids),
    IntersectionsPid = spawn(fun() -> intersections_holder(maps:new()) end),
    parallel_map(fun ({P1, P2}) ->
        TheRest = sets:to_list(sets:del_element(P1, sets:del_element(P2, AsteroidSet))),
        HasBlockers = lists:any(fun (C) -> is_between(P1,C,P2) end, TheRest),
        IntersectionsPid ! {set_can_see, P1, P2, not HasBlockers}
    end, Pairs),

    VisibilityMap = parallel_map(fun (X) ->
        TheRest = sets:del_element(X, AsteroidSet),
        SeeList = lists:map(fun (Y) ->
            IntersectionsPid ! {get_can_see, X, Y, self()},
            receive
                {ok, true} -> 1;
                {ok, false} -> 0
            end
        end, sets:to_list(TheRest)),
        {X, lists:sum(SeeList)}
    end, Asteroids),
    IntersectionsPid ! kill,
    [MostVisible|_] = lists:sort(fun max_asteroids/2, VisibilityMap),
    {TheMostVisible, NumVisible} = MostVisible,
    io:format("~nBest visibility asteroid is ~p~n", [TheMostVisible]),
    io:format("Part 1: ~p~n", [NumVisible]),

    Remaining = sets:to_list(sets:del_element(TheMostVisible, AsteroidSet)),
    WithBearings = [#comparison{
                       point=X,
                       bearing=bearing_between(TheMostVisible, X),
                       distance=distance(TheMostVisible, X)} || X <- Remaining],
    FauxMultimap = faux_multimap(WithBearings),
    SortedByBearing = lists:sort(fun min_bearings/2, FauxMultimap),
    #comparison{point={X, Y}} = laser_cut(SortedByBearing, 200),
    io:format("Part 2: ~p~n", [(X * 100) + Y]).


%% Bearing is your key, list of {element, distance{} are the value.
faux_multimap(Comparisons) ->
    AsMap = lists:foldl(fun (C = #comparison{bearing=B}, Map) ->
        case maps:get(B, Map, not_yet) of
            not_yet -> maps:put(B, [C], Map);
            Lst -> maps:put(B, [C|Lst], Map)
        end
    end, maps:new(), Comparisons),
    maps:to_list(maps:map(fun (_, V) -> lists:sort(fun sort_by_distance/2, V) end, AsMap)).


sort_by_distance(#comparison{distance=D1}, #comparison{distance=D2}) ->
    D1 =< D2.


intersections_holder(Map) ->
    receive
        {set_can_see, P1, P2, Value} ->
            intersections_holder(maps:put({P1, P2}, Value, Map));
        {get_can_see, P1, P2, Pid} ->
            Val = case maps:get({P1, P2}, Map, otherside) of
                otherside -> maps:get({P2, P1}, Map);
                X -> X
            end,
            Pid ! {ok, Val},
            intersections_holder(Map);
        kill -> ok
    end.


pairs_of_asteroids(List) ->
    Initial = [{X1, X2} || X1 <- List, X2 <- List, X1 /= X2],
    sets:to_list(lists:foldl(fun ({A, B}, Set) ->
            case sets:is_element({B, A}, Set) of
                true -> Set;
                false -> sets:add_element({A, B}, Set)
            end
        end, sets:new(), Initial)).


%% Reversing the ordering function since we want max on the left.
max_asteroids({_, NumVisible1}, {_, NumVisible2}) -> NumVisible2 =< NumVisible1.


read_input_file(Filename) ->
    {ok, Contents} = file:read_file(filename:join([code:priv_dir(day10), Filename])),
    Split = binary:split(Contents, [<<",">>, <<"\n">>], [global]),
    ExtraDelimitersOut = lists:filter(fun (X) -> X =/= <<"">> end, Split),
    lists:map(fun(Line) -> binary_to_list(Line) end, ExtraDelimitersOut).

%% Return a set of the points as {row, col}
lines_to_asteroid_list(L) -> lines_to_asteroid_list_accum(L, 0, []).

lines_to_asteroid_list_accum([], _, Accum) -> lists:flatten(Accum);
lines_to_asteroid_list_accum([H|T], Row, Accum) ->
    LineList = parse_line(H, Row, 0, []),
    lines_to_asteroid_list_accum(T, Row + 1, [LineList|Accum]).

parse_line([], _, _, Accum) -> Accum;
parse_line([$.|T], Row, Col, Accum) -> parse_line(T, Row, Col + 1, Accum);
parse_line([$#|T], Row, Col, Accum) -> parse_line(T, Row, Col + 1, [{Col, Row}|Accum]).


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


is_between(A, C, B) -> within_epsilon(distance(A,C) + distance(C,B), distance(A, B)).

distance({AY, AX},{BY, BX}) -> math:sqrt(((AX - BX) * (AX - BX)) + ((AY - BY) * (AY - BY))).
within_epsilon(A, B) -> abs(A - B) < 0.00000025.


%%%%%%%% PART 2
%%% For this, we'll take the most-connected point, calculate the bearing between it and the rest of the asteroids,
%%% sort the list entirely by the bearing, then cycle around it until we hit the 200th.

% Thanks SO https://math.stackexchange.com/questions/1596513/find-the-bearing-angle-between-two-points-in-a-2d-space
bearing_between({AX, AY}, {BX, BY}) ->
    ThetaPrime = math:atan2(BX - AX, AY - BY),
    _ReturnRadians = case ThetaPrime >= 0 of
        true -> ThetaPrime;
        false -> (math:pi() * 2) + ThetaPrime
    end.

min_bearings({Bearing1, _}, {Bearing2, _}) -> Bearing1 =< Bearing2.

%% Run through the list, returning after you've hit Target.
laser_cut(Remaining, Target) ->
    laser_cut_recursion(Remaining, [], Target, 1).


laser_cut_recursion([], TheRest, Target, Curr) ->
    laser_cut_recursion(lists:reverse(TheRest), [], Target, Curr);

%% Cut the first in a chain: get a new tail, cons the rest for a subsequent cycle.
laser_cut_recursion([Entry|Tail], TheRest, Target, Curr) ->
    {B, [H|T2]} = Entry,
    NewTheRest = case T2 of
        [] -> TheRest;
        _ -> [{B, T2}|TheRest]
    end,
    case Target =:= Curr of
        true -> H;
        false -> laser_cut_recursion(Tail, NewTheRest, Target, Curr + 1)
    end.
