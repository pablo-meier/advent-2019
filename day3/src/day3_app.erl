-module(day3_app).
-export([start/0]).

-record(point, {x, y}).
-record(draw_on, {location, point_set, matches}).


start() ->
    {FirstWire, SecondWire} = read_input_file(),
    EmptyDrawStruct = #draw_on{location=#point{x=0, y=0}, point_set=sets:new(), matches=[]},
    #draw_on{point_set=PS} = draw_wire(EmptyDrawStruct, FirstWire),
    SecondWireStruct = #draw_on{location=#point{x=0, y=0}, point_set=PS, matches=[]},
    #draw_on{matches=Matches} = draw_wire(SecondWireStruct, SecondWire),
    NoOrigin = lists:filter(fun (P) -> P =/= #point{x=0,y=0} end, Matches),
    Part1Solved = lists:min(lists:map(fun manhattan_distance/1, NoOrigin)),
    io:format("~nPart 1: ~p~n", [Part1Solved]),

    EmptyForPart2 = #draw_on{location=#point{x=0, y=0}, point_set=maps:new(), matches=[]},
    #draw_on{point_set=PS2} = draw_wire_step_measure(EmptyForPart2, FirstWire, 0, first),
    SecondWireWithSteps = #draw_on{location=#point{x=0, y=0}, point_set=PS2, matches=[]},
    #draw_on{matches=MatchesWithSteps} = draw_wire_step_measure(SecondWireWithSteps, SecondWire, 0, second),
    EliminateOrigin = lists:filter(fun (P) -> P =/= 0 end, MatchesWithSteps),
    Part2Solved = lists:min(EliminateOrigin),
    io:format("Part 2: ~p~n", [Part2Solved]).


%% By convention, I'll be putting puzzle inputs in priv/
read_input_file() ->
    {ok, Contents} = file:read_file(filename:join([code:priv_dir(day3), "input.txt"])),
    TwoLines = binary:split(Contents, [<<"\n">>], [global]),
    [FirstWireSpec|[SecondWireSpec|[]]] = lists:filter(fun (X) -> X =/= <<"">> end, TwoLines),
    {parse_wire(FirstWireSpec), parse_wire(SecondWireSpec)}.

parse_wire(Spec) ->
    lists:map(fun parse_token/1, binary:split(Spec, [<<",">>], [global])).

parse_token(<<"U", Amount/binary>>) -> {up, list_to_integer(binary_to_list(Amount))};
parse_token(<<"R", Amount/binary>>) -> {right, list_to_integer(binary_to_list(Amount))};
parse_token(<<"D", Amount/binary>>) -> {down, list_to_integer(binary_to_list(Amount))};
parse_token(<<"L", Amount/binary>>) -> {left, list_to_integer(binary_to_list(Amount))}.

%%%%%% PART1

%% To "draw" the wire, we add points to a set. If we cross a point that's been drawn on,
%% we add it to the matches list.
draw_wire(D, []) -> D;
draw_wire(#draw_on{location=InitialPoint, point_set=PointSet, matches=Matches}, [H|T]) ->
    {EndPoint, NewPointSet, NewMatches} = draw_component(H, InitialPoint, PointSet, Matches),
    draw_wire(#draw_on{location=EndPoint, point_set=NewPointSet, matches=NewMatches}, T).

draw_component({_, 0}, Point, Set, Match) -> {Point, Set, Match};
draw_component({Op, N}, Location, Set, Matches) ->
    {NewSet, NewMatches} = case sets:is_element(Location, Set) of
        true ->
            {Set, [Location|Matches]};
        false ->
            {sets:add_element(Location, Set), Matches}
    end,
    NewPoint = op(Op, Location),
    draw_component({Op, N-1}, NewPoint, NewSet, NewMatches).


manhattan_distance(#point{x=X, y=Y}) ->
    abs(X) + abs(Y).


%%%%%% PART2 will use a different draw: instead of a set, use a map with the number of steps. Place an "Ident"
%%%%%% to say you've been here already and only set your step number once. The second "drawer" will see a different
%%%%%% Ident and determine to add step counters.
draw_wire_step_measure(D, [], _, _) -> D;
draw_wire_step_measure(#draw_on{location=InitialPoint, point_set=PointSet, matches=Matches}, [H|T], Steps, Ident) ->
    {EndPoint, NewPointSet, NewMatches, NewSteps} = draw_component_step_measure(H, InitialPoint, PointSet, Matches, Steps, Ident),
    draw_wire_step_measure(#draw_on{location=EndPoint, point_set=NewPointSet, matches=NewMatches}, T, NewSteps, Ident).

draw_component_step_measure({_, 0}, Point, Map, Match, Steps, _) -> {Point, Map, Match, Steps};
draw_component_step_measure({Op, N}, Location, Map, Matches, Steps, Ident) ->
    {NewMap, NewMatches} = case maps:get(Location, Map, not_visited) of
        not_visited ->
            {maps:put(Location, {Ident, Steps}, Map), Matches};
        {Ident, _} -> {Map, Matches};
        {_, PreviousSteps} ->
            TotalSteps = Steps + PreviousSteps,
            {maps:put(Location, {Ident, TotalSteps}, Map), [TotalSteps|Matches]}
        %% You've already crossed
    end,
    draw_component_step_measure({Op, N-1}, op(Op, Location), NewMap, NewMatches, Steps + 1, Ident).


op(up, #point{x=X, y=Y}) -> #point{x=X, y=Y+1};
op(down, #point{x=X, y=Y}) -> #point{x=X, y=Y-1};
op(left, #point{x=X, y=Y}) -> #point{x=X-1, y=Y};
op(right, #point{x=X, y=Y}) -> #point{x=X+1, y=Y}.
