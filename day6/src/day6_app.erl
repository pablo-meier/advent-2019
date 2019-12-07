-module(day6_app).
-export([start/0]).

%% ALL STANDARD LIBRARIES NEED A MULTIMAP LOL


start() ->
    %% Quick tests.
    LTest = read_input_file("test.txt"),
    GTest = build_graph(LTest),
    CTest = count_orbits(GTest),
    42 = CTest,

    Lines = read_input_file("input.txt"),
    Graph = build_graph(Lines),
    Count = count_orbits(Graph),
    io:format("~nPart 1: ~p~n", [Count]),

    LTest2 = read_input_file("part2_test.txt"),
    GTest2 = build_graph(LTest2),
    TestPath = count_path_from(<<"YOU">>, <<"SAN">>, GTest2),
    4 = TestPath,

    SantaPath = count_path_from(<<"YOU">>, <<"SAN">>, Graph),
    io:format("Part 2: ~p~n", [SantaPath]).


%% By convention, I'll be putting puzzle inputs in priv/
read_input_file(Filename) ->
    {ok, Contents} = file:read_file(filename:join([code:priv_dir(day6), Filename])),
    Lines = binary:split(Contents, [<<"\n">>], [global]),
    lists:filter(fun (X) -> X =/= <<"">> end, Lines).


%% Probably doesn't need a graph, an adjacency-list suffices.
build_graph(Lines) ->
    Pairs = lists:map(fun (L) -> binary:split(L, [<<")">>]) end, Lines),
    {OrbitList, DirectOrbit, AllBodies, OrbitingBodies} = lists:foldl(
        fun ([Body, OrbitedBy], {OrbitListAccum, DirectOrbitAccum, AllBodiesAccum, OrbitingBodiesAccum}) ->
                {multimap_add(Body, OrbitedBy, OrbitListAccum),
                 maps:put(OrbitedBy, Body, DirectOrbitAccum),
                 sets:add_element(Body, AllBodiesAccum),
                 sets:add_element(OrbitedBy, OrbitingBodiesAccum)}
        end, {maps:new(), maps:new(), sets:new(), sets:new()}, Pairs),
    {OrbitList, DirectOrbit, hd(sets:to_list(sets:subtract(AllBodies, OrbitingBodies)))}.


%% BFS from Root, can do dynamic programming-like to re-use previous computations.
count_orbits({OrbitMap, DirectOrbit, Root}) ->
    SumMap = maps:from_list([{Root, 0}]),
    BFSQueue = queue:from_list(maps:get(Root, OrbitMap)),
    count_orbits_accum(OrbitMap, DirectOrbit, SumMap, BFSQueue, 0).


%% This pattern-match with empty queue is nasty lol
count_orbits_accum(_, _, _, {[], []}, Accum) -> Accum;
count_orbits_accum(BFSMap, DirectOrbit, SumMap, Queue, Accum) ->
    CurrElem = queue:head(Queue),
    Orbits = maps:get(CurrElem, DirectOrbit),
    ItsSum = maps:get(Orbits, SumMap),

    NewOrbits = ItsSum + 1,
    NewSumMap = maps:put(CurrElem, NewOrbits, SumMap),
    NewQueue = add_all_to_queue(maps:get(CurrElem, BFSMap, []), queue:tail(Queue)),
    count_orbits_accum(BFSMap, DirectOrbit, NewSumMap, NewQueue, Accum + NewOrbits).


%%%%%%%%% PART 2

%%% If anything, a bit easier: BFS out from Start into a Queue adding 1 each later, stop when you hit
%%% SAN.
count_path_from(Start, Finish, {OrbitList, DirectOrbit, _}) ->
    count_path_recursion(Finish, OrbitList, DirectOrbit, queue:from_list([{Start, 0}]), sets:new()).


count_path_recursion(Finish, OrbitList, DirectOrbit, Queue, Seen) ->
    case queue:head(Queue) of
        {Finish, Accum} -> Accum - 2;  % We don't count the edges of the start or end nodes.
        {Other, Count} ->
            case sets:is_element(Other, Seen) of
                true ->
                    count_path_recursion(Finish, OrbitList, DirectOrbit, queue:tail(Queue), Seen);
                false ->
                    Neighbor = maps:get(Other, DirectOrbit, base),
                    NeighborList = maps:get(Other, OrbitList, []),
                    AllNeighbors = case Neighbor of
                        base -> NeighborList;
                        X -> [X|NeighborList]
                    end,
                    ToAdd = lists:map(fun (X) -> {X, Count + 1} end, AllNeighbors),
                    NewQueue = add_all_to_queue(ToAdd, Queue),
                    count_path_recursion(Finish, OrbitList, DirectOrbit, NewQueue, sets:add_element(Other, Seen))
            end
    end.


add_all_to_queue([], Q) -> Q;
add_all_to_queue([H|T], Q) -> add_all_to_queue(T, queue:snoc(Q, H)).


multimap_add(K, V, Multimap) ->
    case maps:find(K, Multimap) of
        {ok, Lst} -> maps:put(K, [V|Lst], Multimap);
        error -> maps:put(K, [V], Multimap)
    end.
