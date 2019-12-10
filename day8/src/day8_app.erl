-module(day8_app).
-export([start/0]).

-define(WIDE, 25).
-define(TALL, 6).


start() ->
    [Line] = read_input_file("input.txt"),
    Layers = split_layers(binary_to_list(Line), ?WIDE, ?TALL),
    Mapped = lists:map(fun digit_counts/1, Layers),
    {_, Ones, Twos} = lists:foldl(fun (LayerMap, {MinZeroes, Ones, Twos}) ->
            LayerZeroes = maps:get($0, LayerMap),
            case LayerZeroes < MinZeroes of
                true -> {LayerZeroes, maps:get($1, LayerMap), maps:get($2, LayerMap)};
                false -> {MinZeroes, Ones, Twos}
            end
        end, {600, nope, nope}, Mapped),
    io:format("~nPart 1: ~p~n", [Ones * Twos]),

    Condensed = as_one_visible_line(Line, ?WIDE, ?TALL),
    io:format("Part 2:~n~n"),
    print_formatted(Condensed, ?WIDE, ?TALL).


%% By convention, I'll be putting puzzle inputs in priv/
read_input_file(Filename) ->
    {ok, Contents} = file:read_file(filename:join([code:priv_dir(day8), Filename])),
    Lines = binary:split(Contents, [<<"\n">>], [global]),
    lists:filter(fun (X) -> X =/= <<"">> end, Lines).


split_layers(LongLine, Rows, Cols) ->
    split_layers_recursion(LongLine, Rows, Cols, []).

split_layers_recursion([], _, _, Acc) -> lists:reverse(Acc);
split_layers_recursion(Remaining, Rows, Cols, Accum) ->
    {NewLayer, NewRemaining} = lists:split(Rows * Cols, Remaining),
    split_layers_recursion(NewRemaining, Rows, Cols, [NewLayer|Accum]).


digit_counts(Layer) ->
    lists:foldl(fun (Char, Map) ->
            case maps:get(Char, Map, none) of
                none -> maps:put(Char, 1, Map);
                X -> maps:put(Char, X + 1, Map)
            end
        end, maps:new(), Layer).



%% For part two, we'll convert to array for a fast random access,
%% then jump through each layer and assign a "final" color. The return
%% "line" is a list of either `black` or `white`.
as_one_visible_line(Line, Wide, Tall) ->
    Offset = Wide * Tall,
    Arr = array:from_list(lists:map(fun code_to_atom/1, binary_to_list(Line))),
    array:to_list(as_one_visible_line_recur(Arr, Offset, 0)).


code_to_atom($0) -> black;
code_to_atom($1) -> white;
code_to_atom($2) -> transparent.

as_one_visible_line_recur(Arr, Offset, Offset) -> Arr;
as_one_visible_line_recur(Arr, Offset, Idx) ->
    Val = final_pixel_value(Arr, Offset, Idx),
    NewArr = array:set(Idx, Val, Arr),
    as_one_visible_line_recur(NewArr, Offset, Idx + 1).

final_pixel_value(Arr, Offset, Idx) ->
    final_pixel_value_recur(Arr, Offset, Idx, []).

final_pixel_value_recur(Arr, Offset, Idx, Accum) ->
    ThisLayer = array:get(Idx, Arr),
    NewAccum = [ThisLayer|Accum],
    NewIdx = Idx + Offset,
    case NewIdx >= array:size(Arr) of
        true ->
            lists:foldr(fun (This, Previous) ->
                    case Previous of
                        transparent -> This;
                        _ -> Previous
                    end
                end, transparent, NewAccum);
        false ->
            final_pixel_value_recur(Arr, Offset, NewIdx, NewAccum)
    end.


%% Prints `black` or `white` as single characters so we can see the output.
print_formatted(_, _, 0) -> ok;
print_formatted(Condensed, Wide, Tall) ->
    {NewRow, NewRest} = lists:split(Wide, Condensed),
    io:format("~s~n", [lists:map(fun map_color/1, NewRow)]),
    print_formatted(NewRest, Wide, Tall - 1).


map_color(black) -> $X;
map_color(white) -> 32.  % ASCII code for whitespace
