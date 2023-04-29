-module(learn).

-export([main/0]).
-import(csvparser, [parse/0,  print_list/1]).

main() ->
    Vector = parse(),
    Pattern = analyze(Vector),
    print_list(Pattern).


analyze(Vector) ->
    analyze(Vector, []).

analyze(Vector, Pattern) ->
    case Vector of
        [] -> Pattern;
        [H|T] ->
            if 
            H < -1 ->
                analyze(T, lists:append(Pattern, ["neg"]));
            H > 1 ->
                analyze(T, lists:append(Pattern, ["pos"]));
            true ->
                analyze(T, lists:append(Pattern, ["zero"]))
            end
    end.