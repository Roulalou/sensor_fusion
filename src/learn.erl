-module(learn).

-export([main/0]).
-import(csvparser, [parse/0,  print_list/1]).

main() ->
    Vector = parse(),
    Pattern = analyze(Vector),
    % print_list(Pattern),
    Flow = regroup(Pattern),
    print_list(Flow).


analyze(Vector) ->
    analyze(Vector, []).

analyze(Vector, Pattern) ->
    case Vector of
        [] -> Pattern;
        [H|T] ->
            if H < -1 ->
                analyze(T, lists:append(Pattern, ["neg"]));
            H > 1 ->
                analyze(T, lists:append(Pattern, ["pos"]));
            true ->
                analyze(T, lists:append(Pattern, ["zero"]))
            end
    end.

regroup(Pattern) ->
    regroup(Pattern, []).

regroup(Pattern, Flow) ->
    case Pattern of
        [] -> Flow;
        [H|T] ->
            if T == [] ->
                regroup(T, lists:append(Flow, [H]));
            true ->
                [HT|_] = T,
                Next = HT, % head tail
                if H == Next ->
                    regroup(T, Flow);
                true ->
                    regroup(T, lists:append(Flow, [H]))
                end
            end
    end.

    
