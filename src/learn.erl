-module(learn).

-export([to_file/1, learn/2]).
-import(csvparser, [parse/0, parse/1,  print_list/1]).

% CSV : "../measures/AFTERe11_sensor_fusion@nav_1.csv"
learn(CSV, Name) ->
    Vector = parse(CSV),
    Pattern = analyze(Vector),
    % print_list(Pattern),
    Flow = regroup(Pattern),
    Gesture = lists:append([Name], [Flow]),
    to_file(Gesture).

analyze(Vector) ->
    analyze(Vector, []).

analyze(Vector, Pattern) ->
    case Vector of
        [] -> Pattern;
        [H|T] ->
            % Rules of patterns
            {Int_H, _} = string:to_integer(H),
            % io:format("Int_H : ~p~n", [Int_H]),
            if Int_H < -1 ->
                analyze(T, lists:append(Pattern, ["neg"]));
            Int_H > 1 ->
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

to_file(Gesture) ->
    file:write_file("gesture", io_lib:fwrite("~p\n", [Gesture]), [append]).