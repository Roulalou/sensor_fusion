-module(learn).

-export([to_file/1, learn/2, analyze/1, regroup/1, average/1]).
-import(csvparser, [parse/2,  print_list/1]). % peut aussi csvparser:parse(..) au lieu d'import
-define(AXIS, [x, y, z]).
-define(AV_SIZE, 10).

% CSV : "../measures/Anav3_sensor_fusion@nav_1.csv"
% example : learn:learn("../measures/Anav3_sensor_fusion@nav_1.csv", test).
% add a new gesture to the gesture file
learn(CSV, Name) ->
    % learn for the 3 axis
    learn_axis(CSV, Name, lists:nth(1, ?AXIS)),
    learn_axis(CSV, Name, lists:nth(2, ?AXIS)),
    learn_axis(CSV, Name, lists:nth(3, ?AXIS)).

% Called by learn() for a specific axis
learn_axis(CSV, Name, Axis) ->
    case Axis of
        x ->
            Index = 5;
        y ->
            Index = 8;
        z ->
            Index = 11
    end,
    Vector = parse(CSV, Index),
    Pattern = analyze(Vector),
    Clean_Pat = average(Pattern),
    Flow = regroup(Clean_Pat),
    Gesture = lists:append([Name, Axis], Flow),
    to_file(Gesture).

% analyze the list of acc and determine the pattern
analyze(Vector) ->
    analyze(Vector, []).
analyze(Vector, Pattern) ->
    case Vector of
        [] -> Pattern;
        [H|T] ->
            % Rules of patterns
            {Int_H, _} = string:to_integer(H),
            % io:format("Int_H : ~p~n", [Int_H]),

            % It is arbitrary values
            if Int_H < -2 ->
                analyze(T, lists:append(Pattern, [neg]));
            Int_H > 2 ->
                analyze(T, lists:append(Pattern, [pos]));
            true ->
                analyze(T, lists:append(Pattern, [zero]))
            end
    end.


% regroup the pattern to have the general flow
regroup(Pattern) ->
    regroup(Pattern, []).
regroup(Pattern, Flow) ->
    case Pattern of
        [] -> Flow;
        [H|T] ->
            if T == [] -> % If last element
                regroup(T, lists:append(Flow, [H]));
            true ->
                [HT|_] = T,
                Next = HT, % Head Tail
                if H == Next ->
                    regroup(T, Flow);
                true ->
                    regroup(T, lists:append(Flow, [H]))
                end
            end
    end.

% return a list of average value, each time over AV_SIZE data
average(List) ->
    average(List, ?AV_SIZE, []).
average(List, Size, New_L) ->
    if length(List) < Size ->
        Av = calculate_av(List),
        lists:append(New_L, [Av]); % Return the list of average
    true ->
        Sub_List = lists:sublist(List, Size),
        Av = calculate_av(Sub_List),
        Next_L = lists:sublist(List, Size + 1, length(List)),
        average(Next_L, Size, lists:append(New_L, [Av]))
    end.


% hardcoded for :  neg, pos, zero
calculate_av(List) ->
    calculate_av(List, 0, 0, 0).
calculate_av(List, Neg, Pos, Zero) ->
    case List of
        [] -> 
            % io:format("Neg : ~p, Pos : ~p, Zero : ~p~n", [Neg, Pos, Zero]),
            if Neg > Pos, Neg > Zero ->
                neg;
            Pos > Neg, Pos > Zero ->
                pos;
            true ->
                zero
            end;
        [H|T] ->
            case H of
                neg -> calculate_av(T, Neg + 1, Pos, Zero);
                pos -> calculate_av(T, Neg, Pos + 1, Zero);
                zero -> calculate_av(T, Neg, Pos, Zero + 1)
            end
    end.

% export the gesture to the file
to_file(Gesture) ->
    file:write_file("gesture", io_lib:fwrite("~p\n", [Gesture]), [append]).