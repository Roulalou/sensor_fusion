-module(classify).

-import(csvparser, [parse/2, print_list/1]).
-import(learn, [analyze/1, regroup/1, average/1]).
-export([import_gesture/0, classify_new_gesture/1]).

% CSV : "../measures/hc1.csv"
classify_new_gesture(CSV) ->
    List_gestures = import_gesture(),

    VectorX = parse(CSV, 3), % 3 is the index of the x axis acceleration
    PatternX = analyze(VectorX),
    Clean_PatX = average(PatternX),
    NewX = regroup(Clean_PatX), % New is the general flow of the new gesture

    VectorY = parse(CSV, 4), % 4 is the index of the y axis acceleration
    PatternY = analyze(VectorY),
    Clean_PatY = average(PatternY),
    NewY = regroup(Clean_PatY), % New is the general flow of the new gesture

    VectorZ = parse(CSV, 5), % 5 is the index of the z axis acceleration
    PatternZ = analyze(VectorZ),
    Clean_PatZ = average(PatternZ),
    NewZ = regroup(Clean_PatZ), % New is the general flow of the new gesture

    {Name, Accuracy} = compare_gesture(NewX, NewY, NewZ, List_gestures),

    % for the moment a simple print
    io:format("Name : ~p, with Acc : ~p~n", [Name, Accuracy]).

    
% import the gesture file to a list of gesture
import_gesture() ->
    {_, Data} = file:read_file("gesture"),
    Gestures = string:tokens(binary_to_list(Data), "\n"),
    % print_list(Gestures),

    Cleaned_Gestures = [string:substr(G, 2, length(G)-2) || G <- Gestures],
    List_Gestures = [str_to_atom_list(G) || G <- Cleaned_Gestures],
    List_Gestures.
    
% String to list of atoms
str_to_atom_list(Str) ->
    [list_to_atom(E) || E <-string:tokens(Str,",")].

% compare the new gesture to the list of gestures
compare_gesture(NewX, NewY, NewZ, List_gestures) ->
    compare_gesture(NewX, NewY, NewZ, List_gestures, none, 0).
compare_gesture(NewX, NewY, NewZ, List_gestures, Name, Accuracy) ->
    case List_gestures of
        [] -> {Name, Accuracy}; % Empty list
        [H|T] ->
            % Take the 3 next list of flow, to have the 3 axis
            [GName|_] = H, % GName = Gesture Name
            % io:format("HEAD : ~p~n", [H]),
            TriList = lists:sublist([H|T], 3),
            io:format("N : ~p~n", [GName]),
            New_Accuracy = tri_compare(NewX, NewY, NewZ, TriList),
            Next_G = lists:sublist([H|T], 4, length([H|T])), % remove the 3 axis for the gesture compared
            if New_Accuracy > Accuracy ->
                compare_gesture(NewX, NewY, NewZ, Next_G, GName, New_Accuracy);
            true ->
                compare_gesture(NewX, NewY, NewZ, Next_G, Name, Accuracy)
            end
    end.

% compare the 3 axis
tri_compare(NewX, NewY, NewZ, TriList) ->
    GXT = lists:nth(1, TriList), % Gesture X Axis, with TOO MUCH variables
    GX = lists:sublist(GXT, 3, length(GXT)),
    GYT = lists:nth(2, TriList),
    GY = lists:sublist(GYT, 3, length(GYT)),
    GZT = lists:nth(3, TriList),
    GZ = lists:sublist(GZT, 3, length(GZT)),
    Acc_X = direct_compare(NewX, GX),
    Acc_Y = direct_compare(NewY, GY),
    Acc_Z = direct_compare(NewZ, GZ),
    io:format("X : ~p, Y : ~p, Z : ~p~n", [Acc_X, Acc_Y, Acc_Z]),
    Acc = (Acc_X + Acc_Y + Acc_Z) / 3, % Average over the 3 axis
    Acc.


% compare NEW to 1 GESTURE
direct_compare(New, Gesture) ->
    direct_compare(New, Gesture, 0, 0).
direct_compare(New, Gesture, Okay, Comparison) ->
    % For the moment not very opti, we look if a list is empty and then we return the accuracy


    if New == [] ->
        Total_Comp = finish_list(Gesture, 0) + Comparison,
        % io:format("NOk : ~p, Comp : ~p~n", [Okay, Total_Comp]),
        Okay/Total_Comp;
    true ->
        if Gesture == [] ->
            Total_Comp = finish_list(New, 0) + Comparison,
            % io:format("GOk : ~p, Comp : ~p~n", [Okay, Total_Comp]),
            Okay/Total_Comp;
        true ->
            [NH|NT] = New,
            [GH|GT] = Gesture,
            if NH == GH ->
                direct_compare(NT, GT, Okay+1, Comparison+1);
            true ->
                direct_compare(NT, GT, Okay, Comparison+1)
            end
        end
    end.

% Count the number of element in a list
finish_list(List, N) ->
    case List of
        [] -> N;
        [_|T] -> finish_list(T, N+1)
    end.
