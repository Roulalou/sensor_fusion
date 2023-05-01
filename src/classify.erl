-module(classify).

-import(csvparser, [parse/1, print_list/1]).
-import(learn, [analyze/1, regroup/1]).
-export([import_gesture/0, classify_new_gesture/1]).

% CSV : "../measures/test.csv"
classify_new_gesture(CSV) ->
    List_gestures = import_gesture(),

    Vector = parse(CSV),
    Pattern = analyze(Vector),
    New = regroup(Pattern), % New is the general flow of the new gesture
    {Name, Accuracy} = compare_gesture(New, List_gestures),

    % for the moment a simple print
    io:format("Name : ~p, with Acc : ~p~n", [Name, Accuracy]).

    
% import the gesture file to a list of gesture
import_gesture() ->
    {_, Data} = file:read_file("gesture"),
    Gestures = string:tokens(binary_to_list(Data), "\n"),
    print_list(Gestures),

    Cleaned_Gestures = [string:substr(G, 2, length(G)-2) || G <- Gestures],
    List_Gestures = [str_to_atom_list(G) || G <- Cleaned_Gestures],
    List_Gestures.
    
% String to list of atoms
str_to_atom_list(Str) ->
    [list_to_atom(E) || E <-string:tokens(Str,",")].

% compare the new gesture to the list of gestures
compare_gesture(New, List_gestures) ->
    compare_gesture(New, List_gestures, none, 0).
compare_gesture(New, List_gestures, Name, Accuracy) ->
    case List_gestures of
        [] -> {Name, Accuracy};
        [H|T] ->
            [GName|GFlow] = H,
            New_Accuracy = direct_compare(New, GFlow),
            if New_Accuracy > Accuracy ->
                compare_gesture(New, T, GName, New_Accuracy);
            true ->
                compare_gesture(New, T, Name, Accuracy)
            end
    end.

% compare NEW to 1 GESTURE
direct_compare(New, Gesture) ->
    direct_compare(New, Gesture, 0, 0).
direct_compare(New, Gesture, Okay, Comparison) ->
    % For the moment not very opti, we look if a list is empty and then we return the accuracy

    io:format("Ok : ~p, Comp : ~p~n", [Okay, Comparison]),

    if New == [] ->
        Okay/Comparison;
    true ->
        if Gesture == [] ->
            Okay/Comparison;
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

