-module(realtime).

-export([start/2]).

start(Type, Maxtime) ->
    case Type of 
    once ->
        io:format("Countdown!~n"),
        countdown(5),
        [{_, _,StartTime,_}] = hera_data:get(nav3, sensor_fusion@nav_1),
        io:format("StartTime : ~p~n", [StartTime]),
        collect_data_over_time(StartTime + Maxtime);
    loop ->
         grdos(Maxtime) % gesture_recognition_division_over_stop
    end.

%%%%% For the First Method %%%%%%

collect_data_over_time(Maxtime) ->
    collect_data_over_time(Maxtime, [], 0).

collect_data_over_time(Maxtime, List, LastT) ->
    [{_, _,Time, Data}] = hera_data:get(nav3, sensor_fusion@nav_1), %[{_, _,Time, Data}]
    % io:format("~p~n", [Time]),

    if Time > Maxtime ->
        % io:format("List: ~p~n", [List]),
        io:format("Done!~nCalculating...~n"),
        classify:classify_new_gesture(List);
    true ->
        if Time == LastT ->
            % io:format("Same time! : ~p~n", [Time]),
            collect_data_over_time(Maxtime, List, Time);
        true ->
            NewList = lists:append(List, [Data]),
            collect_data_over_time(Maxtime, NewList, Time)
        end
    end.

countdown(Count) ->
    case Count of
        0 ->
            io:format("Move!~n");
        _ ->
            io:format("~p~n", [Count]),
            timer:sleep(1000),
            countdown(Count-1)
    end.

%%%%% For the Second Method %%%%%%

grdos(Maxtime) ->
    AS = learn:av_size() - 1,
    grdos(Maxtime, AS, [], 0, [], 0, 0, o, o, nn).

% TO(TimeOut) : time to stay without moving
% AS(Average Size) : Size of the List where we will do the average (it is too reduce the noise)
% List : List used to detect the stop
% SizeL : Size of the List
% GestureList: List of collected data since last gesture
% LastT : Time of the last data, used to detectif Hera produce a new data
% TSM(Time Since Move) : Last time a movement was detected, if greater than TO, then we have a stop
% LastX, LastY, LastZ : Last gesture for an axis
grdos(TO, AS, List, SizeL, GestureList, LastT, TSM, LastX, LastY, LastZ) ->
    [{_, _,Time, Data}] = hera_data:get(nav3, sensor_fusion@nav_1),
    if Time == LastT ->
        grdos(TO, AS, List, SizeL, GestureList, Time, TSM, LastX, LastY, LastZ); % Skip if no new data
    true ->
        NewGestureList = lists:append(GestureList, [Data]),
        NewList = lists:append(List, [Data]),
        NewSizeL = SizeL + 1,
        if NewSizeL >= AS ->  % It mean we can compute the average
            ListX = csvparser:parse(NewList, 1),
            ListY = csvparser:parse(NewList, 2),
            ListZ = csvparser:parse(NewList, 3),
            PatternX = learn:analyze(ListX),
            PatternY = learn:analyze(ListY),
            PatternZ = learn:analyze(ListZ),
            AvgX = learn:average(PatternX), % VERIFIER QUE BIEN 1 SEULE DONNEE DEDANS
            AvgY = learn:average(PatternY),
            AvgZ = learn:average(PatternZ),
            % io:format("AvX: ~p, AvY: ~p, AvZ: ~p~n", [AvgX, AvgY, AvgZ]),
            [HX|_] = AvgX,
            [HY|_] = AvgY,
            [HZ|_] = AvgZ,
            % io:format("LastX: ~p =?= HX: ~p~n", [LastX, HX]),
            % io:format("LastY: ~p =?= HY: ~p~n", [LastY, HY]),
            % io:format("LastZ: ~p =?= HZ: ~p~n", [LastZ, HZ]),
            if LastX == HX andalso LastY == HY andalso LastZ == HZ -> % If the last gesture is the same as the new one
                if Time >= TSM + TO ->
                    io:format("~n~n~n~n~n~n"), % Just to make it more readable
                    io:format("Stop detected!~n"),
                    classify:classify_new_gesture(GestureList),
                    grdos(TO, AS, [], 0, [], Time, Time, LastX, LastY, LastZ); % AND IT GO AGAIN !!! indefinitely ... (for now)
                true -> % too soon, still need to wait
                    % io:format("Too soon : ~p ms     ",[(TSM + TO) - Time]),
                    grdos(TO, AS, [], 0, GestureList, Time, TSM, LastX, LastY, LastZ)
                end;
            true ->
                NewLastX = HX,
                NewLastY = HY,
                NewLastZ = HZ,
                NewTSM = Time,
                % io:format("X: ~p, Y: ~p, Z: ~p, T: ~p~n", [HX, HY, HZ, Time]),
                grdos(TO, AS, [], 0, NewGestureList, Time, NewTSM, NewLastX, NewLastY, NewLastZ)
            end;
        true ->
            grdos(TO, AS, NewList, NewSizeL, NewGestureList, Time, TSM, LastX, LastY, LastZ)
        end
    end.