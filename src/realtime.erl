-module(realtime).

-export([start/1]).

start(Maxtime) ->
    io:format("Countdown!~n"),
    countdown(5),
    [{_, _,StartTime,_}] = hera_data:get(nav3, sensor_fusion@nav_1),
    io:format("StartTime : ~p~n", [StartTime]),
    collect_data_over_time(StartTime + Maxtime).

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