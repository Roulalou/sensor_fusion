-module(realtime).

-export([start/1]).

start(Maxtime) ->
    io:format("Countdown!~n"),
    countdown(5),
    [{_, _,StartTime,_}] = hera_data:get(nav3, sensor_fusion@nav_1),
    io:format("StartTime : ~p~n", [StartTime]),
    collect_data_over_time(StartTime + Maxtime).

collect_data_over_time(Maxtime) ->
    [{_, _,Time, Data}] = hera_data:get(nav3, sensor_fusion@nav_1),
    io:format("~p~n", [Data]),
    if Time > Maxtime ->
        io:format("Done!~n");
    true ->
        collect_data_over_time(Maxtime)
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