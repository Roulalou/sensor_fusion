-module(csvparser).

-export([parse/0, parse/1, print_list/1]).

parse() ->
    Lines = parser("../measures/AFTERe11_sensor_fusion@nav_1.csv"),
    CL = clean_lines(Lines),
    % print_list(CL),
    CL.

parse(CSV) ->
    Lines = parser(CSV),
    CL = clean_lines(Lines),
    % print_list(CL),
    CL.

parser(CSV) ->
    % {ok, File} = file:open("../measures/AFTERe11_sensor_fusion@nav_1.csv", [read]),
    {_, Data} = file:read_file(CSV),
    % file:close(File),
    % io:format("Data : ~p~n", [Data]),
    % Data.
    % io:format("Data : ~p~n", [Data]),
    Rows = string:tokens(binary_to_list(Data), "\n"),
    Records = [string:tokens(Row, ",") || Row <- Rows],
    % print_list(Records).
    Records.

print_list([H|T]) ->
    io:format("H : ~p~n", [H]),
    print_list(T);
print_list([]) ->
    ok.

clean_lines(List) ->
    clean_lines(List, []).

clean_lines(List, AccX) ->
    case List of 
        [] -> AccX;
        [H|T] ->
            Just_AccX_H  = lists:nth(3, H),
            New_H = lists:append(AccX, [Just_AccX_H]),
            clean_lines(T, New_H)
    end.
