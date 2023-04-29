-module(csvparser).

-export([main/0]).

main() ->
    Lines = parser(),
    CL = clean_lines(Lines),
    print_csv(CL),
    CL.

parser() ->
    % {ok, File} = file:open("../measures/AFTERe11_sensor_fusion@nav_1.csv", [read]),
    {_, Data} = file:read_file("../measures/AFTERe11_sensor_fusion@nav_1.csv"),
    % file:close(File),
    % io:format("Data : ~p~n", [Data]),
    % Data.
    % io:format("Data : ~p~n", [Data]),
    Rows = string:tokens(binary_to_list(Data), "\n"),
    Records = [string:tokens(Row, ",") || Row <- Rows],
    % print_csv(Records).
    Records.

print_csv([H|T]) ->
    io:format("H : ~p~n", [H]),
    print_csv(T);
print_csv([]) ->
    ok.

clean_lines([H|T]) ->
    Just_AccX_H  = lists:nth(3, H),
    clean_lines(T, [Just_AccX_H]).

clean_lines(List, AccX) ->
    case List of 
        [] -> AccX;
        [H|T] ->
            Just_AccX_H  = lists:nth(3, H),
            New_H = lists:append(AccX, [Just_AccX_H]),
            clean_lines(T, New_H)
    end.
