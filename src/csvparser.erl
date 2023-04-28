-module(csvparser).

-export([main/0, parser/0]).

main() ->
    % Lines = parser().
    parser().

parser() ->
    % {ok, File} = file:open("../measures/AFTERe11_sensor_fusion@nav_1.csv", [read]),
    {_, Data} = file:read_file("../measures/AFTERe11_sensor_fusion@nav_1.csv"),
    % file:close(File),
    % io:format("Data : ~p~n", [Data]),
    % Data.
    % io:format("Data : ~p~n", [Data]),
    Rows = string:tokens(binary_to_list(Data), "\n"),
    Records = [string:tokens(Row, ",") || Row <- Rows],
    print_csv(Records).

print_csv([H|T]) ->
    io:format("H : ~p~n", [H]),
    print_csv(T);
print_csv([]) ->
    ok.