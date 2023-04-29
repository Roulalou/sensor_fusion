-module(csvparser).

-export([parse/1, print_list/1]).

% CSV : "../measures/AFTERe11_sensor_fusion@nav_1.csv"
% return a list of the 4th column
parse(CSV) ->
    Lines = parser(CSV),
    CL = clean_lines(Lines),
    % print_list(CL),
    CL.

% return a list of the lines
parser(CSV) ->
    {_, Data} = file:read_file(CSV),
    Rows = string:tokens(binary_to_list(Data), "\n"),
    Records = [string:tokens(Row, ",") || Row <- Rows],
    % print_list(Records).
    Records.

% Useful for debugging, print a list
print_list([H|T]) ->
    io:format("H : ~p~n", [H]),
    print_list(T);
print_list([]) ->
    ok.

% return a list of the 4th column
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
