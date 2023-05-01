-module(csvparser).

-export([parse/2, print_list/1]).

% CSV : "../measures/AFTERe11_sensor_fusion@nav_1.csv"
% return a list of the column Index
% For nav_3 acceleration are in 5, 8, 11
parse(CSV, Index) ->
    Lines = parser(CSV),
    CL = clean_lines(Lines, Index),
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

% return a list of the column Index
clean_lines(List, Index) ->
    clean_lines(List, Index, []).
clean_lines(List, Index, AccX) ->
    case List of 
        [] -> AccX;
        [H|T] ->
            Just_AccX_H  = lists:nth(Index, H),
            New_H = lists:append(AccX, [Just_AccX_H]),
            clean_lines(T, Index, New_H)
    end.
