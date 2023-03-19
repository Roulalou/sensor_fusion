-module(miniCluster).

% -compile(export_all).
-define(DATASET, [{1,8}, {1,9}, {2,9}, {8,1}, {9,1}, {9,2}]).
-export([km/0]).


% init() ->
%     Var = lists:nthtail(14, atom_to_list(node())),
%     io:format("Var : ~p~n", [Var]).

km() ->
    kmean(?DATASET, 2).

kmean(Dataset, K) ->
    io:format("Dataset : ~p~n", [Dataset]),
    Centroids = random_centroids(Dataset, K),
    io:format("Initial Centroid : ~p~n", [Centroids]),
    Clusters = assign_clusters(Dataset, Centroids),
    io:format("Clusters : ~p~n", [Clusters]).
    % New_centroids = compute_centroids(Clusters).
    % repeat
    %     clusters = assign_clusters(dataset, centroids),
    %     new_centroids = compute_centroids(clusters),
    %     if
    %         centroids == new_centroids -> clusters;
    %         true -> kmean(dataset, k)
    %     end
    % end.
    
random_centroids(Dataset, K) ->
    random_centroids(Dataset, K, []).

random_centroids(Dataset, K, Centroids_list) ->
    Centroid  = {rand:uniform(10), rand:uniform(10)},
    New_centroids_list = [Centroid | Centroids_list],
    if
        length(New_centroids_list) == K ->
            New_centroids_list;
        true -> 
            random_centroids(Dataset, K, New_centroids_list)
    end.   
    
assign_clusters(Dataset, Centroids) ->
    N = length(Centroids),
    Clusters = [ [] || _ <- lists:seq(1,N) ], % N = length(Centroids)
    assign_clusters(Dataset, Centroids, Clusters).

assign_clusters(Dataset, Centroid, Clusters) ->
    case Dataset of
        [] -> Clusters; % Parse through the whole Dataset : return clusters
        [H|T] ->
            Closest = closest_centroid(H, Centroid),
            io:format("Clusters : ~p~n", [Clusters]),
            Actual_point = lists:nth(Closest, Clusters),
            New__actual_point = [H | Actual_point],
            New_clusters = replace_nth_element(Closest, New__actual_point, Clusters),
            assign_clusters(T, Centroid, New_clusters)
    end.

closest_centroid(Point, Centroids) -> % Return the index of the closest centroid
    closest_centroid(Point, Centroids, 1, 0, 9999). % 9999 is a random large number

closest_centroid(_, [], _, ClosestIndex, _) -> % When the cluster list is empty
    ClosestIndex;

closest_centroid(Point, [Centroid|Rest], Index, ClosestIndex, ClosestDistance) ->
    Distance = euclidean_distance(Point, Centroid),
    if
        Distance < ClosestDistance ->
            closest_centroid(Point, Rest, Index+1, Index, Distance);
        true ->
            closest_centroid(Point, Rest, Index+1, ClosestIndex, ClosestDistance)
    end.

euclidean_distance({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X2-X1, 2) + math:pow(Y2-Y1, 2)).

replace_nth_element(N, Value, [_ | T]) when N =:= 1 ->
    [Value | T];
replace_nth_element(N, Value, [H | T]) when N > 1 ->
    [H | replace_nth_element(N-1, Value, T)];
replace_nth_element(_, _, []) ->
    [].