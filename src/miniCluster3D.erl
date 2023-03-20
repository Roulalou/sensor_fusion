% Small Cluster algorithm based on kmean for 3D point between 0 and 10

-module(miniCluster3D).

% -compile(export_all).
-define(DATASET, [{1,8,0}, {1,9,0}, {2,9,0}, {1,9,3}, {8,1,9}, {9,1,9}, {9,2,9}, {9,1,7}]).
-define(MAX_OCC, 10).
-define(MAX_VAL, 10).
-define(RANDOM_LARGE_NUMBER, 9999).
-export([km/0, kmean/2]).

km() ->
    kmean(?DATASET, 2).

kmean(Dataset, K) -> % First occurence
    io:format("Dataset : ~p~n", [Dataset]),
    Centroids = random_centroids(Dataset, K),
    io:format("Initial Centroid : ~p~n", [Centroids]),
    kmean_loop(Dataset, Centroids, 0).

kmean_loop(Dataset, Centroids, Occurence) ->
    Clusters = assign_clusters(Dataset, Centroids),
    io:format("Clusters : ~p~n", [Clusters]),
    New_centroids = compute_centroids(Clusters),
    io:format("New Centroid : ~p~n", [New_centroids]),
    Sorted_centroids = lists:sort(Centroids), % Sort the centroids because == donn't work if element aren't at the same place
    Sorted_new_centroids = lists:sort(New_centroids),
    if 
        Sorted_centroids == Sorted_new_centroids -> Clusters; % If the centroids are the same, return the clusters
        Occurence >= ?MAX_OCC -> Clusters; % If the number of occurence is greater than MAX, return the clusters
        true -> kmean_loop(Dataset, New_centroids, Occurence + 1) % Else, repeat the process
    end.
    
random_centroids(Dataset, K) ->
    random_centroids(Dataset, K, []).

random_centroids(Dataset, K, Centroids_list) ->
    Centroid  = {rand:uniform(?MAX_VAL), rand:uniform(?MAX_VAL), rand:uniform(?MAX_VAL)},
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
            Closest = closest_centroid(H, Centroid), % Return the index of the closest centroid
            Actual_point = lists:nth(Closest, Clusters),
            New__actual_point = [H | Actual_point],
            New_clusters = replace_nth_element(Closest, New__actual_point, Clusters),
            assign_clusters(T, Centroid, New_clusters)
    end.

closest_centroid(Point, Centroids) -> % Return the index of the closest centroid
    closest_centroid(Point, Centroids, 1, 0, ?RANDOM_LARGE_NUMBER). % 9999 is a random large number

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

euclidean_distance({X1, Y1, Z1}, {X2, Y2, Z2}) ->
    math:sqrt(math:pow(X2-X1, 2) + math:pow(Y2-Y1, 2) + math:pow(Z2-Z1, 2)).

replace_nth_element(N, Value, [_ | T]) when N =:= 1 ->
    [Value | T];
replace_nth_element(N, Value, [H | T]) when N > 1 ->
    [H | replace_nth_element(N-1, Value, T)];
replace_nth_element(_, _, []) ->
    [].

compute_centroids(Clusters) ->
    compute_centroids(Clusters, []).

compute_centroids(Clusters, New_centroids) ->
    case Clusters of
        [] -> New_centroids;
        [H|T] -> % H is a cluster
            New_centroid = compute_centroid(H),
            compute_centroids(T, [New_centroid | New_centroids])
    end.

compute_centroid(Cluster) ->
    case Cluster of 
        [] -> % If the cluster is empty, return a random point
            {rand:uniform(?MAX_VAL), rand:uniform(?MAX_VAL), rand:uniform(?MAX_VAL)};
        [_|_] ->
            X = lists:sum([X || {X, _, _} <- Cluster]) / length(Cluster),
            Y = lists:sum([Y || {_, Y, _} <- Cluster]) / length(Cluster),
            Z = lists:sum([Z || {_, _, Z} <- Cluster]) / length(Cluster),
            {X, Y, Z}
    end.
        
