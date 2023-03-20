-module(miniCluster).

% -compile(export_all).
-define(DATASET, [{1,8}, {1,9}, {2,9}, {8,1}, {9,1}, {9,2}]).
-define(MAX, 10).
-export([km/0]).


% init() ->
%     Var = lists:nthtail(14, atom_to_list(node())),
%     io:format("Var : ~p~n", [Var]).

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
        Occurence >= ?MAX -> Clusters; % If the number of occurence is greater than MAX, return the clusters
        true -> kmean_loop(Dataset, New_centroids, Occurence + 1) % Else, repeat the process
    end.
    
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
            % io:format("Clusters : ~p~n", [Clusters]),
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
    X = lists:sum([X || {X, _} <- Cluster]) / length(Cluster),
    Y = lists:sum([Y || {_, Y} <- Cluster]) / length(Cluster),
    {X, Y}.