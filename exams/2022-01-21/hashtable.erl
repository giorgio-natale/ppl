-module(hashtable).
-compile(export_all).

listGet(_ , []) ->
    error("Index out of bound");
listGet(0, [X | _]) ->
    X;
listGet(N, [_ | Xs]) -> listGet(N-1, Xs).

createHashTable(HashFun, Nbuckets) ->
    Buckets = spawnBuckets(Nbuckets),
    spawn(fun () -> queryServer(HashFun, Buckets) end).

spawnBuckets(0) -> [];
spawnBuckets(N) -> [spawn(fun () -> bucket(#{}) end) | spawnBuckets(N - 1)].

queryServer(HashFun, Buckets) ->
    receive
        {insert, Key, Val, CallerPid} -> 
            BucketIdx = HashFun(Key),
            io:format("Inserting (~p, ~p) into bucket of index ~p~n", [Key, Val, BucketIdx]),
            listGet(BucketIdx, Buckets) ! {insert, Key, Val, CallerPid, self()};
        {lookup, Key, CallerPid} ->
            BucketIdx = HashFun(Key),
            io:format("retrieving ~p from bucket of index ~p~n", [Key, BucketIdx]),
            listGet(BucketIdx, Buckets) ! {lookup, Key, CallerPid, self()};
        {insert_answer, Key, CallerPid} ->
            CallerPid ! {insert_answer, Key};
        {lookup_answer, Key, Val, CallerPid} ->
            CallerPid ! {lookup_answer, Key, Val}
    end,
    queryServer(HashFun, Buckets).

bucket(Map) ->
    receive
        {insert, Key, Val, CallerPid, ServerPid} ->
            NewMap = Map#{Key => Val},
            ServerPid ! {insert_answer, Key, CallerPid},
            bucket(NewMap);
        {lookup, Key, CallerPid, ServerPid} -> 
            #{Key := Val} = Map,
            ServerPid ! {lookup_answer, Key, Val, CallerPid},
            bucket(Map)
    end.

test() ->
    HT = createHashTable(fun (X) -> X rem 7 end, 7),
    HT ! {insert, 19, 7, self()},
    receive
        {insert_answer, _} -> ok
    end,
    io:format("Value inserted~n"),
    HT ! {lookup, 19, self()},
    receive
        {lookup_answer, Key, Val} -> io:format("The value of key ~p is ~p~n", [Key, Val])
    end.


    
