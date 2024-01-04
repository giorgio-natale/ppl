-module(lexer).
-compile(export_all).

string2chunk([], _) -> [];
string2chunk(String, Size) -> 
    NextChunk = lists:sublist(String, 1, Size),
    [NextChunk | string2chunk(lists:sublist(String, length(NextChunk) + 1, length(String) - length(NextChunk)), Size)].

reverse([]) -> [];
reverse([X | Xs]) -> reverse(Xs) ++ [X].

chunk2wordatoms({start, [], Collected}) -> Collected;
chunk2wordatoms({start, [32 | Rest], Collected}) -> chunk2wordatoms({flushSpace, Rest, Collected});
chunk2wordatoms({start, [X | Rest], Collected}) -> chunk2wordatoms({collect, Rest, [[X] | Collected]});
chunk2wordatoms({flushSpace, [32 | Rest], Collected}) -> chunk2wordatoms({flushSpace, Rest, Collected});
chunk2wordatoms({flushSpace, [X | Rest], Collected}) -> chunk2wordatoms({collect, Rest, [[X] | Collected]});
chunk2wordatoms({collect, [32 | Rest], Collected}) -> chunk2wordatoms({flushSpace, Rest, Collected});
chunk2wordatoms({collect, [X | Rest], [Current | Other]}) -> chunk2wordatoms({collect, Rest, [Current ++ [X] | Other]});
chunk2wordatoms({_, [], Collected}) -> Collected.

c2w(String) -> lists:map(fun list_to_atom/1, reverse(chunk2wordatoms({start, String, []}))).

plex(String, Size) ->
    Chunks = string2chunk(String, Size),
    Self = self(),
    WorkerPids = lists:map(fun (Chunk) -> spawn(fun () -> worker(Self, Chunk) end) end, Chunks),
    lists:map(fun (WorkerPid) -> receive {WorkerPid, Words} -> Words end end, WorkerPids).

worker(Caller, Chunk) ->
    Caller ! {self(), c2w(Chunk)}.


test() ->
    io:format("~p~n", [string2chunk("this is a nice  test", 6)]),
    io:format("~p~n", [c2w("this is a nice  test")]).
