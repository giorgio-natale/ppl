-module(btrees).
-compile(export_all).

incbtrees ({leaf, X}) ->
    {branch, {leaf, X + 1}, {leaf, X + 1}};
incbtrees ({branch, Ltree, Rtree}) -> 
    {branch, incbtrees(Ltree), incbtrees(Rtree)}.

start()->
    register(infiniteListProc, spawn(fun () -> infiniteListProc({leaf, 1}) end)).

getNext() -> 
    infiniteListProc ! {self(), next},
    receive
        {infiniteListProc, Tree} -> io:format("Current tree: ~p~n", [Tree])
    end.

stop() ->
    infiniteListProc ! {self(), stop},
    ok.

infiniteListProc(State) ->
    receive
        {Caller, next} ->
            CurrentTree = State,
            NextTree = incbtrees(CurrentTree),
            Caller ! {infiniteListProc, CurrentTree},
            infiniteListProc(NextTree);
        {_, stop} -> ok
    end.
    
