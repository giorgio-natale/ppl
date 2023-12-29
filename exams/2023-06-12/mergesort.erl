-module(mergesort).
-compile(export_all).

merge ([], Rlist) -> Rlist;
merge (Llist, []) -> Llist;
merge ([L | Ls], [R | Rs]) when L =< R -> [L | merge (Ls, [R | Rs])];
merge ([L | Ls], [R | Rs]) when R < L -> [R | merge ([L | Ls], Rs)].




msortp () -> 
    receive
        {ParentPid, []} -> ParentPid ! {self(), []};
        {ParentPid, [Elem]} -> ParentPid ! {self(), [Elem]};
        {ParentPid, L} -> 
            {Llist, Rlist} = lists:split(length(L) div 2, L),
            LeftChildPid = spawn(fun()-> msortp() end),
            RightChildPid = spawn(fun()-> msortp() end),

            LeftChildPid ! {self(), Llist},
            RightChildPid ! {self(), Rlist},

            receive
                {LeftChildPid, SortedL} ->
                    receive
                        {RightChildPid, SortedR} -> ParentPid ! {self(), merge(SortedL, SortedR)}
                    end
            end
    end.

sort(L) ->
    Pid = spawn(fun() -> msortp() end),
    Pid ! {self(), L},
    receive
        {Pid, SortedList} -> io:format("The sorted list is: ~p~n", [SortedList])
    end.

