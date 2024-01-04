-module(d2l).
-compile(export_all).

deeprev ([]) -> [];
deeprev ([[X | Xs] | Rest]) -> deeprev(Rest) ++ [deeprev(Xs) ++ [X]];
deeprev ([X | Rest]) -> deeprev(Rest) ++ [X].

deeprevp (L) ->
    Myself = self(),
    Child = spawn(fun () -> dp(Myself, L) end),
    receive
        {Child, ReversedList} -> ReversedList
    end.

dp(Caller, []) -> Caller ! {self(), []};

dp(Caller, [[X | Xs] | Rest]) ->
    Myself = self(),
    Child1 = spawn(fun () -> dp (Myself, Rest) end),
    Child2 = spawn(fun () -> dp (Myself, Xs) end),
    receive
        {Child1, A} -> 
            receive
                {Child2, B} ->
                    Result = A ++ [B ++ [X]],
                    Caller ! {self(), Result}
            end
    end;

dp(Caller, [X | Rest]) -> 
    Myself = self(),
    Child = spawn(fun () -> dp (Myself, Rest) end),
    receive
        {Child, A} -> Caller ! {self(), A ++ [X]}
    end.



