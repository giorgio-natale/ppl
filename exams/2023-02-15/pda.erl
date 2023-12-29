-module(pda).
-compile(export_all).

pop(Stack) -> 
    [_ | Rest] = Stack,
    Rest.

peek(Stack) ->
    [Head | _] = Stack,
    Head.

start() ->
    register(q0, spawn(fun () -> q0() end)),
    register(q1, spawn(fun () -> q1() end)),
    register(q2, spawn(fun () -> q2() end)),
    register(q3, spawn(fun () -> q3() end)),
    register(q4, spawn(fun () -> q4() end)),
    register(q5, spawn(fun () -> q5() end)).

stop() ->
    unregister(q0),
    unregister(q1),
    unregister(q2),
    unregister(q3),
    unregister(q4),
    unregister(q5).

parse(Tape) -> q0 ! {Tape, Tape, [z]}, ok.

q0()->
    receive
        {Original, [a | Rest], [z | []]} -> q1 ! {Original, Rest, [a, z]}
    end,
    q0().


q1()->
    receive
        {Original, [a | Rest], [a | StackTail]} -> q1 ! {Original, Rest, [a, a] ++ StackTail};
        {Original, [b | Rest], [a | StackTail]} -> q2 ! {Original, Rest, StackTail},
                                                   q3 ! {Original, Rest, [a] ++ StackTail}
    end,
    q1().

q2()->
    receive
        {Original, [b | Rest], [a | StackTail]} -> q2 ! {Original, Rest, StackTail};
        {Original, Tape, [z | StackTail]} -> q5 ! {Original, Tape, StackTail}
    end,
    q2().

q3()->
    receive
        {Original, [b | Rest], [a | StackTail]} -> q4 ! {Original, Rest, StackTail}
    end,
    q3().
q4()->
    receive
        {Original, [b | Rest], [a | StackTail]} -> q3 ! {Original, Rest, [a] ++ StackTail};
        {Original, Tape, [z | StackTail]} -> q5 ! {Original, Tape, StackTail}
    end,
    q4().
q5()->
    receive
        {Original, [], S} -> io:format("Recognized: ~p; stack: ~p~n", [Original, S])
    end,
    q5().



