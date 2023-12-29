-module(stateMachine).
-compile(export_all).

start_machine() -> 
    register(q0, spawn(fun()-> q0_parse() end)),
    register(q1, spawn(fun()-> q1_parse() end)),
    register(q2, spawn(fun()-> q2_parse() end)),
    register(q3, spawn(fun()-> q3_parse() end)),
    register(q4, spawn(fun()-> q4_parse() end)).

parse_string(Tape) ->
    q0 ! {parse, Tape, Tape}, ok.

q0_parse() ->
    receive
        {parse, [a | Rest], Original} -> q0 ! {parse, Rest, Original};
        {parse, [b | Rest], Original} -> q1 ! {parse, Rest, Original}, q2 ! {parse, Rest, Original}
    end,
    q0_parse().

q1_parse() ->
    receive
        {parse, [b | Rest], Original} -> q0 ! {parse, Rest, Original}
    end,
    q1_parse().
q2_parse() ->
    receive
        {parse, [b | Rest], Original} -> q3 ! {parse, Rest, Original}
    end,
    q2_parse().
q3_parse() ->
    receive
        {parse, [c | Rest], Original} -> q4 ! {parse, Rest, Original}
    end,
    q3_parse().
q4_parse() ->
    receive
        {parse, [], Original} -> io:format("Successfully parsed string ~p! ~n", [Original])
    end,
    q4_parse().