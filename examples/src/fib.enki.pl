#!/usr/bin/env swipl

:- use_module(library(clpfd)).

fib_is(N,F) :-
    (
    N #> 1 -> 
    Temp5 #= N - 1,
    fib_is(Temp5,F1),
    Temp14 #= N - 2,
    fib_is(Temp14,F2),
    Temp23 #= F1 + F2,
    F = Temp23
    );
    (
    F = N
    ).


