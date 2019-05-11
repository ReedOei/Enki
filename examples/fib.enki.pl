#!/usr/bin/env swipl

:- use_module(library(clpfd)).

:- style_check(-singleton).
:- style_check(-no_effect).
:- style_check(-var_branches).
:- style_check(-discontiguous).
:- style_check(-charset).

fib_is(N,F) :-
    (
        (
            Temp0 = (N #> 1)
            ->
            Temp1 #= (N - 1),
            fib_is(Temp1,F1,Temp2),
            Temp3 #= (N - 2),
            fib_is(Temp3,F2,Temp4),
            Temp5 #= (F1 + F2),
            F = Temp5
        )
        ;
        F = N
    ).
