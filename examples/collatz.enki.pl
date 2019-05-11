#!/usr/bin/env swipl

:- use_module(library(clpfd)).

:- style_check(-singleton).
:- style_check(-no_effect).
:- style_check(-var_branches).
:- style_check(-discontiguous).
:- style_check(-charset).

collatz_is(X,N) :-
    (
        (
            Temp0 #= (2 * K),
            Temp1 #= (Temp0 + 1),
            X = Temp1
            ->
            Temp2 #= (3 * X),
            Temp3 #= (Temp2 + 1),
            N = Temp3
        )
        ;
            Temp4 #= (2 * N),
            X = Temp4
    ).

collatz_sequence_on_takes_steps(X,N) :-
    (
        (
            X = 1
            ->
            N = 0
        )
        ;
            collatz_is(X,K),
            Temp0 #= (N - 1),
            collatz_sequence_on_takes_steps(K,Temp0)
    ).
