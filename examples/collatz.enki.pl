#!/usr/bin/env swipl

:- use_module(library(clpfd)).

:- style_check(-singleton).
:- style_check(-no_effect).
:- style_check(-var_branches).
:- style_check(-discontiguous).
:- style_check(-charset).

collatz_is(X,N) :-
    (
    Temp3 #= 2 * K,
    Temp2 #= Temp3 + 1,
    X = Temp2 -> 
    Temp12 #= 3 * X,
    Temp11 #= Temp12 + 1,
    N = Temp11
    );
    (
    Temp20 #= 2 * N,
    X = Temp20
    ).

collatz_sequence_on_takes_steps(X,N) :-
    (
    X = 1 -> 
    N = 0
    );
    (
    collatz_is(X,K),
    Temp16 #= N - 1,
    collatz_sequence_on_takes_steps(K,Temp16)
    ).


