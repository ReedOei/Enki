#!/usr/bin/env swipl

:- use_module(library(clpfd)).

even(X) :-
    Temp2 #= 2 * X,
    X = Temp2.

divides(A,B) :-
    Temp2 #= N * A,
    B = Temp2.

