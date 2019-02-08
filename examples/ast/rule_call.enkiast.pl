#!/usr/bin/env swipl

:- use_module(library(clpfd)).

divides(A,B) :-
    Temp2 #= N * A,
    B = Temp2.

even(X) :-
    divides(2,X).

