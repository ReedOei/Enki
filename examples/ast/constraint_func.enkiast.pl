#!/usr/bin/env swipl

:- use_module(library(clpfd)).

half(X,N) :-
    Temp2 #= 2 * N,
    X = Temp2.

