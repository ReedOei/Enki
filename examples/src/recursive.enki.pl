#!/usr/bin/env swipl

:- use_module(library(clpfd)).

sum(N,Temp0) :-
    Temp3 #= N - 1,
    sum(Temp3,Temp1),
    Temp0 #= Temp1 + N.


