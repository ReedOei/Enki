#!/usr/bin/env swipl

:- use_module(library(clpfd)).

:- style_check(-singleton).
:- style_check(-no_effect).
:- style_check(-var_branches).
:- style_check(-discontiguous).
:- style_check(-charset).

sum(N,Temp0) :-
    Temp3 #= N - 1,
    sum(Temp3,Temp1),
    Temp0 #= Temp1 + N.


