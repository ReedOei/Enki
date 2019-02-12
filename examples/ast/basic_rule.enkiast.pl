#!/usr/bin/env swipl

:- use_module(library(clpfd)).

:- style_check(-singleton).
:- style_check(-no_effect).
:- style_check(-var_branches).
:- style_check(-discontiguous).
:- style_check(-charset).

even(X) :-
    Temp2 #= 2 * X,
    X = Temp2.

divides(A,B) :-
    Temp2 #= N * A,
    B = Temp2.

