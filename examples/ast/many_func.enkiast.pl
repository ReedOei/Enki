#!/usr/bin/env swipl

:- use_module(library(clpfd)).

:- style_check(-singleton).
:- style_check(-no_effect).
:- style_check(-var_branches).
:- style_check(-discontiguous).
:- style_check(-charset).

add_to(X,Y,Temp0) :-
    Temp0 #= X + Y.

increment(X,Temp0) :-
    add_to(X,1,Temp0).

addTwice_to(X,Y,Temp0) :-
    add_to(X,Y,Temp4),
    add_to(X,Temp4,Temp0).

distance_from_to(X1,Y1,X2,Y2,Temp0) :-
    Temp2 #= X1 - X2,
    Temp1 #= Temp2 ^ 2,
    Temp9 #= Y1 - Y2,
    Temp8 #= Temp9 ^ 2,
    Temp0 #= Temp1 + Temp8.

