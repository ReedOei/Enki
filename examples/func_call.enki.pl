#!/usr/bin/env swipl

:- use_module(library(clpfd)).

:- style_check(-singleton).
:- style_check(-no_effect).
:- style_check(-var_branches).
:- style_check(-discontiguous).
:- style_check(-charset).

square_root(X,Root) :-
    Temp0 #= (Root ^ 2),
    Temp0 = X.

distance_from_to(X1,Y1,X2,Y2,Temp0) :-
    Temp1 #= (X1 - X2),
    Temp2 #= (Temp1 ^ 2),
    Temp3 #= (Y1 - Y2),
    Temp4 #= (Temp3 ^ 2),
    Temp5 #= (Temp2 + Temp4),
    square_root(Temp5,Temp0).
