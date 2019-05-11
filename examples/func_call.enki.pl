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

distance_from_to(X1,Y1,X2,Y2,Temp5) :-
    Temp0 #= (X1 - X2),
    Temp1 #= (Temp0 ^ 2),
    Temp2 #= (Y1 - Y2),
    Temp3 #= (Temp2 ^ 2),
    Temp4 #= (Temp1 + Temp3),
    square_root(Temp4,Temp5).
