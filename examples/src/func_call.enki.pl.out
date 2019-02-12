#!/usr/bin/env swipl

:- use_module(library(clpfd)).

:- style_check(-singleton).
:- style_check(-no_effect).
:- style_check(-var_branches).
:- style_check(-discontiguous).
:- style_check(-charset).

square_root(X,Root) :-
    Temp1 #= Root ^ 2,
    Temp1 = X.

distance_from_to(X1,Y1,X2,Y2,Temp0) :-
    Temp4 #= X1 - X2,
    Temp3 #= Temp4 ^ 2,
    Temp11 #= Y1 - Y2,
    Temp10 #= Temp11 ^ 2,
    Temp2 #= Temp3 + Temp10,
    square_root(Temp2,Temp0).


