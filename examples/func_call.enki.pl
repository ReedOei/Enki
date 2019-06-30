#!/usr/bin/env swipl

:- use_module(library(clpfd)).

:- style_check(-singleton).
:- style_check(-no_effect).
:- style_check(-var_branches).
:- style_check(-discontiguous).
:- style_check(-charset).

% FuncType EnkiInt EnkiInt
square_root(X,Root) :-
    Temp0 #= (Root ^ 2),
    Temp0 = X.

% FuncType EnkiInt (FuncType EnkiInt (FuncType EnkiInt (FuncType EnkiInt EnkiInt)))
distance_from_to(X1,Y1,X2,Y2,Temp1) :-
    Temp2 #= (X1 - X2),
    Temp3 #= (Temp2 ^ 2),
    Temp4 #= (Y1 - Y2),
    Temp5 #= (Temp4 ^ 2),
    Temp6 #= (Temp3 + Temp5),
    square_root(Temp6,Temp1).
