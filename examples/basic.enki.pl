#!/usr/bin/env swipl

:- use_module(library(clpfd)).

:- style_check(-singleton).
:- style_check(-no_effect).
:- style_check(-var_branches).
:- style_check(-discontiguous).
:- style_check(-charset).

% FuncType EnkiInt (FuncType EnkiInt EnkiInt)
add_to(X,Y,Temp0) :-
    Temp0 #= (X + Y).

% RuleType EnkiInt EnkiInt
half(X,H) :-
    Temp0 #= (2 * H),
    X = Temp0.

% FuncType EnkiInt EnkiInt
square_root(X,Root) :-
    Temp0 #= (Root ^ 2),
    Temp0 = X.
