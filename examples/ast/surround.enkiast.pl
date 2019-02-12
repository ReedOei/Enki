#!/usr/bin/env swipl

:- use_module(library(clpfd)).

:- style_check(-singleton).
:- style_check(-no_effect).
:- style_check(-var_branches).
:- style_check(-discontiguous).
:- style_check(-charset).

surround_with(X,Left,Right,Temp0) :-
    atom_concat(Left,X,Temp1),
    atom_concat(Temp1,Right,Temp0).

