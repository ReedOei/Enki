#!/usr/bin/env swipl

:- use_module(library(clpfd)).

surround_with(X,Left,Right,Temp0) :-
    atom_concat(Left,X,Temp1),
    atom_concat(Temp1,Right,Temp0).

