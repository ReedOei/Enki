#!/usr/bin/env swipl

:- use_module(library(clpfd)).

surround_with(X,Left,Right,Temp0) :-
    atom_concat(Left,X,Temp1),
    atom_concat(Temp1,Right,Temp0).

resolve_path(Parent,Child,Temp0) :-
    atom_concat(Parent,"/",Temp1),
    atom_concat(Temp1,Child,Temp0).

temp(X,Temp0) :-
    atom_concat(X,X,Temp0).

calllots(X,Y,Temp0) :-
    resolve_path(X,Y,Temp2),
    temp(X,Temp18),
    temp(Temp18,Temp16),
    temp(Temp16,Temp14),
    resolve_path(X,Temp14,Temp10),
    resolve_path(Temp2,Temp10,Temp0).

