#!/usr/bin/env swipl

:- use_module(library(clpfd)).

test(X,X) :-
    atom_concat(X,X,Temp2),
    writeln(Temp2).

