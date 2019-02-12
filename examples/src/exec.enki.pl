#!/usr/bin/env swipl

:- use_module(library(clpfd)).

:- style_check(-singleton).
:- style_check(-no_effect).
:- style_check(-var_branches).
:- style_check(-discontiguous).
:- style_check(-charset).

:- initialization(main, main).

main(Args) :-
    writeln(4),
    inc(10,Temp6),
    writeln(Temp6).



inc(X,Temp0) :-
    Temp0 #= X + 1.


