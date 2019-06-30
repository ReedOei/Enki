#!/usr/bin/env swipl

:- use_module(library(clpfd)).

:- style_check(-singleton).
:- style_check(-no_effect).
:- style_check(-var_branches).
:- style_check(-discontiguous).
:- style_check(-charset).

% FuncType EnkiInt EnkiInt
step_collatz(X,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            Temp0 #= (2 * K),
            Temp1 #= (Temp0 + 1),
            X = Temp1
            ->
            Temp2 #= (3 * X),
            Temp3 #= (Temp2 + 1),
            AUTOGENERATEDFUNCTIONRESULT = Temp3
        ;
            Temp4 #= (2 * N),
            X = Temp4,
            AUTOGENERATEDFUNCTIONRESULT = N
    ).

% RuleType EnkiInt EnkiInt
collatz_sequence_on_takes_steps(X,N) :-
    (
            X = 1
            ->
            N = 0
        ;
            step_collatz(X,Temp7),
            Temp6 #= (N - 1),
            collatz_sequence_on_takes_steps(Temp7,Temp6)
    ).
