#!/usr/bin/env swipl

:- use_module(library(clpfd)).

:- style_check(-singleton).
:- style_check(-no_effect).
:- style_check(-var_branches).
:- style_check(-discontiguous).
:- style_check(-charset).


map_built_in(F,empty(), empty()).
map_built_in(F,cons(H,T),cons(NewH,NewT)) :-
    call(F, H, NewH),
    map_built_in(F, T, NewT).



% RuleType EnkiInt EnkiInt
collatz_is(X,N) :-
    (
            Temp0 #= (2 * K),
            Temp1 #= (Temp0 + 1),
            X = Temp1
            ->
            Temp2 #= (3 * X),
            Temp3 #= (Temp2 + 1),
            N = Temp3
        ;
            Temp4 #= (2 * N),
            X = Temp4
    ).

% RuleType EnkiInt EnkiInt
collatz_sequence_on_takes_steps(X,N) :-
    (
            X = 1
            ->
            N = 0
        ;
            collatz_is(X,K),
            Temp7 #= (N - 1),
            collatz_sequence_on_takes_steps(K,Temp7)
    ).
