#!/usr/bin/env swipl

:- use_module(library(clpfd)).

:- style_check(-singleton).
:- style_check(-no_effect).
:- style_check(-var_branches).
:- style_check(-discontiguous).
:- style_check(-charset).

construct(Head,Full,Tail) :-
    Full = cons(Head,Tail).

sum_is(List,N) :-
    (
    List = cons(H,T) -> 
    Temp13 #= N - H,
    sum_is(T,Temp13)
    );
    (
    N = 0
    ).

inc_is(List,NewList) :-
    (
    List = cons(H,T) -> 
    Temp15 #= H + 1,
    construct(Temp15,NewList,Temp13),
    inc_is(T,Temp13)
    );
    (
    NewList = empty
    ).

zip_and_is(A,B,Zipped) :-
    (
    A = cons(HA,TA),
    B = cons(HB,TB) -> 
    construct(pair_and(HA,HB),Zipped,Temp24),
    zip_and_is(TA,TB,Temp24)
    );
    (
    Zipped = empty
    ).


