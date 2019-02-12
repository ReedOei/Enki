#!/usr/bin/env swipl

:- use_module(library(clpfd)).

sum_is(List,N) :-
    (
    List = cons(H,T) -> 
    Temp13 #= N - H,
    sum_is(T,Temp13)
    );
    (
    N = 0 -> true
    ).

inc_is(List,NewList) :-
    (
    List = cons(H,T) -> 
    inc_is(T,NewT),
    Temp19 #= H + 1,
    NewList = cons(Temp19,NewT)
    );
    (
    NewList = empty -> true
    ).

tail_is(List,T) :-
    (
    List = cons(H,T) -> true
    );
    (
    empty,
    T = Temp11 -> true
    ).

zip_and_is(A,B,Zipped) :-
    (
    A = cons(HA,TA),
    B = cons(HB,TB) -> 
    zip_and_is(TA,TB,TZipped),
    Zipped = cons(pair_and(HA,HB),TZipped)
    );
    (
    Zipped = empty -> true
    ).


