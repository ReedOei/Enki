#!/usr/bin/env swipl

:- use_module(library(clpfd)).

prepend(X,List,cons(X,List)).

head(List,Head) :-
    List = cons(Head,Tail).

