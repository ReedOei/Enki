#!/usr/bin/env swipl

:- use_module(library(clpfd)).

:- style_check(-singleton).
:- style_check(-no_effect).
:- style_check(-var_branches).
:- style_check(-discontiguous).
:- style_check(-charset).

prepend(X,List,cons(X,List)).

head(List,Head) :-
    List = cons(Head,Tail).

