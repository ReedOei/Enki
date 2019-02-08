#!/usr/bin/env swipl

% :- initialization(main, main).

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)) .
:- use_module(library(pio)) .
:- use_module(library(filesex)) .
:- use_module(library(achelois)) .

main(Argv) :-
    Argv = [] ->
        writeln('Usage: ./analyze-maude.pl FILE1 FILE2 ...'),
        writeln('FILE(S) - The files to analyze');

    forall(member(File, Argv), analyze_file(File)) .

analyze_file(File) :-
    find_singleton_vars(File).

find_singleton_vars(File) :-
    phrase_from_file(maude_var_dec(VarsCodes), File),
    maplist(atom_codes, Vars, VarsCodes),
    findall(Var,
    (
        member(Var, Vars),
        count_all(Var, File, 1)
    ), SingletonVars),
    format('Singleton variables: ~w', [SingletonVars]).

count_all(Var, File, N) :-
    atom_codes(Var, VarCodes),
    findall(Num, phrase_from_file(count_occurrences(VarCodes, Num), File), Counts),
    max_list(Counts, N).

count_occurrences(Str, N) -->
    string(_),
    string(Str),
    (")"; ","; " "),
    count_occurrences(Str, N1),
    { N #= N1 + 1 } .
count_occurrences(Str, 0) -->
    string_without(Str, _),
    remainder(_).

maude_var_dec(Vars) -->
    string(_),
    ("var "; "vars "),
    whites,
    maude_vars(Vars1),
    maude_var_dec(Vars2),
    { append(Vars1, Vars2, Vars) } .
maude_var_dec([]) -->
    remainder(_).

maude_vars([]) --> whites, ":" .
maude_vars([Var|Vars]) -->
    string_without(" :", Var),
    " ",
    whites,
    maude_vars(Vars) .

