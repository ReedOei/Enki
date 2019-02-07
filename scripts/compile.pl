#!/usr/bin/env swipl

:- initialization(main, main).

:- use_module(library(filesex)).
:- use_module(library(achelois)).

root(Root) :-
    source_file(File),
    atom_concat(_, 'scripts/compile.pl', File),
    file_directory_name(File, Root).

main(Argv) :-
    root(Root),
    parent_path(Root, ProjRoot),
    directory_file_path(ProjRoot, 'enki.maude', EnkiMain),
    Argv = [File|_] -> forall(member(File, Argv), process_file(EnkiMain, File));

    writeln('Usage: ./compile.pl FILE'),
    writeln('FILE - The file to compile. Should be either a .enki file or a .enkiast file').

process_file(EnkiMain, File) :-
    file_name_extension(_, 'enki', File) -> parse_and_compile(EnkiMain, File);
    file_name_extension(_, 'enkiast', File) -> compile(EnkiMain, File);
    format('Unknown extension on file: ~w~n', [File]).

parse_and_compile(EnkiMain, File) :-
    file_name_extension(Base, _, File),
    file_name_extension(Base, 'enkiast', OutputFile),
    parse_file(File, OutputFile),
    compile(EnkiMain, OutputFile).

parse_file(File, OutputFile) :-
    format('Tried to parse ~w to ~w~n. Parsing is currently unimplemented.', [File, OutputFile]).

maude_run(EnkiMain, Str, Result) :-
    atomic_list_concat(['reduce in ENKI : ', Str, ' .'], '', CompileStr),
    process(path(maude), ['-no-banner', '-no-wrap', '-no-advise', EnkiMain], [input(CompileStr), lines(Lines)]),
    member(Line, Lines),
    atom_concat('result ', WithType, Line),
    atom_concat(TypeWithColon, Result, WithType),
    atom_concat(_Type, ': ', TypeWithColon).

% From a file with each definition on a separate line, builds a
% Maude list with a call to the proper functions to compile them
compile_string(Str, CompileStr) :-
    atomic_list_concat(Temp, '\n', Str),
    include(\=(''), Temp, Definitions),
    atomic_list_concat(Definitions, ' ', DefList),
    surround_atom('genFuncs(inferFuncs(empty, ', '))', DefList, CompileStr).

compile_result(Result, CompileResult) :-
    surround_atom('"', '"', InnerStr, Result),
    % Get rid of the escaped newlines, and replace them with regular newlines
    atomic_list_concat(Lines, '\\n', InnerStr),
    atomic_list_concat(Lines, '\n', CompileResult).

compile(EnkiMain, File) :-
    read_file(File, Str),
    compile_string(Str, CompileStr),
    maude_run(EnkiMain, CompileStr, Result),
    compile_result(Result, CompileResult),
    writeln(CompileResult).

