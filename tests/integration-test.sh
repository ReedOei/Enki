#!/usr/bin/env bash

try_compile() {
    fname="$1"

    ./bin/enki "examples/src/$fname" > "examples/src/$fname.pl"
    if diff -Bb "examples/src/$fname.pl" "examples/src/$fname.pl.out" > /dev/null; then
        test_pass "$fname"
    else
        test_fail "$fname" "$(cat examples/src/$fname.pl.out)" "$(cat examples/src/$fname.pl)"
    fi
}

try_compile "basic.enki"
try_compile "collatz.enki"
try_compile "func_call.enki"
try_compile "recursive.enki"
try_compile "lists.enki"
try_compile "fib.enki"
try_compile "collatz_func.enki"

