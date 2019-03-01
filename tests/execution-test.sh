#!/usr/bin/env bash

try_exec() {
    fname="$1"

    ./bin/enki "examples/src/$fname" > "examples/src/$fname.pl"
    swipl "examples/src/$fname.pl" > "temp"
    if diff -Bb "temp" "examples/src/$fname.out" > /dev/null; then
        test_pass "$fname"
    else
        test_fail "$fname" "$(cat examples/src/$fname.out)" "$(cat temp)"
    fi
    rm "temp"
}

try_exec "pe1.enki"
try_exec "pe2.enki"

