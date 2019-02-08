#!/usr/bin/env bash

try_run() {
    fname="$1"

    ./scripts/compile.pl "examples/ast/$fname" > temp
    if diff -Bb temp "examples/ast/$fname.out" > /dev/null; then
        test_pass "$fname"
    else
        test_fail "$fname" "$(cat examples/ast/$fname.out)" "$(cat temp)"
    fi

    rm temp
}

try_run "add.enkiast"
try_run "distance.enkiast"
try_run "many_func.enkiast"

