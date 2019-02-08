#!/usr/bin/env bash

try_run() {
    fname="$1"

    ./bin/enki "examples/ast/$fname" > temp
    if diff -Bb temp "examples/ast/$fname.pl" > /dev/null; then
        test_pass "$fname"
    else
        test_fail "$fname" "$(cat examples/ast/$fname.pl)" "$(cat temp)"
    fi

    rm temp
}

try_run "add.enkiast"
try_run "distance.enkiast"
try_run "many_func.enkiast"
try_run "surround.enkiast"
try_run "multistringop.enkiast"
try_run "constraint_func.enkiast"
try_run "basic_rule.enkiast"
try_run "rule_call.enkiast"

