#!/usr/bin/env bash

set -e

if [[ -z "$1" ]] || [[ -z "$2" ]]; then
    echo "Usage: bash test.sh MAUDE_FILE TEST_CASES"
    echo "MAUDE_FILE - The main file to reduce in."
    echo "TEST_CASES - Files containing lines, separated by ~, where the first field is a Maude expression to reduce, and the second part is the expected result, and the (optional) third part is the module to reduce in"
    exit 1
fi

print_color_num() {
    cnum="$1"
    msg="$2"
    flags="$3"

    tput setaf "$cnum"
    echo $flags "$msg"
    tput sgr0
}

print_color() {
    color="$1"
    msg="$2"
    flags="$3"
    cnum=0

    if [[ "$color" == "red" ]]; then
        cnum=1
    elif [[ "$color" == "green" ]]; then
        cnum=2
    elif [[ "$color" == "yellow" ]]; then
        cnum=3
    elif [[ "$color" == "blue" ]]; then
        cnum=4
    elif [[ "$color" == "magenta" ]]; then
        cnum=5
    elif [[ "$color" == "cyan" ]]; then
        cnum=6
    elif [[ "$color" == "white" ]]; then
        cnum=7
    fi

    print_color_num "$cnum" "$msg" "$flag"
}

test_pass() {
    test_name="$1"

    export PASSED="$((PASSED + 1))"
    print_color "green" "Passed: $test_name;"
}

test_fail() {
    test_name="$1"
    expected="$2"
    result="$3"

    export FAILED="$((FAILED + 1))"
    print_color "red" "Failed: $test_name;"
    print_color "red" "Expected: $expected"
    print_color "red" "Got     : $result"
}

run_test() {
    red="$1"
    expected="$(echo "$2" | tr -d "[:space:]")"
    module="$3"

    if [[ ! -z "$red" ]] && [[ ! -z "$expected" ]]; then
        red_str="reduce $red ."
        if [[ ! -z "$module" ]]; then
            red_str="reduce in $module : $red ."
        fi

        result="$(echo "reduce $red ." | maude -no-banner -no-wrap "$maude_file" | sed -n -e '/result /,$p' | head -n -1 | sed -E "s/result //g" | tr -d "[:space:]")"

        if [[ "$result" == "$expected" ]]; then
            test_pass "$red_str"
        else
            test_fail "$red_str" "$expected" "$result"
        fi
    fi
}

export PASSED=0
export FAILED=0

export maude_file="$1"
test_cases="$2"

if [[ ! "$maude_file" =~ "/.*" ]]; then
    maude_file="$(pwd)/$maude_file"
fi

export -f run_test
export -f print_color
export -f print_color_num

echo "Started at: $(date)"
start_time="$(date +"%s")"

for f in "${@:2}"; do
    source "$f" # Source to get the passed/failed counts
done

echo "Passed: $PASSED, Failed: $FAILED"
end_time="$(date +"%s")"
echo "Elapsed: $((end_time - start_time)) seconds"
echo "Finished at: $(date)"

if [[ "$FAILED" -gt 0 ]]; then
    exit 1
fi

