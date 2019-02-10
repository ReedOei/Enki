#!/usr/bin/env bash

make_func() {
    if [[ ! -z "$3" ]]; then
        echo "f($1, cs($2), e($3))"
    else
        echo "f($1, cs(nil), e($2))"
    fi
}

make_rule() {
    echo "r($1, cs($2))"
}

run_test "firstUnused(empty, 0)" "EnkiType: any(\"T0\")"
run_test "firstUnused(any(\"T0\"), 0)" "EnkiType: any(\"T1\")"
run_test "types(empty)" "Set{EnkiType}: (empty).Set{EnkiType}"
run_test "freshtype(freshtype(empty, v(\"X\")), v(\"Y\"))" "NeSet{TypedId}: tid(v(\"X\"), any(\"T0\")), tid(v(\"Y\"), any(\"T1\"))"
run_test "freshtypes(empty, v(\"X\") v(\"Y\") v(\"X\") v(\"Z\"))" "NeSet{TypedId}: tid(v(\"X\"), any(\"T0\")), tid(v(\"Y\"), any(\"T1\")), tid(v(\"Z\"), any(\"T2\"))"
run_test "varlist(comp(s(\"add\") v(\"X\") s(\"to\") v(\"Y\")))" "NeList{Id}: v(\"X\") v(\"Y\")"
run_test "attachVars(empty, varlist(comp(s(\"add\") v(\"X\") s(\"to\") v(\"Y\"))))" "NeList{TypedId}: tid(v(\"X\"), any(\"T0\")) tid(v(\"Y\"), any(\"T1\"))"
run_test "functype(attachVars(empty, varlist(comp(s(\"add\") v(\"X\") s(\"to\") v(\"Y\")))))" "EnkiType: func(any(\"T0\"), any(\"T1\"))"
run_test "functype(tid(v(\"S\"), int) tid(v(\"T\"), int))" "EnkiType: func(int, int)"
run_test "join(int, int)" "EnkiType: int"
run_test "join(int, any(\"T0\"))" "EnkiType: int"
run_test "join(bool, any(\"T0\"))" "EnkiType: bool"
run_test "unify(freshtypes(empty, v(\"X\") v(\"Y\")), v(\"X\"), v(\"Y\"))" "NeSet{TypedId}: tid(v(\"X\"), any(\"T1\")), tid(v(\"Y\"), any(\"T1\"))"
run_test "inferId(freshtypes(empty, v(\"X\") s(\"=\") v(\"Y\")), comp(v(\"X\") s(\"=\") v(\"Y\")))" "NeSet{TypedId}: tid(s(\"=\"), string), tid(v(\"X\"), any(\"T1\")), tid(v(\"Y\"), any(\"T1\")), tid(comp(v(\"X\") s(\"=\") v(\"Y\")), any(\"T1\"))"
run_test "inferId(freshtypes(empty, v(\"X\") s(\"=\") i(40)), comp(v(\"X\") s(\"=\") i(40)))" "NeSet{TypedId}: tid(s(\"=\"), string), tid(i(40), int), tid(v(\"X\"), int), tid(comp(v(\"X\") s(\"=\") i(40)), int)"
run_test "inferId(freshtypes(empty, idList(comp(v(\"X\") s(\"+\") v(\"Y\")))), comp(v(\"X\") s(\"+\") v(\"Y\")))" "NeSet{TypedId}: tid(s(\"+\"), string), tid(v(\"X\"), int), tid(v(\"Y\"), int), tid(comp(v(\"X\") s(\"+\") v(\"Y\")), int)"

add_fid="comp(s(\"add\") v(\"X\") s(\"to\") v(\"Y\"))"
add_body="comp(v(\"X\") s(\"+\") v(\"Y\"))"
add_type="func(int, func(int, int))"
add_f="$(make_func "$add_fid" "$add_body")"

add_twice_fid="comp(s(\"addTwice\") v(\"X\") s(\"to\") v(\"Y\"))"
add_twice_body="comp(s(\"add\") v(\"X\") s(\"to\") comp(s(\"add\") v(\"X\") s(\"to\") v(\"Y\")))"
add_twice_type="func(int, func(int, int))"
add_twice_f="$(make_func "$add_twice_fid" "$add_twice_body")"

run_test "inferFunc(empty, $add_f)" "TypedFunc: typed($add_fid, $add_type, typedConstraints(nil), typedExpr(tid($add_body, int)))"
run_test "findFuncType(tid(fid($add_fid), $add_type), comp(s(\"add\") i(10) s(\"to\") comp(v(\"X\") s(\"+\") v(\"Y\"))))" "TypedId: tid(comp(s(\"add\") v(\"X\") s(\"to\") v(\"Y\")), func(int, func(int, int)))"
run_test "inferDefs(empty, def($add_f) def($add_twice_f))" "NeList{TypedDef}: def(typed($add_fid, $add_type, typedConstraints(nil), typedExpr(tid($add_body, int)))) def(typed($add_twice_fid, $add_twice_type, typedConstraints(nil), typedExpr(tid(fcall($add_fid, func(int, func(int, int)), (\"X\" |-> v(\"X\"), \"Y\" |-> fcall($add_fid, func(int, func(int, int)), (\"X\" |-> v(\"X\"), \"Y\" |-> v( \"Y\"))))), int))))"
run_test "genFunc(inferFunc(empty, $add_f))" "NeList{Line}: hornline(\"add_to(X,Y,Temp0)\") andline(\"Temp0#=X+Y\")"

distance_fid="comp(s(\"distance\") s(\"from\") v(\"X1\") v(\"Y1\") s(\"to\") v(\"X2\") v(\"Y2\"))"
distance_body="comp(comp(comp(v(\"X1\") s(\"-\") v(\"X2\")) s(\"^\") i(2)) s(\"+\") comp(comp(v(\"Y1\") s(\"-\") v(\"Y2\")) s(\"^\") i(2)))"
distance_type="func(int, func(int, func(int, func(int, int))))"
distance_f="$(make_func "$distance_fid" "$distance_body")"

run_test "resolve(tid(fid($add_fid), $add_type), tid($distance_body, int))" "TypedId: tid($distance_body, int)"

run_test "genDefs(inferDefs(empty, def($add_f) def($add_twice_f)))" "String:\"add_to(X,Y,Temp0):-\nTemp0#=X+Y.\n\naddTwice_to(X,Y,Temp0):-\nadd_to(X,Y,Temp4),\nadd_to(X,Temp4,Temp0).\n\n\""

increment_fid="comp(s(\"increment\") v(\"X\"))"
increment_body="comp(s(\"add\") v(\"X\") s(\"to\") i(1))"
increment_type="func(int, int)"
increment_f="$(make_func "$increment_fid" "$increment_body")"

run_test "genDefs(inferDefs(empty, def($add_f) def($increment_f)))" "String:\"add_to(X,Y,Temp0):-\nTemp0#=X+Y.\n\nincrement(X,Temp0):-\nadd_to(X,1,Temp0).\n\n\""

run_test "generate(tid(v(\"X\"), int), 0)" "GenVal: genVal(nil, \"X\", 0)"
run_test "generate(tid(i(102321), int), 0)" "GenVal: genVal(nil, \"102321\", 0)"
run_test "generate(tid(b(true), bool), 0)" "GenVal: genVal(nil, \"true\", 0)"
run_test "generate(tid(b(false), bool), 0)" "GenVal: genVal(nil, \"false\", 0)"
run_test "generate(tid(s(\"test\"), string), 0)" "GenVal: genVal(nil, \"test\", 0)"
run_test "generate(tid(s(\"+\"), string), 0)" "GenVal: genVal(nil, \"+\", 0)"
run_test "generate(tid(comp(s(\"blah\")), string), 0)" "GenVal: genVal(nil, \"blah\", 0)"
run_test "generate(tid(comp(v(\"Y\")), string), 0)" "GenVal: genVal(nil, \"Y\", 0)"
run_test "generate(tid(comp(v(\"X\") s(\"+\") v(\"Y\")), int), 0)" "GenVal: genVal(andline(\"Temp0 #= X + Y\"), \"Temp0\", 3)"

surround_fid="comp(s(\"surround\") v(\"X\") s(\"with\") v(\"Left\") v(\"Right\"))"
surround_body="comp(comp(v(\"Left\") s(\"..\") v(\"X\")) s(\"..\") v(\"Right\"))"
surround_type="func(string, func(string, func(string, string)))"
surround_f="$(make_func "$surround_fid" "$surround_body")"

run_test "inferFunc(empty, $surround_f)" "TypedFunc:typed(comp(s(\"surround\")v(\"X\")s(\"with\")v(\"Left\")v(\"Right\")),func(string,func(string,func(string,string))),typedConstraints(nil),typedExpr(tid(comp(comp(v(\"Left\")s(\"..\")v(\"X\"))s(\"..\")v(\"Right\")),string)))"


half_fid="comp(s(\"half\") v(\"X\"))"
half_const="comp(v(\"X\") s(\"=\") comp(i(2) s(\"*\") v(\"N\")))"
half_body="v(\"N\")"
half_type="func(int, int)"
half_f="$(make_func "$half_fid" "c($half_const)" "$half_body")"

run_test "inferFunc(empty, $half_f)" "TypedFunc: typed($half_fid, $half_type, typedConstraint(tid($half_const, int)), typedExpr(tid($half_body, int)))"

even_rid="comp(s(\"even\") v(\"X\"))"
even_body="comp(v(\"X\") s(\"=\") comp(i(2) s(\"*\") v(\"X\")))"
even_type="int"
even_r="$(make_rule "$even_rid" "c($even_body)")"

run_test "inferRule(empty, $even_r)" "TypedRule: typed($even_rid, $even_type, typedConstraint(tid($even_body, int)))"

divides_rid="comp(v(\"A\") s(\"divides\") v(\"B\"))"
divides_body="comp(v(\"A\") s(\"=\") comp(v(\"N\") s(\"*\") v(\"B\")))"
divides_type="rule(int, int)"
divides_r="$(make_rule "$divides_rid" "c($divides_body)")"

run_test "inferRule(empty, $divides_r)" "TypedRule: typed($divides_rid, $divides_type, typedConstraint(tid($divides_body, int)))"

pair_const="comp(s(\"pair\") v(\"X\") v(\"Y\"))"

run_test "constructorTypes(s(\"pair\"), constructor($pair_const, field(s(\"X\"), int) field(s(\"Y\"), int)))" "TypedId: tid(fid($pair_const), data(int, data(int, type(s(\"pair\")))))"

cons_const="comp(s(\"cons\") v(\"Head\") v(\"Tail\"))"
list_def="def(d(s(\"list\"), constructor(comp(s(\"empty\")), nil) constructor($cons_const, field(s(\"Head\"), int) field(s(\"Tail\"), type(s(\"list\"))))))"

