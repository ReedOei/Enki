#!/usr/bin/env bash

make_func() {
    echo "f($1, e($2))"
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

run_test "inferFunc(empty, f($add_fid, e($add_body)))" "TypedFunc: typedf($add_fid, $add_type, typedExpr(tid($add_body, int)))"
run_test "findFuncType(tid(fid($add_fid), $add_type), comp(s(\"add\") i(10) s(\"to\") comp(v(\"X\") s(\"+\") v(\"Y\"))))" "TypedId: tid(comp(s(\"add\") v(\"X\") s(\"to\") v(\"Y\")), func(int, func(int, int)))"
run_test "inferFuncs(empty, $add_f $add_twice_f)" "NeList{TypedFunc}: typedf($add_fid, $add_type, typedExpr(tid($add_body, int))) typedf($add_twice_fid, $add_twice_type, typedExpr(tid(fcall($add_fid, int int int, (\"X\" |-> v(\"X\"), \"Y\" |-> fcall($add_fid, int int int, (\"X\" |-> v(\"X\"), \"Y\" |-> v( \"Y\"))))), int)))"
run_test "genFunc(inferFunc(empty, $add_f))" "NeList{String}: \"add_to(X,Y,Temp0) :-\" \"Temp0#=X+Y\""

distance_fid="comp(s(\"distance\") s(\"from\") v(\"X1\") v(\"Y1\") s(\"to\") v(\"X2\") v(\"Y2\"))"
distance_body="comp(comp(comp(v(\"X1\") s(\"-\") v(\"X2\")) s(\"^\") i(2)) s(\"+\") comp(comp(v(\"Y1\") s(\"-\") v(\"Y2\")) s(\"^\") i(2)))"
distance_type="func(int, func(int, func(int, func(int, int))))"
distance_f="$(make_func "$distance_fid" "$distance_body")"

run_test "resolve(tid(fid($add_fid), $add_type), tid($distance_body, int))" "TypedId: tid($distance_body, int)"

run_test "genFunc(inferFunc(empty, $distance_f))" "NeList{String}:\"distance_from_to(X1,Y1,X2,Y2,Temp0):-\" \"Temp2#=X1-X2\" \"Temp1#=Temp2^2\" \"Temp9#=Y1-Y2\" \"Temp8#=Temp9^2\" \"Temp0#=Temp1+Temp8\""

run_test "genFuncs(inferFuncs(empty, $add_f $add_twice_f))" "String:\"add_to(X,Y,Temp0):-\nTemp0#=X+Y.\n\naddTwice_to(X,Y,Temp0):-\nadd_to(X,Y,Temp4),\nadd_to(X,Temp4,Temp0).\n\n\""

increment_fid="comp(s(\"increment\") v(\"X\"))"
increment_body="comp(s(\"add\") v(\"X\") s(\"to\") i(1))"
increment_type="func(int, int)"
increment_f="$(make_func "$increment_fid" "$increment_body")"

run_test "genFuncs(inferFuncs(empty, $add_f $increment_f))" "String:\"add_to(X,Y,Temp0):-\nTemp0#=X+Y.\n\nincrement(X,Temp0):-\nadd_to(X,1,Temp0).\n\n\""

run_test "generate(tid(v(\"X\"), int), 0)" "GenVal: genVal(nil, \"X\", 0)"
run_test "generate(tid(i(102321), int), 0)" "GenVal: genVal(nil, \"102321\", 0)"
run_test "generate(tid(b(true), bool), 0)" "GenVal: genVal(nil, \"true\", 0)"
run_test "generate(tid(b(false), bool), 0)" "GenVal: genVal(nil, \"false\", 0)"
run_test "generate(tid(s(\"test\"), string), 0)" "GenVal: genVal(nil, \"test\", 0)"
run_test "generate(tid(s(\"+\"), string), 0)" "GenVal: genVal(nil, \"+\", 0)"
run_test "generate(tid(comp(s(\"blah\")), string), 0)" "GenVal: genVal(nil, \"blah\", 0)"
run_test "generate(tid(comp(v(\"Y\")), string), 0)" "GenVal: genVal(nil, \"Y\", 0)"
run_test "generate(tid(comp(v(\"X\") s(\"+\") v(\"Y\")), int), 0)" "GenVal: genVal(\"Temp0 #= X + Y\", \"Temp0\", 3)"

surround_fid="comp(s(\"surround\") v(\"X\") s(\"with\") v(\"Left\") v(\"Right\"))"
surround_body="comp(comp(v(\"Left\") s(\"..\") v(\"X\")) s(\"..\") v(\"Right\"))"
surround_type="func(string, func(string, func(string, string)))"
surround_f="$(make_func "$surround_fid" "$surround_body")"

run_test "inferFunc(empty, $surround_f)" "TypedFunc:typedf(comp(s(\"surround\")v(\"X\")s(\"with\")v(\"Left\")v(\"Right\")),func(string,func(string,func(string,string))),typedExpr(tid(comp(comp(v(\"Left\")s(\"..\")v(\"X\"))s(\"..\")v(\"Right\")),string)))"

