#!/usr/bin/env bash

make_func() {
    echo "f($1, e($2))"
}

run_test "firstUnused(empty, 0)" "EnkiType: any(\"T0\")"
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

run_test "inferId((tid(s(\"add\"), string), tid(s(\"addTwice\"), string), tid(s(\"to\"), string), tid(v(\"X\"), any(\"T0\")), tid(v(\"Y\"), any(\"T1\")), tid(fid($add_fid), $add_type)), $add_twice_body)" "NeSet{TypedId}: tid(s(\"add\"), string), tid(s(\"addTwice\"), string), tid(s(\"to\"), string), tid(v(\"X\"), int), tid(v(\"Y\"), int), tid(fid($add_fid), $add_type), tid($add_twice_body, int)"
run_test "inferFunc(empty, f($add_fid, e($add_body)))" "TypedFunc: typedf($add_fid, $add_type, typedExpr(tid($add_body, int)))"
run_test "findFuncType(tid(fid($add_fid), $add_type), comp(s(\"add\") i(10) s(\"to\") comp(v(\"X\") s(\"+\") v(\"Y\"))))" "TypedId: tid(comp(s(\"add\") v(\"X\") s(\"to\") v(\"Y\")), func(int, func(int, int)))"
run_test "inferFuncs(empty, f($add_fid, e($add_body)) f($add_twice_fid, e($add_twice_body)))" "NeList{TypedFunc}: typedf(comp(s(\"add\") v(\"X\") s(\"to\") v(\"Y\")), func(int, func(int, int)), typedExpr(tid(comp(v(\"X\") s(\"+\") v(\"Y\")), int))) typedf(comp(s(\"addTwice\") v(\"X\") s(\"to\") v(\"Y\")), func(int, func(int, int)), typedExpr(tid(fcall(comp(s(\"add\") v(\"X\") s(\"to\") v(\"Y\")), int int int, (\"X\" |-> v(\"X\"), \"Y\" |-> fcall(comp(s(\"add\") v(\"X\") s(\"to\") v(\"Y\")), int int int, (\"X\" |-> v(\"X\"), \"Y\" |-> v( \"Y\"))))), int)))"
run_test "genId($add_body)" "String: \"(X + Y)\""
run_test "genFunc(inferFunc(empty, f($add_fid, e($add_body))))" "String: \"add_to(X,Y,Result_add_to) :- Result_add_to #= (X + Y)\""

distance_fid="comp(s(\"distance\") s(\"from\") v(\"X1\") v(\"Y1\") s(\"to\") v(\"X2\") v(\"Y2\"))"
distance_body="comp(comp(comp(v(\"X1\") s(\"-\") v(\"X2\")) s(\"^\") i(2)) s(\"+\") comp(comp(v(\"Y1\") s(\"-\") v(\"Y2\")) s(\"^\") i(2)))"
distance_type="func(int, func(int, func(int, func(int, int))))"
distance_f="$(make_func "$distance_fid" "$distance_body")"

run_test "resolve(tid(fid($add_fid), $add_type), tid($distance_body, int))" "TypedId: tid($distance_body, int)"

run_test "genFunc(inferFunc(empty, f($distance_fid, e($distance_body))))" "String: \"distance_from_to(X1,Y1,X2,Y2,Result_distance_from_to) :- Result_distance_from_to #= (((X1 - X2) ^ 2) + ((Y1 - Y2) ^ 2))\""

run_test "genFuncs(inferFuncs(empty, f($add_fid, e($add_body)) f($add_twice_fid, e($add_twice_body))))" "String : \"add_to(X,Y,Result_add_to) :- Result_add_to #= (X + Y).\n\naddTwice_to(X,Y,Result_addTwice_to) :- \nTemp2 #= Y,\nTemp1 #= X,\nadd_to(Temp1,Temp2,Temp3),\nTemp0 #= X,\nadd_to(Temp0,Temp3,Result_addTwice_to).\n\n\""

increment_fid="comp(s(\"increment\") v(\"X\"))"
increment_body="comp(s(\"add\") v(\"X\") s(\"to\") i(1))"
increment_type="func(int, int)"
increment_f="$(make_func "$increment_fid" "$increment_body")"

run_test "genFuncs(inferFuncs(empty, $add_f $increment_f))" "String: \"add_to(X,Y,Result_add_to) :- Result_add_to #= (X + Y).\n\nincrement(X,Result_increment) :- \nTemp1 #= 1,\nTemp0 #= X,\nadd_to(Temp0,Temp1,Result_increment).\n\n\""

