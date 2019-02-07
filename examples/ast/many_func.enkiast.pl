add_X_to_Y(X,Y,add_X_to_Y_Result) :- add_X_to_Y_Result #= (X + Y).

increment_X(X,increment_X_Result) :-
Temp1 #= 1,
Temp0 #= X,
add_X_to_Y(Temp0,Temp1,increment_X_Result).

addTwice_X_to_Y(X,Y,addTwice_X_to_Y_Result) :-
Temp2 #= Y,
Temp1 #= X,
add_X_to_Y(Temp1,Temp2,Temp3),
Temp0 #= X,
add_X_to_Y(Temp0,Temp3,addTwice_X_to_Y_Result).

distance_from_X1_Y1_to_X2_Y2(X1,Y1,X2,Y2,distance_from_X1_Y1_to_X2_Y2_Result) :- distance_from_X1_Y1_to_X2_Y2_Result #= (((X1 - X2) ^ 2) + ((Y1 - Y2) ^ 2)).

