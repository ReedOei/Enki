map_built_in(_F,empty(), empty()).
map_built_in(F,cons(H,T),cons(NewH,NewT)) :-
    call(F, H, NewH),
    map_built_in(F, T, NewT).

filter_built_in(_F, empty(), empty()).
filter_built_in(F, cons(H, T), Out) :-
    call(F, H) ->
        filter_built_in(F, T, Temp),
        Out = cons(H, Temp);
    filter_built_in(F, T, Out).

call_built_in(F, X, Res) :- call(F, X, Res).

disjunction_built_in(A, B, X) :-
    call(A, X);
    call(B, X).

