map_built_in(_F, empty, empty).
map_built_in(F,cons(H,T),cons(NewH,NewT)) :-
    call(F, H, NewH),
    map_built_in(F, T, NewT).

filter_built_in(_F, empty, empty).
filter_built_in(F, cons(H, T), Out) :-
    call(F, H), filter_built_in(F, T, Temp),
        Out = cons(H, Temp);
    filter_built_in(F, T, Out).

call_built_in(F, X, Res) :- call(F, X, Res).
call_rule_built_in(F, X) :- call(F, X).

disjunction_built_in(A, B, X) :-
    call(A, X);
    call(B, X).

one_of_built_in(empty, _) :- false.
one_of_built_in(cons(P, Rest), X) :-
    call(P, X);
    one_of_built_in(Rest, X).

% Use freeze so it gets checked whether we have bound D (maybe we can do clpfd something here?)
int_check(D) :- freeze(D, integer(D)).
string_check(S) :- freeze(S, atom(S)).

always(X).

% You can never have a value of type void.
void_check(Void) :- false.

unit_check(unit).

