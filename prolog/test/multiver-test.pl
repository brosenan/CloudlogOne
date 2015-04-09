:- begin_tests(multiver).
:- use_module(multiver).

multiver:query(what, test(X), X).
multiver:mutate(add(N), test(X), Y) :-
	Y is X + N.

multiver:mutate(mult(N), test(X), Y) :-
	Y is X * N.

test(initial_value, []) :-
	multiver:empty(M0),
	multiver:init(M0, test(0), M1).

:- end_tests(multiver).
