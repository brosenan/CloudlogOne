:- begin_tests(multiver).
:- use_module(multiver).

multiver:query(what, test(X), X).
multiver:patch(add(N), test(X), Y) :-
	Y is X + N.

multiver:patch(mult(N), test(X), Y) :-
	Y is X * N.

multiver:query(getHash, test(X), X).

test(initial_value, [R == 0]) :-
	multiver:empty(M0),
	multiver:init(M0, test(0), H, M1),
	util:enforce(H == 0),
	multiver:query(what, M1, 0, R).

:- end_tests(multiver).
