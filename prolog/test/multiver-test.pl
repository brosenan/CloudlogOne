:- begin_tests(multiver).
:- use_module(multiver).

multiver:query(what, test(X), X).
multiver:patch(add(N), test(X), test(Y)) :-
	Y is X + N.

multiver:patch(mult(N), test(X), test(Y)) :-
	Y is X * N.

multiver:query(getHash, test(X), X).

test(initial_value, [R == 0]) :-
	multiver:empty(M0),
	multiver:init(M0, test(0), H, M1),
	util:enforce(H == 0),
	multiver:query(what, M1, 0, R).

test(patch, [R == 3]) :-
	multiver:empty(M0),
	multiver:init(M0, test(0), H0, M1),
	multiver:patch(add(3), M1, H0, H1, M2),
	multiver:query(what, M2, H1, R).

:- end_tests(multiver).
