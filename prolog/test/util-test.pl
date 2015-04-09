:- begin_tests(util).
:- ['../util.pl'].


test(if_true, [true(X == 2)]) :-
	if(true, X = 2, X = 3).
test(if_false, [true(X == 3)]) :-
	if(fail, X = 2, X = 3).


test(pivot_split, [true([R1, R2] == [[1, 2], [3, 4, 5]])]) :-
	util:pivotSplit([1, 2, 3, 4, 5], [], 3, R1, R2).

test(update_hash, [H2 == H0]) :-
	sha_hash('', H0, []),
	util:updateHash(H0, a(_, 2), H1),
	ground(H1),
	H1 \= H0,
	util:updateHash(H1, a(_, 2), H2).

test(saturate, [R == a('$SAT'(1), b('$SAT'(2), '$SAT'(1)))]) :-
	util:saturate(a(X, b(_, X)), R).

:- end_tests(util).