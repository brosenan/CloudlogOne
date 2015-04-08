:- begin_tests(util).
:- ['../util.pl'].


test(if_true, [true(X == 2)]) :-
	if(true, X = 2, X = 3).
test(if_false, [true(X == 3)]) :-
	if(fail, X = 2, X = 3).


test(pivot_split, [true([R1, R2] == [[1, 2], [3, 4, 5]])]) :-
	util:pivotSplit([1, 2, 3, 4, 5], [], 3, R1, R2).

:- end_tests(util).