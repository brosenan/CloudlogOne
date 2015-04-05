:- begin_tests(util).
:- ['../util.pl'].


test(if_true, [true(X == 2)]) :-
	if(true, X = 2, X = 3).
test(if_false, [true(X == 3)]) :-
	if(fail, X = 2, X = 3).

:- end_tests(util).