:- begin_tests(termcompare).
:- use_module('../termcompare.pl').

test(dominates_equals, []) :-
	termcompare:dominates(a(b), a(b)).

test(dominates_var_vs_ground, []) :-
	termcompare:dominates(a(_), a(b)).

test(dominates_ground_vs_var, []) :-
	\+termcompare:dominates(a(b), a(_)).

test(dominates_diff_after_first_var, []) :-
	termcompare:dominates(a(_, 1), a(b, 2)).

test(dominates_functor, []) :-
	\+termcompare:dominates(a(X, 1), b(X, 1)).

test(dominates_arity, []) :-
	\+termcompare:dominates(a(X, 1), a(X, 1, 3)).

test(dominates_nested, []) :-
	termcompare:dominates(a(b(1, _), c), a(b(1, 2), _)).

test(dominates_arity_after_var, []) :-
	\+termcompare:dominates(a(_, 1), a(_, 1, 3)).

:- end_tests(termcompare).
