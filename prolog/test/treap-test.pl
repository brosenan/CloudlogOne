:- begin_tests(treap).
:- use_module('../treap.pl').

test(create_empty) :-
	treap:empty(_).

test(get_returns_nil, [true(R == nil)]) :-
	treap:empty(T0),
	treap:get(T0, foo, R).

test(add_and_retrieve_value, [true(R == 3)]) :-
	treap:empty(T0),
	treap:set(T0, foo, 1, 3, T1),
	treap:get(T1, foo, R).

test(add_and_retrieve_two_values, [true([R1, R2] == [2, 3])]) :-
	treap:empty(T0),
	treap:set(T0, foo, 1, 2, T1),
	treap:set(T1, bar, 2, 3, T2),
	treap:get(T2, foo, R1),
	treap:get(T2, bar, R2).

test(left_rotation, [true(T2 = t(_, b, 2, 2, _))]) :-
	treap:empty(T0),
	treap:set(T0, a, 1, 3, T1),
	treap:set(T1, b, 2, 2, T2).

test(right_rotation, [true(T2 = t(_, a, 2, 2, _))]) :-
	treap:empty(T0),
	treap:set(T0, b, 1, 3, T1),
	treap:set(T1, a, 2, 2, T2).

:- end_tests(treap).
