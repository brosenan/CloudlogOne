:- begin_tests(treap).
:- use_module('../treap.pl').

test(create_empty) :-
	treap:empty(_).

test(lookup_returns_zero, [true(R == 0)]) :-
	treap:empty(T0),
	treap:lookup(T0, foo, R).

test(add_and_retrieve_value, [true(R == 3)]) :-
	treap:empty(T0),
	treap:add(T0, foo, 1, 3, T1),
	treap:lookup(T1, foo, R).

test(add_and_retrieve_two_values, [true([R1, R2] == [2, 3])]) :-
	treap:empty(T0),
	treap:add(T0, foo, 1, 2, T1),
	treap:add(T1, bar, 2, 3, T2),
	treap:lookup(T2, foo, R1),
	treap:lookup(T2, bar, R2).

test(left_rotation, [true(T2 = t(_, b, 2, 2, _))]) :-
	treap:empty(T0),
	treap:add(T0, a, 1, 3, T1),
	treap:add(T1, b, 2, 2, T2).

test(right_rotation, [true(T2 = t(_, a, 2, 2, _))]) :-
	treap:empty(T0),
	treap:add(T0, b, 1, 3, T1),
	treap:add(T1, a, 2, 2, T2).

:- end_tests(treap).
