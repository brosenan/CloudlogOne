:- begin_tests(treap).
:- use_module('../util.pl').
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

test(set_without_weight, [true([R1, R2] = [3, 2])]) :-
	treap:empty(T0),
	treap:set(T0, b, 3, T1),
	treap:set(T1, a, 2, T2),
	treap:get(T2, b, R1),
	treap:get(T2, a, R2).

populateTree(T0, N0, N, T) :-
	if(N0 = N,
	  T = T0,
	% else
	  (N1 is N0 + 1,
	  treap:set(T0, N0, N0, T1),
	  plunit_treap:populateTree(T1, N1, N, T))).

maxDepth(nil, 0).
maxDepth(t(L, _, _, _, R), D) :-
	maxDepth(L, DL),
	maxDepth(R, DR),
	D is max(DL, DR) + 1.

test(max_depth, [true(D < 27)]) :-
	treap:empty(T0),
	populateTree(T0, 0, 1000, T),
	maxDepth(T, D).

validateTree(T, N0, N) :-
	if(N0 = N,
	  true,
	% else
	  (N1 is N0 + 1,
	  treap:get(T, N0, V),
	  if(V \= N0, (write(V \= N0), nl, fail), true),
	  plunit_treap:validateTree(T, N1, N))).
	

test(validate_values, []) :-
	treap:empty(T0),
	populateTree(T0, 0, 1000, T),
	validateTree(T, 0, 1000).

test(merge_treaps, []) :-
	treap:empty(TA0),
	populateTree(TA0, 0, 1000, TA),
	treap:empty(TB0),
	populateTree(TB0, 1000, 2000, TB),
	treap:mergeTree(TA, TB, T),
	validateTree(T, 0, 2000).

test(delete, [true(R1 = nil)]) :-
	treap:empty(T0),
	populateTree(T0, 0, 1000, T1),
	treap:delete(T1, 300, T2),
	treap:get(T2, 300, R1).

:- end_tests(treap).
