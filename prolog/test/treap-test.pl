:- begin_tests(treap).
:- use_module('../util.pl').
:- use_module('../treap.pl').
:- use_module('../multiver.pl').

test(create_empty) :-
	treap:empty(_).

test(get_returns_zero, [true(R == 0)]) :-
	treap:empty(T0),
	treap:get(T0, foo, R).

test(add_and_retrieve_value, [true((R1,R2) == (3,3))]) :-
	treap:empty(T0),
	treap:add(T0, foo, 1, 3, -1, T1, R1),
	treap:get(T1, foo, R2).

test(add_and_retrieve_two_values, [true([R1, R1Prime, R2, R2Prime] == [2, 2, 3, 3])]) :-
	treap:empty(T0),
	treap:add(T0, foo, 1, 2, -1, T1, R1Prime),
	treap:add(T1, bar, 2, 3, -1, T2, R2Prime),
	treap:get(T2, foo, R1),
	treap:get(T2, bar, R2).

test(add_acts_as_add, [true((R, R1, R2) == (5, 3, 5))]) :-
	treap:empty(T0),
	treap:add(T0, foo, 1, 3, -1, T1, R1),
	treap:add(T1, foo, 1, 2, -1, T2, R2),
	treap:get(T2, foo, R).

test(left_rotation, [true(T2 = t(_, b, 2, 2, _, _))]) :-
	treap:empty(T0),
	treap:add(T0, a, 1, 3, -1, T1, _),
	treap:add(T1, b, 2, 2, -1, T2, _).

test(right_rotation, [true(T2 = t(_, a, 2, 2, _, _))]) :-
	treap:empty(T0),
	treap:add(T0, b, 1, 3, -1, T1, _),
	treap:add(T1, a, 2, 2, -1, T2, _).

test(add_without_weight, [true([R1, R2] = [3, 2])]) :-
	treap:empty(T0),
	treap:add(T0, b, 3, T1),
	treap:add(T1, a, 2, T2),
	treap:get(T2, b, R1),
	treap:get(T2, a, R2).

populateTree(T0, N0, N, T) :-
	if(N0 = N,
	  T = T0,
	% else
	  (N1 is N0 + 1,
	  treap:add(T0, N0, N0, T1),
	  plunit_treap:populateTree(T1, N1, N, T))).

maxDepth(nil(_), 0).
maxDepth(t(L, _, _, _, _, R), D) :-
	maxDepth(L, DL),
	maxDepth(R, DR),
	D is max(DL, DR) + 1.

test(max_depth, [true(D < 28)]) :-
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
	once(treap:mergeTree(TA, TB, T)),
	validateTree(T, 0, 2000).

test(delete, [true(R1 = 0)]) :-
	treap:empty(T0),
	populateTree(T0, 0, 1000, T1),
	treap:delete(T1, 300, T2),
	treap:get(T2, 300, R1).

populateTreeFromList(T, [], T).
populateTreeFromList(T0, [First | Rest], T) :-
	treap:add(T0, First, 1, T1),
	populateTreeFromList(T1, Rest, T).

abTree(T) :-
	treap:empty(T0),
	findall(a(X, Y), (member(X, [1, 2, 3]), member(Y, [4, 5, 6])), L),
	once(populateTreeFromList(T0, L, T)).

test(find_dominated, [true(R == [[a(2, 4), 1], [a(2, 5), 1], [a(2, 6), 1]])]) :-
	abTree(T),
	findall([K, V], treap:findDominated(T, a(2, _), K, V), R).

test(trivial_add_hook, [true(R =@= [[testHook(1), 1]])]) :-
	treap:empty(T1),
	treap:addHook(T1, testHook(1), 1, a(3, _), T2, _),
	findall([H,V], treap:getHook(T2, a(3, 1), H, V), R).

test(trivial_add_hook_mismatch, [R =@= []]) :-
	treap:empty(T1),
	treap:addHook(T1, testHook(2), 1, a(3, _), T2, _),
	findall([H,V], treap:getHook(T2, a(2, 1), H, V), R).

test(add_hook, [true(R =@= [[testHook(4), 2], [testHook(5), 1]])]) :-
	abTree(T1),
	treap:addHook(T1, testHook(5), 1, a(3, _), T2, _),
	treap:addHook(T2, testHook(4), 2, a(_, _), T3, _),
	setof([H,V], treap:getHook(T3, a(3, 1), H, V), R).

test(add_hook_split_nil1, [true(R =@= [[testHook(3), 1]])]) :-
	treap:empty(T0),
	treap:addHook(T0, testHook(3), 1, a(3, _), T1, _),
	treap:add(T1, a(2, 1), 0, 1, -1, T2, _),
	treap:add(T2, a(3, 1), 1, 1, -1, T3, _),
	findall([H,V], treap:getHook(T3, a(3, 1), H, V), R).

test(add_hook_split_nil2, [true(R =@= [[testHook(6), 1]])]) :-
	treap:empty(T0),
	treap:addHook(T0, testHook(6), 1, a(3, _), T1, _),
	treap:add(T1, a(5, 1), 0, 1, -1, T2, _),
	treap:add(T2, a(4, 1), 1, 1, -1, T3, _),
	treap:add(T3, a(3, 1), 2, 1, -1, T4, _),
	findall([H,V], treap:getHook(T4, a(3, 1), H, V), R).

test(misplaced_hook, [true(R =@= [])]) :-
	treap:empty(T0),
	treap:addHook(T0, testHook(7), 1, a(3, _), T1, _),
	treap:add(T1, a(4, 1), 0, 1, -1, T2, _),
	findall([H,V], treap:getHook(T2, a(4, 1), H, V), R).

test(hook_rotate_left, [true(R =@= [testHook(8)])]) :-
	treap:empty(T0),
	treap:add(T0, a(3, 1), 0, 1, -1, T1, _),
	treap:addHook(T1, testHook(8), 1, a(3, _), T2, _),
	treap:add(T2, a(3, 2), 1, 1, -1, T3, _),
	findall(H, treap:getHook(T3, a(3, 2), H, _), R).

test(hook_rotate_right, [true(R =@= [testHook(9)])]) :-
	treap:empty(T0),
	treap:add(T0, a(3, 2), 0, 1, -1, T1, _),
	treap:addHook(T1, testHook(9), 1, a(3, _), T2, _),
	treap:add(T2, a(3, 1), 1, 1, -1, T3, _),
	findall(H, treap:getHook(T3, a(3, 1), H, _), R).

printTree(nil(Hs), Indent) :- write(Indent), write(nil(Hs)), nl.
printTree(ph(PH), Indent) :- write(Indent), write(ph(PH)), nl.
printTree(t(L, K, W, V, H, R), Indent) :-
	atom_concat(Indent, '    ', Indent1),
	printTree(L, Indent1),
	write(Indent), write([K, W, V, H]), nl,
	printTree(R, Indent1).

test(trivial_modify_hook_value, [R=[(testHook(10), 3)]]) :-
	treap:empty(T0),
	treap:addHook(T0, testHook(10), 1, foo(X), T1, _),
	treap:addHook(T1, testHook(10), 2, foo(X), T2, _),
	findall((H,V), treap:getHook(T2, foo(4), H, V), R).

test(trivial_add_hook_returns_value, [(R1, R2) == (1, 3)]) :-
	treap:empty(T0),
	treap:addHook(T0, testHook(11), 1, foo(X), T1, R1),
	treap:addHook(T1, testHook(11), 2, foo(X), _, R2).

test(modify_hook_value, [R=[(testHook(11), 3)]]) :-
	treap:empty(T0),
	treap:add(T0, foo(2), 7, T1),
	treap:addHook(T1, testHook(11), 1, foo(X), T2, _),
	treap:addHook(T2, testHook(11), 2, foo(X), T3, _),
	findall((H,V), treap:getHook(T3, foo(4), H, V), R).

test(add_hook_returns_new_value, [(R1, R2) == (1, 3)]) :-
	treap:empty(T0),
	treap:add(T0, foo(2), 7, T1),
	treap:add(T1, bar, 10, 3, -1, T2, _), % bar will be the root
	treap:addHook(T2, testHook(12), 1, foo(X), T3, R1),
	treap:addHook(T3, testHook(12), 2, foo(X), _, R2).

test(capped_add, [throws(treap_error(depth_limit_exceeded([])))]) :-
	treap:empty(T0),
	treap:add(T0, 1, 3, 1, 2, T1, _),
	treap:add(T1, 2, 2, 2, 2, T2, _),
	treap:add(T2, 3, 1, 3, 2, _, _).

test(capped_add_throws_pending_hooks, [throws(treap_error(depth_limit_exceeded([kv(a(4), testHook(13), 1)])))]) :-
	treap:empty(T0),
	treap:addHook(T0, testHook(13), 1, a(4), T1, _),
	treap:add(T1, a(1), 3, 1, 2, T2, _),
	treap:add(T2, a(2), 2, 2, 2, T3, _),
	treap:add(T3, a(3), 1, 3, 2, _, _).

test(put_placeholder, [true(R =@= ph(my_placeholder))]) :-
	treap:empty(T0),
	treap:add(T0, a(2, X), 1, T1),
	treap:putPlaceholder(T1, a(3, X), my_placeholder, T2),
	treap:get(T2, a(3, X), R).

test(update_placeholder, [true(R =@= ph(placeholder2))]) :-
	treap:empty(T0),
	treap:add(T0, a(2, X), 1, T1),
	treap:putPlaceholder(T1, a(3, X), placeholder1, T2),
	treap:updatePlaceholder(T2, a(3, X), placeholder1, placeholder2, T3),
	treap:get(T3, a(3, X), R).

test(update_placeholder_fails_on_mismatch, [throws(treap_error(unexpected_placeholder(placeholder1,placeholderXX)))]) :-
	treap:empty(T0),
	treap:add(T0, a(2, X), 1, T1),
	treap:putPlaceholder(T1, a(3, X), placeholder1, T2),
	treap:updatePlaceholder(T2, a(3, X), placeholderXX, placeholder2, _).

test(placeholders_appear_in_dominated_results, [true(R =@= [(_, ph(abc)), (a(2, 4), 1), (a(2, 5), 1), (a(2, 6), 1)])]) :-
	abTree(T0),
	treap:putPlaceholder(T0, a(2, 3), abc, T1),
	findall((K, V), treap:findDominated(T1, a(2, _), K, V), R).

test(patches_and_queries, [true((R1,R2,R3) =@= (1, [(testHook(x), 2)], [(_,ph(abcd))]))]) :-
	treap:empty(T0),
	multiver:patch(add(foo, 1, _), T0, T1),
	multiver:query(get(foo), T1, R1),
	multiver:patch(addHook(testHook(x), 2, bar, _), T1, T2),
	findall(X, multiver:query(getHook(bar), T2, X), R2),
	multiver:patch(putPlaceholder(baz, abc), T2, T3),
	multiver:patch(updatePlaceholder(baz, abc, abcd), T3, T4),
	findall(X, multiver:query(findDominated(baz), T4, X), R3).

test(patches_return_new_value, [(R1, R2) == (1, 2)]) :-
	treap:empty(T0),
	multiver:patch(add(foo, 1, R1), T0, T1),
	multiver:patch(addHook(testHook(x), 2, bar, R2), T1, _).

test(zero_values_should_be_removed, [true(T2 == T0)]) :-
	treap:empty(T0),
	treap:add(T0, foo, 3, T1),
	treap:add(T1, foo, -3, T2).

test(zero_hook_values_should_be_removed, [true(T2 == T0)]) :-
	treap:empty(T0),
	treap:addHook(T0, testHook(2), 3, foo, T1, _),
	treap:addHook(T1, testHook(2), -3, foo, T2, _).

:- end_tests(treap).
