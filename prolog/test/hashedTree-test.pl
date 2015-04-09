:- begin_tests(hashedTree).
:- use_module('../hashedTree.pl').
:- use_module('../multiver.pl').

test(create_new_hashed_tree, [R == 0]) :-
	hashedTree:empty(T0),
	multiver:query(h(get(foo)), T0, R).

test(get_hash, [H == '2jmj7l5rSw0yVb/vlWAYkK/YBwk=']) :- % The sha1 hash of an empty string
	hashedTree:empty(T0),
	multiver:query(getHash, T0, H).

test(h_add, [(R1, R2) == (3, '2jmj7l5rSw0yVb/vlWAYkK/YBwk=')]) :-
	hashedTree:empty(T0),
	multiver:patch(h_add(foo, 3, _), T0, T1),
	multiver:query(h(get(foo)), T1, R1),
	multiver:query(getHash, T1, H1),
	util:enforce(H1 \= '2jmj7l5rSw0yVb/vlWAYkK/YBwk='),   % Not the empty string
	multiver:patch(h_add(foo, -3, _), T1, T2),
	multiver:query(getHash, T2, R2).

test(h_add_hook, [R == (testHook(foo, bar), 3)]) :-
	hashedTree:empty(T0),
	multiver:query(getHash, T0, H0),
	multiver:patch(h_addHook(testHook(foo, bar), 3, _), T0, T1),
	multiver:query(h(getHook(foo)), T1, R),
	multiver:query(getHash, T1, H1),
	util:enforce(H1 \= H0),
	multiver:patch(h_addHook(testHook(foo, bar), -3, _), T1, T2),
	multiver:query(getHash, T2, H2),
	util:enforce(H2 == H0).

test(h_placeholder, [(R1, R2) == (ph(abc), ph(abcd))]) :-
	hashedTree:empty(T0),
	multiver:query(getHash, T0, H0),
	multiver:patch(h_putPlaceholder(foo, abc), T0, T1),
	multiver:query(h(findDominated(foo)), T1, (_,R1)),
	multiver:query(getHash, T1, H1),
	util:enforce(H1 \= H0),
	multiver:patch(h_updatePlaceholder(foo, abc, abcd), T1, T2),
	multiver:query(h(findDominated(foo)), T2, (_,R2)),
	multiver:query(getHash, T2, H2),
	util:enforce(H2 \= H1),
	multiver:patch(h_updatePlaceholder(foo, abcd, abc), T2, T3),
	multiver:query(getHash, T3, H3),
	util:enforce(H3 == H1).

:- end_tests(hashedTree).
