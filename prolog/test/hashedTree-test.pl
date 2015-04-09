:- begin_tests(hashedTree).
:- use_module('../hashedTree.pl').
:- use_module('../multiver.pl').

test(create_new_hashed_tree, [R == 0]) :-
	hashedTree:empty(T0),
	multiver:query(h(get(foo)), T0, R).

test(get_hash, [H == '2jmj7l5rSw0yVb/vlWAYkK/YBwk=']) :- % The sha1 hash of an empty string
	hashedTree:empty(T0),
	multiver:query(getHash, T0, H).

test(add, [(R1, R2) == (3, '2jmj7l5rSw0yVb/vlWAYkK/YBwk=')]) :-
	hashedTree:empty(T0),
	multiver:patch(h(add(foo, 3, _)), T0, T1),
	multiver:query(h(get(foo)), T1, R1),
	multiver:query(getHash, T1, H1),
	H1 \= '2jmj7l5rSw0yVb/vlWAYkK/YBwk=',   % Not the empty string
	multiver:patch(h(add(foo, -3, _)), T1, T2),
	multiver:query(getHash, T2, R2).

:- end_tests(hashedTree).
