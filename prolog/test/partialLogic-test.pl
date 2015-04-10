:- begin_tests(partialLogic).
:- use_module('../partialLogic.pl').
:- use_module('../multiver.pl').
:- use_module('../hashedTree.pl').


% [patch] add_v(+Axiom, +Value): Adds Value to Axiom.  Typically, Value=1 means adding Axiom to the database, and Value=-1 means removing it.
% [query] rawAxiom(?Axiom): Finds all matches to Axiom in the DB, along with their (non-zero) value.  Returns (Axiom, Value) pairs.
test(add_v, [R == (foo(bar),1)]) :-
	hashedTree:empty(T0),
	multiver:patch(add_v(foo(bar), 1), T0, T1),
	once(multiver:query(rawAxiom(foo(_)), T1, R)).

% [patch] add_m(+Axiom, +Value): Adds Value to a multiplier matching Axiom.  
%                                If Axiom is a rule, Value will be added to its multiplier over all matching facts, and vice versa.
% [query] add_v(+Axiom, +Value): Performs bottom-up evaluation of rules as a result of the add_v patch with the same Axiom and Value.  
%                                If Axiom is a rule, it finds matching facts, and vice versa.  It returns all resulting axioms.
test(add_m_rule, [R == (bar(abc), 6)]) :-
	hashedTree:empty(T0),
	multiver:patch(add_m(rule(foo(X), true, bar(X)), 2), T0, T1),
	multiver:query(add_v(foo(abc), 3), T1, R).

% [nondet] match(+Axiom1, +Axiom2, -Axiom3): If one of Axiom1 and Axiom2 is a fact and the other is a matching rule,
%                                            Axiom3 is unified with all results that satisfy the rule's guard.
test(match_fact_rule, [R == b(t)]) :-
	partialLogic:match(a(t), rule(a(X), true, b(X)), R).

test(match_rule_fact, [R == b(s)]) :-
	partialLogic:match(rule(a(X), true, b(X)), a(s), R).


test(match_fact_rule_with_guard, [R1 == [b(t, 1), b(t, 2), b(t, 3)]]) :-
	findall(R, partialLogic:match(a(t), rule(a(X), member(Y, [1, 2, 3]), b(X, Y)), R), R1).

test(match_rule_fact_with_guard, [R1 == [b(s, 1), b(s, 2), b(s, 3)]]) :-
	findall(R, partialLogic:match(rule(a(X), member(Y, [1, 2, 3]), b(X, Y)), a(s), R), R1).

infinite_loop(X) :- infinite_loop(X).

% in case of a timeout, a special timed_out axiom is created instead of any of the results, containing the rule that failed, matched with the fact.
test(match_protected_agains_non_terminating_guard, [R == timed_out(rule(a(s), plunit_partialLogic:infinite_loop(s), b(s)))]) :-
	partialLogic:match(rule(a(X), plunit_partialLogic:infinite_loop(X), b(X)), a(s), R).

:- end_tests(partialLogic).
