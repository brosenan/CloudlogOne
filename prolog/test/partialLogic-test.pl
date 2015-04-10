:- begin_tests(partialLogic).
:- use_module('../partialLogic.pl').
:- use_module('../multiver.pl').
:- use_module('../hashedTree.pl').


% This module provides logic operations on a partial database.
% The "add" operation that adds or subtracts value from a certain axiom (and thus adds or removes it from the database) is split into two operations: add_v and add_m.
% add_v is the operation of adding the axiom itself to the database, and is keyed by the axiom itself.
% add_m is the operation of adding a hook to match future axioms that interact with this axiom.  
% If the axiom is a rule, a hook for future facts is added, and if a fact, a hook for future rules is added.  The hook always matches axioms that are as or more specific than the axiom itself.

% [patch] add_v(+Axiom, +Value): Adds Value to Axiom.  Typically, Value=1 means adding Axiom to the database, and Value=-1 means removing it.
% [query] rawAxiom(?Axiom): Finds all matches to Axiom in the DB, along with their (non-zero) value.  Returns (Axiom, Value) pairs.
test(add_v, [R == (foo(bar),1)]) :-
	hashedTree:empty(T0),
	multiver:patch(add_v(foo(bar), 1), T0, T1),
	once(multiver:query(rawAxiom(foo(_)), T1, R)).

test(rawAxiom_must_match, [fail]) :-
	hashedTree:empty(T0),
	multiver:patch(add_v(a(1, 1), 1), T0, T1),
	multiver:patch(add_v(a(1, 2), 1), T1, T2),
	multiver:patch(add_v(a(1, 3), 1), T2, T3),
	once(multiver:query(rawAxiom(a(_, 4)), T3, _)).	

% [patch] add_m(+Axiom, +Value): Adds Value to a multiplier matching Axiom.  
%                                If Axiom is a rule, Value will be added to its multiplier over all matching facts, and vice versa.
% [query] add_v(+Axiom, +Value): Performs bottom-up evaluation of rules as a result of the add_v patch with the same Axiom and Value.  
%                                If Axiom is a rule, it finds matching facts, and vice versa.  It returns all resulting axioms.
%                                The associated value is the multiplication of the values of the fact and the rule
test(add_m_rule, [R == (bar(abc), 6)]) :-
	hashedTree:empty(T0),
	multiver:patch(add_m(rule(foo(X), true, bar(X)), 2), T0, T1),
	multiver:query(add_v(foo(abc), 3), T1, R).

% In this case the fact needs to be more general than the rule.
test(add_m_fact, [R == (bar, 15)]) :-
	hashedTree:empty(T0),
	multiver:patch(add_m(foo(_), 5), T0, T1),
	multiver:query(add_v(rule(foo(abc), true, bar), 3), T1, R).

% [query] add_m(+Axiom, +Value): Perform bottom-up evaluation as a result of adding a hook for matching axioms.  It scans the tree for matches that already exist.
test(add_v_fact, [R == (bar(abc), 6)]) :-
	hashedTree:empty(T0),
	multiver:patch(add_v(foo(abc), 2), T0, T1),
	once(multiver:query(add_m(rule(foo(X), true, bar(X)), 3), T1, R)).

test(add_v_rule, [R == (bar, 6)]) :-
	hashedTree:empty(T0),
	multiver:patch(add_v(rule(foo(abc), true, bar), 2), T0, T1),
	once(multiver:query(add_m(foo(_), 3), T1, R)).

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

infinite_results(0).
infinite_results(X) :- 
	infinite_results(X1),
	X is X1 + 1.

test(match_protected_agains_guard_with_infinite_results, [R1 =@= [timed_out(rule(a(s), plunit_partialLogic:infinite_results(Y), b(s, Y)))]]) :-
	findall(R, partialLogic:match(rule(a(X), plunit_partialLogic:infinite_results(Y), b(X, Y)), a(s), R), R1).

test(match_fails_on_two_rules, [fail]) :-
	partialLogic:match(rule(foo(_), true, _), rule(bar(_), true, _), _).

test(match_fails_on_two_facts, [fail]) :-
	partialLogic:match(foo(_), bar(_), _).

:- end_tests(partialLogic).
