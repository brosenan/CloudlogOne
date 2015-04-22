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

% Unlike findDominated, which returns all dominated results, rawAxiom only returns axioms that match Axiom.
test(rawAxiom_must_match, [fail]) :-
	hashedTree:empty(T0),
	multiver:patch(add_v(a(1, 1), 1), T0, T1),
	multiver:patch(add_v(a(1, 2), 1), T1, T2),
	multiver:patch(add_v(a(1, 3), 1), T2, T3),
	multiver:query(rawAxiom(a(_, 4)), T3, _).	

% Patch add_v throws an exception if the modification needs to be performed in a placeholder.
test(add_v_throws_on_placeholder, [throws(forwardToPlaceholder(myPlaceholder))]) :-
	hashedTree:empty(T0),
	multiver:patch(h_putPlaceholder(a, myPlaceholder), T0, T1),
	multiver:patch(add_v(b, 1), T1, _).

% If rawAxiom encounters a placeholder, it returns (Axiom, ph(PH)), where Axiom is not further evaluated, and PH the placeholder term.
test(rawAxiom_returns_placeholder, [R =@= (b(_), ph(myPlaceholder))]) :-
	hashedTree:empty(T0),
	multiver:patch(h_putPlaceholder(a, myPlaceholder), T0, T1),
	multiver:query(rawAxiom(b(_)), T1, R).


% [patch] add_m(+Axiom, +Value): Adds Value to a multiplier matching Axiom.  
%                                If Axiom is a rule, Value will be added to its multiplier over all matching facts, and vice versa.
% [query] add_v(+Axiom, +Value): Performs bottom-up evaluation of rules as a result of the add_v patch with the same Axiom and Value.  
%                                If Axiom is a rule, it finds matching facts, and vice versa.  It returns all resulting axioms.
%                                The associated value is the multiplication of the values of the fact and the rule
test(add_m_rule, [R == (bar(abc), 6)]) :-
	hashedTree:empty(T0),
	multiver:patch(add_m(rule(foo(X), true, bar(X)), 2), T0, T1),
	multiver:query(add_v(foo(abc), 3), T1, R).

% In the following case the fact needs to be more general than the rule.
test(add_m_fact, [R == (bar, 15)]) :-
	hashedTree:empty(T0),
	multiver:patch(add_m(foo(_), 5), T0, T1),
	multiver:query(add_v(rule(foo(abc), true, bar), 3), T1, R).

% Patch add_m must ignore placeholders
test(add_m_ignores_placeholder, [T1 == T2]) :-
	hashedTree:empty(T0),
	multiver:patch(h_putPlaceholder(a, myOtherPlaceholder), T0, T1),
	multiver:patch(add_m(rule(b, true, c), 1), T1, T2).

% [query] add_m(+Axiom, +Value): Perform bottom-up evaluation as a result of adding a hook for matching axioms.  It scans the tree for matches that already exist.
test(add_v_fact, [R == (bar(abc), 6)]) :-
	hashedTree:empty(T0),
	multiver:patch(add_v(foo(abc), 2), T0, T1),
	once(multiver:query(add_m(rule(foo(X), true, bar(X)), 3), T1, R)).

test(add_v_rule, [R == (bar, 6)]) :-
	hashedTree:empty(T0),
	multiver:patch(add_v(rule(foo(abc), true, bar), 2), T0, T1),
	once(multiver:query(add_m(foo(_), 3), T1, R)).

% The add_m query returns placeholder results
test(add_m_returns_placeholder, [R =@= (_, ph(myPlaceholder))]) :-
	hashedTree:empty(T0),
	multiver:patch(h_putPlaceholder(a, myPlaceholder), T0, T1),
	multiver:query(add_m(rule(a, true, b), 1), T1, R).

% [query] logicQuery(?Result, +Goal, +Mul): Evaluates Goal using clauses (axioms of the form H :- B) in the database. Result should be a term sharing some variables with Goal, and Mul should be a number.
%                                           Returns zero or more of:
%                                            - res(Result, Value), where Result is unified by the goal, and Value is the value of the clause axiom contributing this result, times Mul.
%                                            - ph(PH), where PH is a placeholder, in case the placeholder needs to be consulted for more results.
%                                            - logicQuery(Result, Goal1, Mul1), where Goal1 is a (different) goal, and Mul1 is a (different) number, indicates that Goal1 needs to be consulted
%                                              for more results.

% If Goal is "true", the query succeeds without further conditions.
test(logicQuery_simple, [R == res(bar, 6)]) :-
	hashedTree:empty(T0),
	multiver:patch(add_v((foo(bar) :- true), 3), T0, T1),
	once(multiver:query(logicQuery(X, foo(X), 2), T1, R)).

% If Goal is local(G), goal G is evaluated locally (as a Prolog goal).
test(logicQuery_local, [R == res(bar, 15)]) :-
	hashedTree:empty(T0),
	multiver:patch(add_v((foo(X) :- local(X = bar)), 5), T0, T1),
	once(multiver:query(logicQuery(X, foo(X), 3), T1, R)).

% Evaluation of local goals should be proteceted agains runaway goals.
test(logicQuery_local_runaway, [throws(timed_out(_))]) :-
	hashedTree:empty(T0),
	multiver:patch(add_v((foo(X) :- local(plunit_partialLogic:infinite_results(X))), 5), T0, T1),
	findall(R, multiver:query(logicQuery(X, foo(X), 3), T1, R), _).

% If Goal is of the form (G1,G2), goal G1 is evaluated and for each result, G2 is evaluated.
test(logicQuery_conj, [R == res(baz, 15)]) :-
	hashedTree:empty(T0),
	multiver:patch(add_v((foo(X) :- local(member(X, [bar, baz, bat])), local(X = baz)), 5), T0, T1),
	once(multiver:query(logicQuery(X, foo(X), 3), T1, R)).

% If Goal is none of the above, the goal must be re-evaluated using a different set of clauses. Therefore, a subsequent query is returned.
test(logicQuery_other, [R =@= logicQuery(X, bar(X), 15)]) :-
	hashedTree:empty(T0),
	multiver:patch(add_v((foo(X) :- bar(X)), 5), T0, T1),
	once(multiver:query(logicQuery(X, foo(X), 3), T1, R)).

% It is possible to combine different kinds of goals through conjunction
test(logicQuery_local_other, [R =@= logicQuery(7, bar(7), 15)]) :-
	hashedTree:empty(T0),
	multiver:patch(add_v((foo(X) :- local(X = 7), bar(X)), 5), T0, T1),
	once(multiver:query(logicQuery(X, foo(X), 3), T1, R)).

test(logicQuery_other_local, [R =@= logicQuery(X, (bar(X), local(X = 7)), 15)]) :-
	hashedTree:empty(T0),
	multiver:patch(add_v((foo(X) :- bar(X), local(X = 7)), 5), T0, T1),
	once(multiver:query(logicQuery(X, foo(X), 3), T1, R)).

test(logicQuery_placeholder, [R == ph(myPlaceholder)]) :-
	hashedTree:empty(T0),
	multiver:patch(h_putPlaceholder(a, myPlaceholder), T0, T1),
	multiver:query(logicQuery(X, foo(X), 1), T1, R).

% Results can be either of the form res(Result), unifying Result with a result, or logicQuery(Result, OtherGoal), indicating a different goal to be evaulated.
% In case of a placeholder PH (more results located elsewhere), logicQuery(Result, Goal, PH) will be returned.

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
