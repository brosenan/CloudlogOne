:- module(partialLogic, []).

:- use_module(multiver).
:- use_module(util).

treap:hookDomain(rule(A, _, _), A) :- !.
treap:hookDomain(A, rule(A, _, _)) :- !.


multiver:patch(add_v(Axiom, Value), T1, T2) :-
	multiver:patch(h_add(Axiom, Value, _), T1, T2).

multiver:query(rawAxiom(Axiom), T, (Axiom, V)) :-
	multiver:query(h(findDominated(Axiom)), T, (Axiom, V)).

multiver:patch(add_m(Axiom, Value), T1, T2) :-
	multiver:patch(h_addHook(Axiom, Value, _), T1, T2).

multiver:query(add_v(Axiom1, Value1), T, (Axiom3, ValueMult)) :-
	multiver:query(h(getHook(Axiom1)), T, (Axiom2, Value2)),
	partialLogic:match(Axiom1, Axiom2, Axiom3),
	ValueMult is Value1 * Value2.

multiver:query(add_m(Axiom1, Value1), T, (Axiom3, ValueMult)) :-
	treap:hookDomain(Axiom1, D),
	multiver:query(h(findDominated(D)), T, (Axiom2, Value2)),
	partialLogic:match(Axiom1, Axiom2, Axiom3),
	ValueMult is Value1 * Value2.

partialLogic:match(rule(Fact, Guard, Res), Fact, Res1) :- !, %%%
	util:time_out(Guard, 100, Status),
	if(Status = success,
	  Res1 = Res,
	% else
	  Res1 = timed_out(rule(Fact, Guard, Res))).

partialLogic:match(Fact, rule(Fact, Guard, Res), Res1) :-
	partialLogic:match(rule(Fact, Guard, Res), Fact, Res1).
