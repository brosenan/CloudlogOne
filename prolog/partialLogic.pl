:- module(partialLogic, []).

:- use_module(multiver).
:- use_module(util).
:- use_module(sandbox).

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
	if(Value2 = ph(_),
	  ValueMult = Value2,
	% else
	  (partialLogic:match(Axiom1, Axiom2, Axiom3),
	  ValueMult is Value1 * Value2)).
	  

partialLogic:match(rule(Fact, Guard, Res), Fact, Res1) :- !, %%%
	catch((sandbox:eval(Res, Guard), Res1 = Res),
	  timed_out(_), 
	  Res1 = timed_out(rule(Fact, Guard, Res))).

partialLogic:match(Fact, rule(Fact, Guard, Res), Res1) :-
	partialLogic:match(rule(Fact, Guard, Res), Fact, Res1).


multiver:query(logicQuery(Res, Goal, Mul), T, Ret) :-
	multiver:query(rawAxiom((Goal :- _)), T, ((Goal :- Body), Val1)),
	if(Val1 = ph(PH),
	  Ret = ph(PH),
	% else
	  (Val2 is Val1 * Mul,
	  partialLogic:evaluateGoal(Body, Res, Val2, Ret))).

partialLogic:evaluateGoal(true, Res, Val, res(Res, Val)) :- !.  %%%
partialLogic:evaluateGoal(local(Goal), Res, Val, res(Res, Val)) :- !,  %%%
	sandbox:eval(Res, Goal).
partialLogic:evaluateGoal((Goal1, Goal2), Res, Val, Ret) :- !,  %%%
	if(partialLogic:canEval(Goal1),
	  (partialLogic:evaluateGoal(Goal1, Res, Val, res(_, _)),
	  partialLogic:evaluateGoal(Goal2, Res, Val, Ret)),
	% else
	  Ret = logicQuery(Res, (Goal1, Goal2), Val)).
partialLogic:evaluateGoal(Goal, Res, Val, logicQuery(Res, Goal, Val)).

partialLogic:canEval(true).
partialLogic:canEval(local(_)).
partialLogic:canEval((G1,G2)) :-
	partialLogic:canEval(G1),
	partialLogic:canEval(G2).
