:- module(partialLogic, []).

:- use_module(multiver).
:- use_module(util).
:- use_module(sandbox).

partialLogic:hookDomain(A, B) :-
		if(A = rule(B, _, _),
		(
			true
		), % else
		(
			B = rule(A, _, _)
		)).

multiver:patch(add_v(Axiom, Value), T1, T2) :-
	multiver:patch(h_add(Axiom, Value, _), T1, T2).

multiver:query(rawAxiom(Axiom), T, (Axiom, V)) :-
	multiver:query(h(findDominated(Axiom)), T, (Axiom, V)).

multiver:query(rawAxiom(Axiom), T, (Axiom, V)) :-
	multiver:query(h(getHook(rule(Axiom,_,_))), T, (Axiom, V)).

multiver:patch(add_m(Axiom, Value), T1, T2) :-
	if(ground(Axiom),
	(
	    T2 = T1 % Don't bother for ground axioms
	), % else
	(
    catch(
    (
			partialLogic:hookDomain(Axiom, Key),
			multiver:patch(h_addHook(Axiom, Value, Key, _), T1, T2)
    ),
    forwardToPlaceholder(_),
    (
			T2 = T1
    ))
	)).

multiver:query(add_v(Axiom1, Value1), T, add(Axiom3, ValueMult)) :-
	multiver:query(h(getHook(Axiom1)), T, (Axiom2, Value2)),
	partialLogic:match(Axiom1, Axiom2, Axiom3),
	ValueMult is Value1 * Value2.

multiver:query(add_m(Axiom1, Value1), T, add(Axiom3, ValueMult)) :-
	\+ground(Axiom1),
	partialLogic:hookDomain(Axiom1, D),
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
	partialLogic:evaluateGoal(Goal, Res, Mul, T, Ret1),
	if((Ret1 = logicQuery(_,SubGoal,_), partialLogic:canReevaluate(SubGoal)),
	(
	    multiver:query(Ret1, T, Ret)
	), % else
	(
	    Ret = Ret1
	)).

partialLogic:evaluateGoal(true, Res, Mul, _, res(Res, Mul)).
partialLogic:evaluateGoal(local(G), Res, Mul, _, res(Res, Mul)) :-
	sandbox:eval(Res, G).
partialLogic:evaluateGoal((G1, G2), Res, MulIn, T, logicQuery(Res, GOut, MulOut)) :-
	multiver:query(logicQuery(G2, G1, MulIn), T, Ret1),
	if(Ret1 = res(GOut, MulOut),
	(
	    true
	), % else
	(
	    Ret1 = logicQuery(G2, G1_, MulOut),
	    GOut = (G1_, G2)
	)).
partialLogic:evaluateGoal(Head, Res, MulIn, T, Ret) :-
	multiver:query(rawAxiom((Head :- _)), T, ((Head :- Body), MulClause)),
	if(MulClause = ph(PH),
	(
	    Ret = ph(PH)
	), % else
	(
	    MulOut is MulIn * MulClause,
	    Ret = logicQuery(Res, Body, MulOut)
	)).

partialLogic:canReevaluate(true).
partialLogic:canReevaluate(local(_)).
partialLogic:canReevaluate((G1,_)) :-
	partialLogic:canReevaluate(G1).

% logicQuery as a trivial patch
multiver:patch(logicQuery(_, _, _), T, T).

partialLogic:canEval(true).
partialLogic:canEval(local(_)).
partialLogic:canEval((G1,G2)) :-
	partialLogic:canEval(G1),
	partialLogic:canEval(G2).
