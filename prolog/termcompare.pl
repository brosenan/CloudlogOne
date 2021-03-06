:- module(termcompare, []).
:- use_module(util).

termcompare:dominates(X, Y) :-
	if(var(X),
	  true,
	% else
	  if(var(Y),
	    fail,
	  % else
	    (X =.. [Func | XArgs],
	    Y =.. [Func | YArgs],
	    termcompare:dominatesList(XArgs, YArgs)))).

termcompare:dominatesList([], []).
termcompare:dominatesList([X | Xs], [Y | Ys]) :-
	if(X == Y,
	  termcompare:dominatesList(Xs, Ys),
	% else
	  (termcompare:dominates(X, Y),
	  length(Xs, Arity),
	  length(Ys, Arity))).
