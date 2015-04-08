:- module(util, [if/3]).

if(C, T, _) :- C, !, T.
if(_, _, E) :- E.

enforce(G) :-
	if(G, true, (
	  write('Failed: '),
	  write(G),
	  nl,
	  fail)).

debug(G) :-
	write('before: '), write(G), nl,
	G,
	write('after: '), write(G), nl.

pivotSplit([], L2, _, [], L2).
pivotSplit([X | L1], L2, Pivot, L1Out, L2Out) :-
	if(X @< Pivot,
	  (L1Out = [X | L1Prime],
	  L2Out = L2Prime),
	% else
	  (L1Out = L1Prime,
	  L2Out = [X | L2Prime])),
	pivotSplit(L1, L2, Pivot, L1Prime, L2Prime).

