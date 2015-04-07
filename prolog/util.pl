:- module(util, [if/3]).

if(C, T, _) :- C, !, T.
if(_, _, E) :- E.

enforce(G) :-
	if(G, true, (
	  write('Failed: '),
	  write(G),
	  nl,
	  fail)).