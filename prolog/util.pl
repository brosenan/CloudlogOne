:- module(util, [if/3]).

if(C, T, _) :- C, !, T.
if(_, _, E) :- E.

