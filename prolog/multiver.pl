:- module(multiver, []).

:- use_module(util).

:- multifile patch/3.
:- multifile query/3.

multiver:empty(M0) :-
	rb_empty(M0).

multiver:init(M1, T, H, M2) :-
	multiver:query(getHash, T, H),
	rb_insert(M1, H, T, M2).

multiver:query(Q, M, H, V) :-
	rb_lookup(H, T, M),
	multiver:query(Q, T, V).

%multiver:query([Q|_], T, V) :-
%	multiver:query(Q, T, V).
%
%multiver:query([_|Q], T, V) :-
%	multiver:query(Q, T, V).


multiver:patch(P, M1, H1, H2, M2) :-
	rb_lookup(H1, T1, M1),
	multiver:patch(P, T1, T2),
	multiver:query(getHash, T2, H2),
	rb_insert(M1, H2, T2, M2).

multiver:patch([], T, T).
multiver:patch([P | Ps], T1, T3) :-
	multiver:patch(P, T1, T2),
	multiver:patch(Ps, T2, T3).

