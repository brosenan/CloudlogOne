:- module(multiver, []).

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

	
