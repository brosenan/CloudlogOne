:- module(treap, [empty/1, lookup/3, add/5]).
:- use_module(util).

treap:empty(nil).

treap:lookup(nil, _, 0).
treap:lookup(t(L, K, V, R), K1, V1) :-
	if(K = K1,
	  V1 = V,
	%else
	  if(K1 @< K,
	    treap:lookup(L, K1, V1),
	  %else
	    treap:lookup(R, K1, V1))).

treap:add(nil, K, _, V, t(nil, K, V, nil)).
treap:add(t(L, K, V, R), K1, _, V1, T) :-
	if(K @< K1,
	  T = t(t(L, K, V, R), K1, V1, nil),
	%else
	  T = t(nil, K1, V1, t(L, K, V, R))).
