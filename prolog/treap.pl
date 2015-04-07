:- module(treap, [empty/1, get/3, set/5]).
:- use_module(util).

treap:empty(nil).

treap:get(nil, _, nil).
treap:get(t(L, K, _, V, R), K1, V1) :-
	if(K = K1,
	  V1 = V,
	%else
	  if(K1 @< K,
	    treap:get(L, K1, V1),
	  %else
	    treap:get(R, K1, V1))).

treap:set(T1, K, V, T2) :-
	random(W),
	treap:set(T1, K, W, V, T2).

treap:set(nil, K, W, V, t(nil, K, W, V, nil)).
treap:set(t(L, K, W, V, R), K1, W1, V1, TOut) :-
	if(K == K1, (
	  L2 = L,
	  R2 = R,
	  V2 is V + V1),
	%else
	  if(K1 @< K, (
	    treap:set(L, K1, W1, V1, L2),
	    R2 = R,
	    V2 = V,
	    treap:rotateRight(t(L2, K, W, V2, R2), TOut)),
	  %else
	    (L2 = L,
	    treap:set(R, K1, W1, V1, R2),
	    V2 = V,
	    treap:rotateLeft(t(L2, K, W, V2, R2), TOut)))).

treap:rotateLeft(t(X, K1, W1, V1, t(Y, K2, W2, V2, Z)), T) :-
	if(W2 @< W1,
	  T = t(X, K1, W1, V1, t(Y, K2, W2, V2, Z)),
	% else
	  T = t(t(X, K1, W1, V1, Y), K2, W2, V2, Z)).

treap:rotateRight(t(t(X, K1, W1, V1, Y), K2, W2, V2, Z), T) :-
	if(W1 @< W2,
	  T = t(t(X, K1, W1, V1, Y), K2, W2, V2, Z),
	% else
	  T = t(X, K1, W1, V1, t(Y, K2, W2, V2, Z))).

treap:mergeTree(nil, nil, nil).
treap:mergeTree(nil, t(L, K, W, V, R), t(L, K, W, V, R)).
treap:mergeTree(t(L, K, W, V, R), nil, t(L, K, W, V, R)).
treap:mergeTree(t(L1, K1, W1, V1, R1), t(L2, K2, W2, V2, R2), T) :-
	util:enforce(K1 @< K2),
	if(W1 @> W2,
	  (treap:mergeTree(R1, t(L2, K2, W2, V2, R2), R1_),
	  T = t(L1, K1, W1, V1, R1_)),
	% else
	  (treap:mergeTree(t(L1, K1, W1, V1, R1), L2, L2_),
	  T = t(L2_, K2, W2, V2, R2))).
	  


