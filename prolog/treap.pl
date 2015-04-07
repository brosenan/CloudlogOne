:- module(treap, [empty/1, lookup/3, add/5]).
:- use_module(util).

treap:empty(nil).

treap:lookup(nil, _, 0).
treap:lookup(t(L, K, _, V, R), K1, V1) :-
	if(K = K1,
	  V1 = V,
	%else
	  if(K1 @< K,
	    treap:lookup(L, K1, V1),
	  %else
	    treap:lookup(R, K1, V1))).

treap:add(nil, K, W, V, t(nil, K, W, V, nil)).
treap:add(t(L, K, W, V, R), K1, W1, V1, TOut) :-
	if(K == K1, (
	  L2 = L,
	  R2 = R,
	  V2 is V + V1),
	%else
	  if(K1 @< K, (
	    treap:add(L, K1, W1, V1, L2),
	    R2 = R,
	    V2 = V,
	    treap:rotateRight(t(L2, K, W, V2, R2), TOut)),
	  %else
	    (L2 = L,
	    treap:add(R, K1, W1, V1, R2),
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
