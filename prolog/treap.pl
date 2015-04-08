:- module(treap, [empty/1, get/3, set/4]).
:- use_module(util).
:- use_module(termcompare).

:- multifile hookDomain/2.

treap:empty(nil([])).

treap:get(nil(_), _, nil).
treap:get(ph(PH), _, ph(PH)).
treap:get(t(L, K, _, V, _, R), K1, V1) :-
	if(K = K1,
	  V1 = V,
	%else
	  if(K1 @< K,
	    treap:get(L, K1, V1),
	  %else
	    treap:get(R, K1, V1))).

treap:set(T1, K, V, T2) :-
	random(W),
	treap:set(T1, K, W, V, -1, T2).

treap:set(nil(H), K, W, V, D, t(nil(HL), K, W, V, HM, nil(HR))) :-
	if(D = 0,
	  throw(treap_error(depth_limit_exceeded(H))),
	% else
	  true),
	treap:splitThreeWays(H, K, HL, HM, HR).

treap:set(t(L, K, W, V, H, R), K1, W1, V1, D, TOut) :-
	D1 is D - 1,
	if(K == K1, (
	  L2 = L,
	  R2 = R,
	  V2 = V1),
	%else
	  if(K1 @< K, (
	    treap:set(L, K1, W1, V1, D1, L2),
	    R2 = R,
	    V2 = V,
	    treap:rotateRight(t(L2, K, W, V2, H, R2), TOut)),
	  %else
	    (L2 = L,
	    treap:set(R, K1, W1, V1, D1, R2),
	    V2 = V,
	    treap:rotateLeft(t(L2, K, W, V2, H, R2), TOut)))).

treap:rotateLeft(t(X, K1, W1, V1, H1, t(Y, K2, W2, V2, H2, Z)), T) :-
	if(W2 @< W1,
	  T = t(X, K1, W1, V1, H1, t(Y, K2, W2, V2, H2, Z)),
	% else
	  (treap:rotateHooks(H1, H2, K2, H1Prime, H2Prime),
	  T = t(t(X, K1, W1, V1, H1Prime, Y), K2, W2, V2, H2Prime, Z))).

treap:rotateRight(t(t(X, K1, W1, V1, H1, Y), K2, W2, V2, H2, Z), T) :-
	if(W1 @< W2,
	  T = t(t(X, K1, W1, V1, H1, Y), K2, W2, V2, H2, Z),
	% else
	  (treap:rotateHooks(H2, H1, K1, H2Prime, H1Prime),
	  T = t(X, K1, W1, V1, H1Prime, t(Y, K2, W2, V2, H2Prime, Z)))).

treap:mergeTree(nil(H1), nil(H2), nil(H)) :- append(H1, H2, H).
treap:mergeTree(nil(H1), t(L, K, W, V, H2, R), t(L, K, W, V, H, R)) :- append(H1, H2, H).
treap:mergeTree(t(L, K, W, V, H1, R), nil(H2), t(L, K, W, V, H, R)) :- append(H1, H2, H).
treap:mergeTree(t(L1, K1, W1, V1, H1, R1), t(L2, K2, W2, V2, H2, R2), T) :-
	util:enforce(K1 @< K2),
	if(W1 @> W2,
	  (treap:mergeTree(R1, t(L2, K2, W2, V2, H2, R2), R1_),
	  T = t(L1, K1, W1, V1, H1, R1_)),
	% else
	  (treap:mergeTree(t(L1, K1, W1, V1, H1, R1), L2, L2_),
	  T = t(L2_, K2, W2, V2, H2, R2))).

treap:delete(t(L, K, _, _, _, R), K1, T) :-
	if(K = K1,
	  once(treap:mergeTree(L, R, T)),
	%else
	  if(K1 @< K,
	    treap:delete(L, K1, T),
	  %else
	    treap:delete(R, K1, T))).

treap:findDominated(t(L, K, W, V, H, R), D, KOut, VOut) :-
	if(termcompare:dominates(D, K),
	  (treap:treeMember(t(L, K, W, V, H, R), KOut, VOut),
	  termcompare:dominates(D, KOut)),
	% else
	  if(D @< K,
	    treap:findDominated(L, D, KOut, VOut),
	  % else
	    treap:findDominated(R, D, KOut, VOut))).

treap:treeMember(t(L, _, _, _, _, _), K, V) :- treap:treeMember(L, K, V).
treap:treeMember(t(_, K, _, V, _, _), K, V).
treap:treeMember(t(_, _, _, _, _, R), K, V) :- treap:treeMember(R, K, V).

treap:setHook(T1, H, V, T2) :-
	treap:hookDomain(H, D),
	treap:setHook(T1, H, V, D, T2).

treap:setHook(nil(Hs), H, Hv, _, nil(Hs1)) :-
	treap:updateHookValue(Hs, H, Hv, Hs1).
treap:setHook(t(L, K, W, V, Hs, R), H, Hv, D, T) :-
	if(termcompare:dominates(D, K),
	  (treap:updateHookValue(Hs, H, Hv, Hs1),
	  T = t(L, K, W, V, Hs1, R)),
	% else
	  if(D @< K,
	    (treap:setHook(L, H, Hv, D, L1),
	    T = t(L1, K, W, V, Hs, R)),
	  % else
	    (treap:setHook(R, H, Hv, D, R1),
	    T = t(L, K, W, V, Hs, R1)))).
	    

treap:getHook(nil(Hs), K, H, V) :-
	member(kv(H,V), Hs),
	treap:hookDomain(H, D),
	termcompare:dominates(D, K).

treap:getHook(t(_, _, _, _, Hs, _), K, H, V) :-
	member(kv(H,V), Hs),
	treap:hookDomain(H, D),
	termcompare:dominates(D, K).
	
treap:getHook(t(L, K, _, _, _, R), K1, H, V) :-
	\+termcompare:dominates(K1, K),
	if(K1 @< K,
	  treap:getHook(L, K1, H, V),
	% else
	  treap:getHook(R, K1, H, V)).

treap:rotateHooks([], H2, _, [], H2).
treap:rotateHooks([kv(H, V) | Rest], H2, K, H1Prime, H2Prime) :-
	treap:rotateHooks(Rest, H2, K, H1PrimePrime, H2PrimePrime),
	treap:hookDomain(H, D),
	if(termcompare:dominates(D, K),
	  (H1Prime = H1PrimePrime,
	  H2Prime = [kv(H, V), H2PrimePrime]),
	% else
	  (H1Prime = [kv(H, V) | H1PrimePrime],
	  H2Prime = H2PrimePrime)).

treap:splitThreeWays([], _, [], [], []).
treap:splitThreeWays([kv(H, V) | Hs], K, LOut, MOut, ROut) :-
	treap:hookDomain(H, D),
	if(termcompare:dominates(D, K),
	  [LOut, MOut, ROut] = [LPrime, [kv(H, V) | MPrime], RPrime],
	% else
	  if(D @< K,
	    [LOut, MOut, ROut] = [[kv(H, V) | LPrime], MPrime, RPrime],
	  % else
	    [LOut, MOut, ROut] = [LPrime, MPrime, [kv(H, V) | RPrime]])),
	treap:splitThreeWays(Hs, K, LPrime, MPrime, RPrime).

treap:updateHookValue([], H, Hv, [kv(H, Hv)]).
treap:updateHookValue([kv(K, V) | Hs], H, Hv, HsOut) :-
	if(K =@= H,
	  HsOut = [kv(H, Hv) | Hs],
	% else
	  (HsOut = [kv(K, V) | HsPrime],
	  treap:updateHookValue(Hs, H, Hv, HsPrime))).

treap:putPlaceholder(nil(_), _, PH, ph(PH)).
treap:putPlaceholder(t(L, K, W, V, H, R), K1, PH, T) :-
	if(K1 @< K,
	  (treap:putPlaceholder(L, K1, PH, L1),
	  T = t(L1, K, W, V, H, R)),
	% else
	  (treap:putPlaceholder(R, K1, PH, R1),
	  T = t(L, K, W, V, H, R1))).

treap:updatePlaceholder(ph(PH1), _, PH1, PH2, ph(PH2)).
treap:updatePlaceholder(t(L, K, W, V, H, R), K1, PH1, PH2, T) :-
	if(K1 @< K,
	  (treap:updatePlaceholder(L, K1, PH1, PH2, L1),
	  T = t(L1, K, W, V, H, R)),
	% else
	  (treap:updatePlaceholder(R, K1, PH1, PH2, R1),
	  T = t(L, K, W, V, H, R1))).
	

