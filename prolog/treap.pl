:- module(treap, []).
:- use_module(util).
:- use_module(termcompare).
:- use_module(multiver).

:- multifile hookDomain/2.

:- dynamic max_depth/1.

max_depth(20).

treap:empty(nil([])).

treap:get(nil(_), _, 0).
treap:get(ph(PH), _, ph(PH)).
treap:get(t(L, K, _, V, _, R), K1, V1) :-
	if(K = K1,
	  V1 = V,
	%else
	  if(K1 @< K,
	    treap:get(L, K1, V1),
	  %else
	    treap:get(R, K1, V1))).

treap:add(T1, K, V, T2) :-
	random(W),
	treap:add(T1, K, W, V, -1, T2, _).

treap:add(nil(H), K, W, V, D, t(nil(HL), K, W, V, HM, nil(HR)), V) :-
	if(D = 0,
	  throw(treap_error(depth_limit_exceeded(H))),
	% else
	  true),
	treap:splitThreeWays(H, K, HL, HM, HR).

treap:add(t(L, K, W, V, H, R), K1, W1, V1, D, TOut, V2) :-
	D1 is D - 1,
	if(K == K1,
	  (V2 is V + V1,
	  if(V2 == 0,
	    once(treap:mergeTree(L, R, TOut)),
	  % else
	    TOut = t(L, K, W, V2, H, R))),
	%else
	  if(K1 @< K, (
	    treap:add(L, K1, W1, V1, D1, L2, V2),
	    treap:rotateRight(t(L2, K, W, V, H, R), TOut)),
	  %else
	    (L2 = L,
	    treap:add(R, K1, W1, V1, D1, R2, V2),
	    treap:rotateLeft(t(L2, K, W, V, H, R2), TOut)))).

treap:add(ph(PH), _, _, _, _, _, _) :-
	throw(forwardToPlaceholder(PH)).

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

treap:findDominated(ph(PH), _, _, ph(PH)).
treap:findDominated(t(L, K, W, V, H, R), D, KOut, VOut) :-
	if(termcompare:dominates(D, K),
	  treap:treeMember(t(L, K, W, V, H, R), D, KOut, VOut),
	% else
	  if(D @< K,
	    treap:findDominated(L, D, KOut, VOut),
	  % else
	    treap:findDominated(R, D, KOut, VOut))).

treap:treeMember(ph(PH), _, _, ph(PH)).
treap:treeMember(t(L, _, _, _, _, _), D, K, V) :- treap:treeMember(L, D, K, V).
treap:treeMember(t(_, K, _, V, _, _), D, K, V) :- termcompare:dominates(D, K).
treap:treeMember(t(_, _, _, _, _, R), D, K, V) :- treap:treeMember(R, D, K, V).

treap:addHook(T1, H, V, T2, NewV) :-
	treap:hookDomain(H, D),
	treap:addHook(T1, H, V, D, T2, NewV).

treap:addHook(nil(Hs), H, Hv, _, nil(Hs1), NewV) :-
	treap:updateHookValue(Hs, H, Hv, Hs1, NewV).
treap:addHook(t(L, K, W, V, Hs, R), H, Hv, D, T, NewV) :-
	if(termcompare:dominates(D, K),
	  (treap:updateHookValue(Hs, H, Hv, Hs1, NewV),
	  T = t(L, K, W, V, Hs1, R)),
	% else
	  if(D @< K,
	    (treap:addHook(L, H, Hv, D, L1, NewV),
	    T = t(L1, K, W, V, Hs, R)),
	  % else
	    (treap:addHook(R, H, Hv, D, R1, NewV),
	    T = t(L, K, W, V, Hs, R1)))).

treap:addHook(ph(PH), _, _, _, _, _) :-
	throw(forwardToPlaceholder(PH)).

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

treap:updateHookValue([], H, Hv, [kv(H, Hv)], Hv).
treap:updateHookValue([kv(K, V) | Hs], H, Hv, HsOut, Hv1) :-
	if(K =@= H,
	  (Hv1 is V + Hv,
	  if(Hv1 == 0,
	    HsOut = Hs,
	  % else
	    HsOut = [kv(H, Hv1) | Hs])),
	% else
	  (HsOut = [kv(K, V) | HsPrime],
	  treap:updateHookValue(Hs, H, Hv, HsPrime, Hv1))).

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
	
multiver:patch(add(K, V, NewV), T1, T2) :- 
	random(W), 
	max_depth(D), 
	treap:add(T1, K, W, V, D, T2, NewV).

multiver:query(get(K), T, V) :-
	treap:get(T, K, V).

multiver:patch(addHook(H, V, NewV), T1, T2) :-
	treap:addHook(T1, H, V, T2, NewV).

multiver:query(getHook(K), T, (H,V)) :-
	treap:getHook(T, K, H, V).

multiver:patch(putPlaceholder(K, PH), T1, T2) :-
	treap:putPlaceholder(T1, K, PH, T2).

multiver:patch(updatePlaceholder(K, PH1, PH2), T1, T2) :-
	treap:updatePlaceholder(T1, K, PH1, PH2, T2).

multiver:query(findDominated(D), T, (K,V)) :-
	treap:findDominated(T, D, K, V).