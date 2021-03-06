:- module(hashedTree, []).
:- use_module(treap).

hashedTree:empty(hashed(T, H)) :-
	treap:empty(T),
	sha_hash('', H, []).

multiver:query(h(Q), hashed(T, _), V) :-
	multiver:query(Q, T, V).

multiver:query(getHash, hashed(_, H), B64) :- !, %%%%
	util:codesToB64(H, B64).

multiver:patch(h_add(K, V, V1), hashed(T1, H1), hashed(T2, H2)) :-
	multiver:patch(add(K, V, V1), T1, T2),
	V2 is V1 - V, % The value before the change
	util:updateHash(H1, (K, V2), H3),
	util:updateHash(H3, (K, V1), H2).

multiver:patch(h_addHook(Hook, V, K, V1), hashed(T1, H1), hashed(T2, H2)) :-
	multiver:patch(addHook(Hook, V, K, V1), T1, T2),
	V2 is V1 - V,
	util:updateHash(H1, hook(K, Hook, V2), H3),
	util:updateHash(H3, hook(K, Hook, V1), H2).

multiver:patch(h_putPlaceholder(K, PH), hashed(T1, H1), hashed(T2, H2)) :-
	multiver:patch(putPlaceholder(K, PH), T1, T2),
	util:updateHash(H1, ph(PH), H2).

multiver:patch(h_updatePlaceholder(K, PH1, PH2), hashed(T1, H1), hashed(T2, H2)) :-
	multiver:patch(updatePlaceholder(K, PH1, PH2), T1, T2),
	util:updateHash(H1, ph(PH1), H3),
	util:updateHash(H3, ph(PH2), H2).
