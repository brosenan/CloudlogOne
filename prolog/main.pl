:- module(main, [cloudlog1/0]).
:- use_module(treap).
:- use_module(util).
:- use_module(partialLogic).
:- use_module(multiver).
:- use_module(hashedTree).

main:cloudlog1 :-
	rb_empty(T),
	if(main:cloudlog1(T),
	  true,
	% else
	  (write('! stopped'), nl)).

main:cloudlog1(T1) :-
	read(Cmd),
	catch(
	  if(main:handleCmd(Cmd, T1, T2, Continue),
	    true,
	  %else
	    (write('! '), main:mywrite(unknownCommand(Cmd)), nl)),
	% catch
	  Err, (write('! '), main:mywrite(error(Err)), nl)),
	if(Continue = yes,
	  main:cloudlog1(T2),
	% else
	  true).

main:handleCmd(heartbeat, T, T, yes) :-
	write('. alive'), nl.

main:handleCmd(end_of_file, T, T, no).

main:handleCmd(create(Patch), C1, C2, yes) :-
	multiver:empty(M0),
	hashedTree:empty(T0),
	multiver:query(getHash, T0, H0),
	multiver:init(M0, T0, H0, M1),
	multiver:patch(Patch, M1, H0, H1, M2),
	rb_insert(C1, H1, M2, C2),
	writeHash(H1, H1).

main:handleCmd(on((Hc,Hv1), Op), C1, C2, yes) :-
	rb_lookup(Hc, M1, C1),
	forall(
	  multiver:query(Op, M1, Hv1, R),
	% do
	  if(main:hasPlaceholder(R, PH),
	    main:writeUpstream(PH, Op),
	  % else
	    (write(': '), main:mywrite(R), nl))),
	catch(
	  catch(
	    multiver:patch(Op, M1, Hv1, Hv2, M2),
	  % catch
	    treap_error(depth_limit_exceeded(Hooks)),
	    (main:convertHooks(Hooks, HookPatches),
	    main:calcInitialHash([Op|HookPatches], InitialHash),
	    writeUpstream((InitialHash, '_'), [Op|HookPatches]), 
	    (Hv2, M2) = (Hv1, M1))),
	% catch
	  forwardToPlaceholder(PH),
	  (main:writeUpstream(PH, Op),
	  (Hv2, M2) = (Hv1, M1))),
	rb_insert(C1, Hc, M2, C2),
	writeHash(Hc, Hv2).

main:handleCmd(set_max_depth(D), C, C, yes) :-
	retract(treap:max_depth(OldD)),
	assert(treap:max_depth(D)),
	write('. was: '),
	write(OldD),
	nl.

main:handleCmd(dump((Hc, Hv)), C, C, yes) :-
	rb_lookup(Hc, M, C),
	rb_lookup(Hv, T, M),
	write('. '),
	main:mywrite(T),
	nl.

main:writeHash(H1, H2) :-
	write('. '),
	mywrite((H1,H2)),
	nl.

mywrite(Term) :-
	write_term(Term, [quoted(true)]).

main:hasPlaceholder(ph(PH), PH).
main:hasPlaceholder((_, ph(PH)), PH).

main:writeUpstream(PH, Op) :-
	if(main:getKey(Op, Key),
	(
	    write('@ '), 
	    main:mywrite(Key),
	    nl
	),
	(
	    true
	)),
	write('? '), 
	main:mywrite(PH), 
	write(' '),
	main:mywrite(Op), 
	nl.

main:convertHooks([], []).
main:convertHooks([kv(K, V) | Hooks], [add_m(K, V) | Patches]) :-
	main:convertHooks(Hooks, Patches).

main:calcInitialHash(Patch, Hash) :-
	hashedTree:empty(T0),
	multiver:patch(Patch, T0, T1),
	multiver:query(getHash, T1, Hash).

main:getKey(add_v(Key, _), Key).
main:getKey(add_m(Rule, _), Key) :-
	treap:hookDomain(Rule, Key).
main:getKey(logicQuery(_, Key, _), Key).
main:getKey([Op | _], Key) :-
	main:getKey(Op, Key).