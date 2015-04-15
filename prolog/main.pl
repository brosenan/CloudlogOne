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
	    treap_error(depth_limit_exceeded([])),
	    (writeUpstream(('_', '_'), []), (Hv2, M2) = (Hv1, M1))),
	% catch
	  forwardToPlaceholder(PH),
	  (main:writeUpstream(PH, Op),
	  (Hv2, M2) = (Hv1, M1))),
	rb_insert(C1, Hc, M2, C2),
	writeHash(Hc, Hv2).

main:writeHash(H1, H2) :-
	write('. '),
	mywrite((H1,H2)),
	nl.

mywrite(Term) :-
	write_term(Term, [quoted(true)]).

main:hasPlaceholder(ph(PH), PH).
main:hasPlaceholder((_, ph(PH)), PH).

main:writeUpstream(PH, Op) :-
	write('? '), 
	main:mywrite(PH), 
	write(' '), 
	main:mywrite(Op), 
	nl.