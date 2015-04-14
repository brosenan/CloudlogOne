:- module(main, [cloudlog1/0]).
:- use_module(util).
:- use_module(partialLogic).
:- use_module(multiver).
:- use_module(hashedTree).

main:cloudlog1 :-
	rb_empty(T),
	main:cloudlog1(T).

main:cloudlog1(T1) :-
	read(Cmd),
	if(main:handleCmd(Cmd, T1, T2, Continue),
	  true,
	%else
	  (write('! '), write(unknownCommand(Cmd)), nl)),
	if(Continue = yes,
	  main:cloudlog1(T2),
	% else
	  true).

main:handleCmd(heartbeat, T, T, yes) :-
	write('. alive'), nl.

main:handleCmd(end_of_file, T, T, no).

main:handleCmd(create(List), T1, T1, yes) :-
	hashedTree:empty(L0),
	multiver:query(getHash, L0, H),
	write('. '),
	write(H),
	write(':'),
	write(H),
	nl.