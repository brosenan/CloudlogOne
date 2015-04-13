:- module(main, [cloudlog1/0]).
:- use_module(util).


main:cloudlog1 :-
	read(Cmd),
	if(main:handleCmd(Cmd, Continue),
	  true,
	%else
	  (write('! '), write(unknownCommand(Cmd)), nl)),
	if(Continue = yes,
	  main:cloudlog1,
	% else
	  true).

main:handleCmd(heartbeat, yes) :-
	write('. alive'), nl.

main:handleCmd(end_of_file, no).
