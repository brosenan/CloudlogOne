:- module(util, [if/3]).

if(C, T, _) :- C, !, T.
if(_, _, E) :- E.

util:enforce(G) :-
	if(G, true, (
	  write('Failed: '),
	  write(G),
	  nl,
	  fail)).

util:debug(G) :-
	write('before: '), write(G), nl,
	G,
	write('after: '), write(G), nl.

util:pivotSplit([], L2, _, [], L2).
util:pivotSplit([X | L1], L2, Pivot, L1Out, L2Out) :-
	if(X @< Pivot,
	  (L1Out = [X | L1Prime],
	  L2Out = L2Prime),
	% else
	  (L1Out = L1Prime,
	  L2Out = [X | L2Prime])),
	pivotSplit(L1, L2, Pivot, L1Prime, L2Prime).


util:codesToB64(Codes, B64) :-
	atom_codes(Atom, Codes),
	base64(Atom, B64).

util:updateHash(H1, Term, H2) :-
	saturate(Term, Sat),
	term_to_atom(Sat, Atom),
	sha_hash(Atom, H3, []),
	xorList(H1, H3, H2).

xorList([], [], []).
xorList([A | As], [B | Bs], [C | Cs]) :-
	C is A xor B,
	xorList(As, Bs, Cs).


saturate(Term, Sat) :-
	copy_term(Term, Sat),
	saturate(Sat, 1, _).

saturate(Term, N1, N2) :-
	if(var(Term),
	  (Term = '$SAT'(N1),
	  N2 is N1 + 1),
	% else
	  (Term =.. [_ | Args],
	  saturateList(Args, N1, N2))).

saturateList([], N, N).
saturateList([Term | List], N1, N2) :-
	saturate(Term, N1, N3),
	saturateList(List, N3, N2).



% Copied from SICStus compatibility library for SWI-Prolog
time_out(Goal, Time_ms, Result) :-
	Time_s is (Time_ms//1)/1000,
        catch( ( Result0 = success,
                 setup_call_cleanup(
                        alarm(Time_s, throw(time_out), Id),
                        Goal,
                        ( Removed = true, remove_alarm(Id) )),
                 (   var(Removed)
                 ->  uninstall_alarm(Id),
                     ( true ; install_alarm(Id,Time_s), fail )
                 ;   true
                 )
	       ),
	       time_out,
	       Result0 = time_out ),
        Result = Result0.
