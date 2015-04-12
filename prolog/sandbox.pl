:- module(sandbox, []).

:- use_module(util).

sandbox:eval(Res, Goal) :-
	util:time_out(findall(Res, Goal, Rs), 100, Status),
	if(Status = success,
	  member(Res, Rs),
	% else
	  throw(timed_out(Goal))).
