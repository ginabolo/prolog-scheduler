% assuming data in rdf format as follows:

% relationships:
% class: 
	% - subject (eg cpsc)
	% - code (course code)
	% - name
	% - credits
% section: 
	% - subject (eg cpsc)
	% - course 
	% - code (section code)
	% - term
	% - start_hour
	% - start_minute
	% - end_hour
	% - end_minute
	% - days 
	% - instructor, location
	% - class
% time(hours, minutes)
:- include('courses.pl').
:- include('sections.pl').
:- use_module(library(time)).


% Takes a list of classes and list of sections and makes sure they correspond.
% Will force the ordering to match - WLOG, and should help restrict the set of answers.
% Remove anonymous variable, force uniqueness

% Use: Classes constraints, schedule(Classes, Sections), Section constraints
schedule(ListC, ListS) :- set(ListC), set(ListS), join(ListC, ListS),
						 \+ conflicts(ListS), 
						 subtract(ListC, [var(_)], ListC), subtract(ListS, [var(_)], ListS).

schedule(ListC) :- set(ListC), subtract(ListC, [var(_)], ListC).

schedule(ListS) :- set(ListS), \+ conflicts(ListS), subtract(ListS, [var(_)], ListS).


join([],[]).
join([Class|ListC], [Section|ListS]) :- course(Class, subject, Sub), section(Section, subject, Sub), 
											course(Class, code, C), section(Section, course, C),
											join(ListC, ListS).

% Use set before other queries to narrow down results first
set([]).
set([E|Es]) :-
   maplist(dif(E), Es),
   set(Es).

% Takes list of sections, and a constant denoting an instructor
hasInstructor([S1|_], Instructor) :- section(S1, instructor, Instructor).
hasInstructor([_|Rest], Instructor) :- hasInstructor(Rest, Instructor).


% Takes list of sections, and a constant denoting an instructor
avoidInstructor([], _).
avoidInstructor([S1|Rest], Instructor) :- section(S1, instructor, I), dif(I, Instructor), avoidInstructor(Rest, Instructor).


% takes a list of sections, a number of classes for each term
meetsMaxPerTerm([], M1, M2) :- M1 >= 0, M2 >= 0.
meetsMaxPerTerm([S|Rest], M1, M2) :- section(S, term, "1"),
									 MLeft is M1-1,
									 meetsMaxPerTerm(Rest, MLeft, M2).
meetsMaxPerTerm([S|Rest], M1, M2) :- section(S, term, "2"),
									 MLeft is M2-1,
									 meetsMaxPerTerm(Rest, M1, MLeft).

% takes a list of sections, a number of classes for each term
meetsMinPerTerm(_, M1, M2) :- M1 =< 0, M2 =< 0.
meetsMinPerTerm([S|Rest], M1, M2) :- section(S, term, "1"),
									 MLeft is M1-1,
									 meetsMinPerTerm(Rest, MLeft, M2).
meetsMinPerTerm([S|Rest], M1, M2) :- section(S, term, "2"),
									 MLeft is M2-1,
									 meetsMinPerTerm(Rest, M1, MLeft).


% takes a list of classes, a constant denoting a year level, and a number of classes which should be from this year level
hasEnoughOfYearLevel(_, _, 0).
hasEnoughOfYearLevel([Class|Lst], Y, Num) :- course(Class, code, CS),
										number_string(C, CS),
										C > Y*100,
										NumLeft is Num-1,
										hasEnoughOfYearLevel(Lst, Y, NumLeft).


% takes a list of classes, and a minimum number of credits
hasEnoughCredits(_, N) :- N =< 0.
hasEnoughCredits([Class|Lst], NumCreds) :- course(Class, credits, CredGainedS),
										number_string(CredGained, CredGainedS),
										NumCredsLeft is NumCreds-CredGained,
										hasEnoughCredits(Lst, NumCredsLeft).


% takes a list of sections and a constant denoting a day
notOnDay([], _).
notOnDay([S1|Rest], Day) :- section(S1, days, D), dif(D, Day),
							notOnDay(Rest, Day).

% takes a list of sections
conflicts([S1,S2|_]) :- conflicts(S1, S2).
conflicts([S1,_|Rest]) :- conflicts([S1|Rest]).
conflicts([_,S2|Rest]) :- conflicts([S2|Rest]).

% takes two sections as input
% checks if S2 conflicts with S1
conflicts(S1, S2) :- section(S1, start_hour, SH1), section(S1, start_minute, SM1), 
					section(S1, end_hour, EH1), section(S1, end_minute, EM1), 
					section(S2, start_hour, SH2), section(S2, start_minute, SM2), 
					section(S1, days, D), section(S2, days, D), 
					section(S1, term, T), section(S2, term, T), 
					after(time(SH2,SM2), time(SH1,SM1)), before(time(SH2,SM2), time(EH1,EM1)).

conflicts(S1, S2) :- section(S1, start_hour, SH1), section(S1, start_minute, SM1), 
					section(S1, end_hour, EH1), section(S1, end_minute, EM1), 
					section(S2, end_hour, EH2), section(S2, end_minute, EM2), 
					section(S1, days, D), section(S2, days, D), 
					section(S1, term, T), section(S2, term, T), 
					after(time(EH2, EM2), time(SH1, SM1)), before(time(EH2, EM2), time(EH1, EM1)).

% takes a list of sections and a time
allBefore([], _).
allBefore([S1|Rest], Time) :- section(S1, start_hour, SH), section(S1, start_minute, SM),
							  before(time(SH, SM, Time)), allBefore(Rest, Time).

% takes a list of sections and a time
allAfter([], _).
allAfter([S1|Rest], Time) :- section(S1, start_hour, SH), section(S1, start_minute, SM), 
							 after(time(SH, SM), Time), allBefore(Rest, Time).

before(time(SH1, _), time(SH2, _)) :- number_string(H1, SH1), number_string(H2, SH2), H1 =< H2.
before(time(H, SM1), time(H, SM2)) :- number_string(M1, SM1), number_string(M2, SM2), M1 =< M2.

after(time(SH1, _), time(SH2, _)) :- number_string(H1, SH1), number_string(H2, SH2), H1 >= H2.
after(time(H, SM1), time(H, SM2)) :- number_string(M1, SM1), number_string(M2, SM2), M1 >= M2.

