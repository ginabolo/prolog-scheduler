% assuming data in rdf format as follows:

% relationships:
% class: 
	% - department (eg cpsc)
	% - sections (format: cpsc_310_101), (optionally including tutorial_sections (format: cpsc_310_T1B))
	% - credits
	% - year_level
% section: 
	% - term
	% - start_time
	% - end_time
	% - days (m w f tues thurs)
	% - instructor, location
	% - class
% time(hours, minutes)



% Takes a list of classes and  list of sections and makes sure they correspond.
% Will force the ordering to match - WLOG, and should help restrict the set of answers.
schedule([Class|ListC], [Section|ListS]) :- rdf(Section, class, Class), schedule([ListC], [ListS]).


% Takes list of sections, and a constant denoting an instructor
hasInstructor([S1|Rest], Instructor) :- rdf(S1, instructor, Instructor).
hasInstructor([S1|Rest], Instructor) :- hasInstructor(Rest, Instructor).


% Takes list of sections, and a constant denoting an instructor
avoidInstructor([S1|Rest], Instructor) :- \+ rdf(S1, instructor, Instructor), avoidInstructor(Rest, Instructor).


% takes a list of sections, a constant denoting a year level, and a number of classes which should be from this year level
belowMaxPerTerm([], M1, M2) :- M1 > 0, M2 > 0.
belowMaxPerTerm([S|Rest], M1, M2) :- rdf(S, term, 1),
									 belowMaxPerTerm(Rest, MLeft, M2),
									 MLeft is M1-1.
belowMaxPerTerm([S|Rest], M1, M2) :- rdf(S, term, 2),
									 belowMaxPerTerm(Rest, M1, MLeft),
									 MLeft is M2-1.


% takes a list of classes, a constant denoting a year level, and a number of classes which should be from this year level
hasEnoughOfYearLevel(_, _, 0).
hasEnoughOfYearLevel([Class|Lst], Y, Num) :- hasEnoughOfYearLevel(Lst, Y, NumLeft),
										rdf(Class, year_level, Y),
										NumLeft is Num-1.


% takes a list of classes, a constant denoting a department, and a number of classes which should be from this department
hasEnoughOfDepartment(_, _, 0).
hasEnoughOfDepartment([Class|Lst], Dept, Num) :- hasEnoughOfDepartment(Lst, Dept, NumLeft),
										rdf(Class, department, Dept),
										NumLeft is Num-1.
hasEnoughOfDepartment([Class|Lst], Dept, Num) :- hasEnoughOfDepartment(Lst, Dept, Num),
										\+ rdf(Class, department, Dept).


% takes a list of classes, and a minimum number of credits
hasEnoughCredits(_, 0).
hasEnoughCredits([Class|Lst], NumCreds) :- hasEnoughCredits(Lst, NumCredsLeft),
										rdf(Class, credits, CredGained),
										NumCredsLeft is NumCreds-CredGained.


% takes a list of sections and a constant denoting a day
notOnDay([S1|Rest], Day) :- \+ rdf(S1, days, Day),
							notOnDay(Rest, Day).

% takes a list of sections
conflicts([S1|S2|Rest]) :- conflicts(S1, S2).
conflicts([S1|S2|Rest]) :- conflicts([S1|Rest]).
conflicts([S1|S2|Rest]) :- conflicts([S2|Rest]).

% takes two sections as input
% checks if S2 conflicts with S1
conflicts(S1, S2) :- rdf(S1, start_time, ST1), rdf(S1, end_time, ET1), 
					rdf(S2, start_time, ST2), rdf(S2, end_time, ET2), 
					rdf(S2, days, D), rdf(S2, days, D), 
					after(ST2, ST1), before(ST2, ET1).

conflicts(S1, S2) :- rdf(S1, start_time, ST1), rdf(S1, end_time, ET1), 
					rdf(S2, start_time, ST2), rdf(S2, end_time, ET2), 
					rdf(S2, days, D), rdf(S2, days, D), 
					after(ET2, ST1), before(ET2, ET1).

% takes a list of sections and a time
allBefore([], _).
allBefore([S1|Rest], Time) :- rdf(S1, start_time, ST1), before(ST1, Time), allBefore(Rest, Time).

% takes a list of sections and a time
allAfter([], _).
allAfter([S1|Rest], Time) :- rdf(S1, start_time, ST1), after(ST1, Time), allBefore(Rest, Time).

before(time(H1, _), time(H2, _)) :- H1 < H2.
before(time(H, M1), time(H, M2)) :- M1 < M2.

after(time(H1, _), time(H2, _)) :- H1 > H2.
after(time(H, M1), time(H, M2)) :- M1 > M2.