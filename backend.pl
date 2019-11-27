% assuming data in rdf format as follows:

% relationships:
% class: 
	% - sections (format: cpsc_310_101) 
	% - credits
	% - (optional) tutorial_sections (format: cpsc_310_T1B))
	% - year_level
	% - prereqs
% section: 
	% - term
	% - start_time
	% - end_time
	% - days (mwf or tuesthurs)
	% - instructor, location
% time(hours, minutes)


%Sketchy Functionality:
%Times, days, upper/lower level, num credits
%Query for info, also make me a schedule


% takes a list of classes, and a minimum number of credits
hasEnoughCredits(_, 0).
hasEnoughCredits([Class|Lst], NumCreds) :- hasEnoughCredits(Lst, NumCredsLeft),
										rdf(Class, credits, CredGained),
										NumCredsLeft is NumCreds-CredGained .


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

before(time(H1, _), time(H2, _)) :- H1 < H2.
before(time(H, M1), time(H, M2)) :- M1 < M2.

after(time(H1, _), time(H2, _)) :- H1 > H2.
after(time(H, M1), time(H, M2)) :- M1 > M2.