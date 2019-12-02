:- use_module(library(clpfd)).

%sections_before_time([], _, _, R, P).
%sections_before_time([S1|SR], TH, TM, R, P) :- (\+ member(S1, P)), (\+ section_before_time(S1, TH, TM)), sections_before_time(SR, TH, TM, R, [S1|P]).
%sections_before_time([S1|SR], TH, TM, R, P) :- (\+ member(S1, P)), section_before_time(S1, TH, TM), sections_before_time(SR, TH, TM, [S1|R], [S1|P]).
%sections_before_time([S1|SR], TH, TM, R, P) :- member(S1, P), sections_before_time(SR, TH, TM, R, P).

section_starts_before_time(S, TH, _) :-
        section(S, start_hour, SH),
        number_string(SHN, SH),
        SHN #< TH.
section_starts_before_time(S, TH, TM) :-
        section(S, start_hour, SH), number_string(SHN, SH),
        section(S, start_minute, SM),
        number_string(SMN, SM),
        SHN #= TH,
        SMN #< TM.

% generate_schedule(+ Courses, + Constraints, - Schedule) generates a schedule involving all courses given satisfying all constraints. 
% It takes in courses as a list of course atoms.
% It takes in constraints as ?
% The returned schedule will be a list of sections, in which its information (such as its time) can be obtained by calling section() on it.

schedule([], _, _, _, _, _, Schedule, Schedule).
schedule([Course|Courses], StartHour, StartMinute, EndHour, EndMinute, Term, Accumulator, Schedule) :-
        course(Course, subject, Subject),
        course(Course, code, Code),
        section(Section, subject, Subject),
        section(Section, course, Code),
        section(Section, term, Term),
        section_between_time(Section, StartHour, StartMinute, EndHour, EndMinute),
        no_overlapping_sections([Section|Accumulator]),
        schedule(Courses, StartHour, StartMinute, EndHour, EndMinute, Term, [Section|Accumulator], Schedule).

% TODO schedule_without_duplicates() :- setof(_, (...), _).

% TODO fix bug - each section must be compared to every other subsequent section
no_overlapping_sections([]).
no_overlapping_sections([_]).
no_overlapping_sections([Section1, Section2]) :-
        \+ overlapping(Section1, Section2).
no_overlapping_sections([Section1, Section2|Sections]) :-
        no_overlapping_sections([Section1|Sections]),
        no_overlapping_sections([Section2|Sections]).

% TODO check days when overlapping, TODO must not backtrack
overlapping(Section1, Section2) :-
    section_times(Section1, SectionStartHour1, SectionStartMinute1, SectionEndHour1, SectionEndMinute1),
    SectionStartTime1 is SectionStartHour1 * 60 + SectionStartMinute1,
    SectionEndTime1 is SectionEndHour1 * 60 + SectionEndMinute1,
    section_times(Section2, SectionStartHour2, SectionStartMinute2, SectionEndHour2, SectionEndMinute2),
    SectionStartTime2 is SectionStartHour2 * 60 + SectionStartMinute2,
    SectionEndTime2 is SectionEndHour2 * 60 + SectionEndMinute2,
    (between(SectionStartTime2, SectionEndTime2, SectionStartTime1)).

overlapping(Section1, Section2) :-
    section_times(Section1, SectionStartHour1, SectionStartMinute1, SectionEndHour1, SectionEndMinute1),
    SectionStartTime1 is SectionStartHour1 * 60 + SectionStartMinute1,
    SectionEndTime1 is SectionEndHour1 * 60 + SectionEndMinute1,
    section_times(Section2, SectionStartHour2, SectionStartMinute2, SectionEndHour2, SectionEndMinute2),
    SectionStartTime2 is SectionStartHour2 * 60 + SectionStartMinute2,
    SectionEndTime2 is SectionEndHour2 * 60 + SectionEndMinute2,
    between(SectionStartTime2, SectionEndTime2, SectionEndTime1).

section_between_time(S, StartHour, StartMinute, EndHour, EndMinute) :-
        section(S, start_hour, SectionStartHour),
        number_string(SectionStartHourNumber, SectionStartHour),
        section(S, start_minute, SectionStartMinute),
        number_string(SectionStartMinuteNumber, SectionStartMinute),
        SectionStartHourNumber * 60 + SectionStartMinuteNumber #>= StartHour * 60 + StartMinute,
        section(S, end_hour, SectionEndHour),
        number_string(SectionEndHourNumber, SectionEndHour),
        section(S, end_minute, SectionEndMinute),
        number_string(SectionEndMinuteNumber, SectionEndMinute),
        SectionEndHourNumber * 60 + SectionEndMinuteNumber #=< EndHour * 60 + EndMinute.

section_times(Section, StartHour, StartMinute, EndHour, EndMinute) :-
        section(Section, start_hour, SectionStartHour),
        number_string(StartHour, SectionStartHour),
        section(Section, start_minute, SectionStartMinute),
        number_string(StartMinute, SectionStartMinute),
        section(Section, end_hour, SectionEndHour),
        number_string(EndHour, SectionEndHour),
        section(Section, end_minute, SectionEndMinute),
        number_string(EndMinute, SectionEndMinute).
