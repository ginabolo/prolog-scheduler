:- include('scheduler.pl').
:- include('generator.pl').

topic([courses | L], L, _).
topic([sections | L], L, _).
topic([classes | L], L, _).

hour_time(Hour, am, Result) :-
    number_string(Hour, Result).

hour_time(Hour, pm, Result) :-
    N is Hour+12,
    number_string(N, Result).

place(Building, Room, Result) :-
    string_upper(Building, BuildingUpper),
    string_concat(BuildingUpper, " ", R),
    string_concat(R, Room, Result).

instructor(First, Last, Result) :-
    string_upper(First, FirstUpper),
    string_upper(Last, LastUpper),
    string_concat(FirstUpper, " ", R),
    string_concat(R, LastUpper, Result).

% proposition(L0, L1, Entity) is true if L0-L1 represents a proposition that can be used for a function in the scheduler.
proposition([with, Value, credits | L], L, Entity) :- 
    number_string(Value, Str),
    course(Entity, credits, Str).

proposition([with, instructor, First, Last | L], L, Entity) :-
    instructor(First, Last, R),
    section(Entity, instructor, R).

proposition([taught, by, First, Last | L], L, Entity) :-
    instructor(First, Last, R),
    section(Entity, instructor, R).

proposition([in, Building, Room | L], L, Entity) :-
    place(Building, Room, R),
    section(Entity, location, R).

proposition([at, Building, Room | L], L, Entity) :-
    place(Building, Room, R),
    section(Entity, location, R).

proposition([starting, at, Time, Meridian | L], L, Entity) :-
    hour_time(Time, Meridian, R),
    section(Entity, start_hour, R).

proposition([starting, at, Hour, :, Minute, Meridian | L], L, Entity) :-
    hour_time(Hour, Meridian, R),
    section(Entity, start_hour, R),
    number_string(Minute, MS),
    section(Entity, start_minute, MS).    

proposition([ending, at, Time, Meridian | L], L, Entity) :-
    hour_time(Time, Meridian, H),
    section(Entity, end_hour, H).

proposition([ending, at, Hour, :, Minute, Meridian | L], L, Entity) :-
    hour_time(Hour, Meridian, R),
    section(Entity, end_hour, R),
    number_string(Minute, MS),
    section(Entity, end_minute, MS).

proposition([before, Time, Meridian | L], L, Entity) :-
    hour_time(Time, Meridian, H), 
    number_string(HN, H),
    section_starts_before_time(Entity, HN, 0).

proposition([before, Hour, :, Minute, Meridian | L], L, Entity) :-
    hour_time(Hour, Meridian, H), 
    number_string(HN, H),
    section_starts_before_time(Entity, HN, Minute).    

proposition([after, Time, Meridian | L], L, Entity) :-
    hour_time(Time, Meridian, H), 
    number_string(HN, H),
    section_starts_after_time(Entity, HN, 0).

proposition([after, Hour, :, Minute, Meridian | L], L, Entity) :-
    hour_time(Hour, Meridian, H), 
    number_string(HN, H),
    section_starts_after_time(Entity, HN, Minute).

proposition([between,T0,MD0,and,T1,MD1 | L], L, Entity) :-
    hour_time(T0, MD0, H0),
    hour_time(T1, MD1, H1),
    number_string(HN0, H0),
    number_string(HN1, H1),
    section_between_time(Entity,HN0,0,HN1,0).

proposition([between,T0,:,M0,MD0,and,T1,:,M1,MD1 | L], L, Entity) :-
    hour_time(T0, MD0, H0),
    hour_time(T1, MD1, H1),
    number_string(HN0, H0),
    number_string(HN1, H1),
    section_between_time(Entity,HN0,M0,HN1,M1).

proposition([about, T1, T2|L], L, Entity) :-
    string_concat(T1, " ", TS),
    string_concat(TS, T2, T),
    string_lower(T, TL),
    course(Entity, description, DD),
    string_lower(DD, DDL),
    course(C, name, DN),
    string_lower(DN, DNL),
    (sub_string(DDL, _, _, _, TL); sub_string(DNL, _, _, _, TL)),
    course(C, subject, CS),
    course(C, code, CC),
    section(Entity, subject, CS),
    section(Entity, course, CC).

proposition([about, Topic | L], L, Entity) :-
    string_concat(Topic, "", T1),
    string_lower(T1, T1L),
    course(C, description, DD),
    string_lower(DD, DDL),
    course(C, name, DN),
    string_lower(DN, DNL),
    (sub_string(DDL, _, _, _, T1L); sub_string(DNL, _, _, _, T1L)),
    course(C, subject, CS),
    course(C, code, CC),
    section(Entity, subject, CS),
    section(Entity, course, CC).

% A joining phrase allows for multiple constraints to be specified. It can be delimited by various tokens, such as a comma, "and", "that is/are", and even a blank string.
% Joining phrases can be used to chain together queries, returning entities that satisfy all constraints.
joining_phrase(L, L2, Entity) :-
    proposition(L, L1, Entity),
    joining_phrase(L1, L2, Entity).

joining_phrase([,|L], L2, Entity) :-
    proposition(L, L1, Entity),
    joining_phrase(L1, L2, Entity).

joining_phrase([and|L], L2, Entity) :-
    proposition(L, L1, Entity),
    joining_phrase(L1, L2, Entity).

joining_phrase([that, is|L], L2, Entity) :-
    proposition(L, L1, Entity),
    joining_phrase(L1, L2, Entity).

joining_phrase([that, are|L], L2, Entity) :-
    proposition(L, L1, Entity),
    joining_phrase(L1, L2, Entity).

joining_phrase([], [], _).

% Noun phrase in this nlp will be made up of "courses" or "sections", followed by a proposition ("with", "before", "after", etc..), followed by a property value.
noun_phrase(L0, L2, Entity) :-
    topic(L0, L1, Entity),
    proposition(L1, L2, Entity).

noun_phrase(L0, L3, Entity) :-
    topic(L0, L1, Entity),
    proposition(L1, L2, Entity),
    joining_phrase(L2, L3, Entity).

question(L0, L1, Entity) :-
    noun_phrase(L0, L1, Entity).

question([what, are|L0], L1, Entity) :-
    noun_phrase(L0, L1, Entity).

% Validation functions


% Schedule Generation
schedule_parameter([not, taught, by, First, Last|L], L, Schedule) :-
    dif(Schedule, []),
    atom_string(First, FN),
    atom_string(Last, LN),
    string_concat(FN, " ", N1),
    string_concat(N1, LN, FullName),
    string_upper(FullName, Upper),
    avoidInstructor(Schedule, Upper).

schedule_parameter([in, term, TermNumber|L], L, Schedule) :-
    dif(Schedule, []),
    all_in_term(Schedule, TermNumber).

schedule_parameter([without, instructor, First, Last|L], L, Schedule) :-
    dif(Schedule, []),
    atom_string(First, FN),
    atom_string(Last, LN),
    string_concat(FN, " ", N1),
    string_concat(N1, LN, FullName),
    string_upper(FullName, Upper),
    avoidInstructor(Schedule, Upper).

schedule_parameter([after, Hour, Minute|L], L, Schedule) :-
    dif(Schedule, []),
    all_start_after_time(Schedule, Hour, Minute).

schedule_parameter([before, Hour, Minute|L], L, Schedule) :-
    dif(Schedule, []),
    all_end_before_time(Schedule, Hour, Minute).

schedule_parameter([not, on, D1, D2|L], L, Schedule) :-
    dif(Schedule, []),
    atom_string(D1, DS1),
    atom_string(D2, DS2),
    string_concat(DS1, " ", DP1),
    string_concat(DP1, DS2, D),
    notOnDay(Schedule, D).

schedule_parameter([not, on, D1, D2, D3|L], L, Schedule) :-
    dif(Schedule, []),
    atom_string(D1, DS1),
    atom_string(D2, DS2),
    atom_string(D3, DS3),
    string_concat(DS1, " ", DP1),
    string_concat(DP1, DS2, DP2),
    string_concat(DP2, " ", DP3),
    string_concat(DP3, DS3, D),
    notOnDay(Schedule, D).

joining_parameter([,|L], L2, Entity) :-
    schedule_parameter(L, L1, Entity),
    joining_parameter(L1, L2, Entity).

joining_parameter([], [], _).

schedule_courses([P, courses, C1, C2, C3, C4, C5, C6, ','|R], R, Schedule) :-
    member(P, [for, with, having]),
    valid_courses([C1, C2, C3, C4, C5, C6]),
    schedule([C1, C2, C3, C4, C5, C6], Schedule).

schedule_courses([P, courses, C1, C2, C3, C4, C5, ','|R], R, Schedule) :-
    member(P, [for, with, having]),
    valid_courses([C1, C2, C3, C4, C5]),
    schedule([C1, C2, C3, C4, C5], Schedule).

schedule_courses([P, courses, C1, C2, C3, C4, ','|R], R, Schedule) :-
    member(P, [for, with, having]),
    valid_courses([C1, C2, C3, C4]),
    schedule([C1, C2, C3, C4], Schedule).

schedule_courses([P, courses, C1, C2, C3, ','|R], R, Schedule) :-
    member(P, [for, with, having]),
    valid_courses([C1, C2, C3]),
    schedule([C1, C2, C3], Schedule).

schedule_courses([P, courses, C1, C2, ','|R], R, Schedule) :-
    member(P, [for, with, having]),
    valid_courses([C1, C2]),
    schedule([C1, C2], Schedule).

schedule_courses([P, courses, C1, ','|R], R, Schedule) :-
    member(P, [for, with, having]),
    valid_courses([C1]),
    schedule([C1], Schedule).


% Generate
generate_schedule([generate, schedule|L0], L3, Schedule) :-
    schedule_courses(L0, L1, Schedule),
    schedule_parameter(L1, L3, Schedule).

% Start
ask(Q,A) :-
    question(Q,[],A).

resolve([]).
resolve([H|T]) :-
    call(H),
    resolve(T).

search(Result) :-
    write("Query: "),
    flush_output(current_output),
    readln(Line),
    question(Line, End, Result),
    member(End, [[], ['?'], ['.']]).

generate(Schedule) :-
    write("Parameters: "),
    flush_output(current_output),
    readln(Line),
    generate_schedule(Line, End, Schedule),
    member(End, [[], ['?'], ['.']]).


% Try:
% facts about cpsc_312
% generate schedule for courses cpsc_210 , without instructor elisa baniassad
% generate schedule for courses cpsc_312 , without instructor david poole
% generate schedule for courses cpsc_304 cpsc_311 cpsc_312 cpsc_313 cpsc_322 , in term 1
% generate schedule for courses cpsc_304 cpsc_311 cpsc_312 cpsc_313 cpsc_322 , after 9 0
% generate schedule for courses cpsc_310 cpsc_213 , not on Tue Thu
