:- include('generator.pl').
:- include('scheduler.pl').

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
    noun_phrase(L0,L1,Entity).


%%% Schedule Generation
schedule_parameter([in, term, TermNumber|L], L, Schedule) :-
    all_in_term(Schedule, TermNumber).
    % TODO recurserest of parameters

joining_parameter([,|L], L2, Entity) :-
    schedule_parameter(L, L1, Entity),
    joining_parameter(L1, L2, Entity).

joining_parameter([], [], _).

schedule_courses([for, courses|Courses], R, Schedule) :-
    schedule(Courses, Schedule).
    % TODO add recursion for R

schedule_courses([with, courses|Courses], R, Schedule) :-
    schedule(Courses, Schedule).
    % TODO add recursion for R

schedule_courses([having, courses|Courses], R, Schedule) :-
    schedule(Courses, Schedule).
    % TODO add recursion for R

generate_schedule([generate, schedule|L0], L1, Schedule) :-
    schedule_courses(L0, L1, Schedule).

generate_schedule([generate, schedule|L0], L3, Schedule) :-
    schedule_courses(L0, L1, Schedule),
    schedule_parameter(L1, L2, Schedule),
    joining_parameter(L2, L3, Schedule).



%%% Start
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
