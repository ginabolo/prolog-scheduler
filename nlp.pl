:- include('courses.pl').
:- include('sections.pl').
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
    string_concat(LastUpper, ", ", R),
    string_concat(R, FirstUpper, Result).

% proposition(L0, L1, Entity) is true if L0-L1 represents a proposition that can be used for a function in the scheduler.
proposition([with, Value, credits | L], L, Entity) :- 
    number_string(Value, Str),
    course(Entity, credits, Str).

proposition([with, instructor, First, Last | L], L, Entity) :-
    instructor(First, Last, R),
    section(Entity, instructor, R).
    
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

% Noun phrase in this nlp will be made up of "courses" or "sections", followed by a proposition ("with", "before", "after", etc..), followed by a property value.
noun_phrase(L0, L4, Entity) :-
    topic(L0, L1, Entity),
    proposition(L1, L4, Entity).

question([what,are | L0],L1,Entity) :-
    noun_phrase(L0,L1,Entity).

ask(Q,A) :-
    question(Q,[],A).

q(Ans) :-
    write("Ask me: "), flush_output(current_output),
    readln(Ln),
    question(Ln,End,Ans),
    member(End,[[],['?'],['.']]).
