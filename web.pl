:- dynamic courses/3.

:- use_module(library(persistency)).
:- use_module(library(http/http_client)).

get_sections(SectionData) :- http_get("https://ngo8sjao28.execute-api.ca-central-1.amazonaws.com/default/get_section_data_rdf", SectionData, []).
get_courses(CourseData) :- http_get("https://ngo8sjao28.execute-api.ca-central-1.amazonaws.com/default/get_course_data_rdf", CourseData, []).

save(Data, File) :- open(File, write, Stream), write(Stream, Data), close(Stream).

start :- get_sections(SectionData), get_courses(CourseData), save(SectionData, "sections.pl"), save(CourseData, "courses.pl"), consult("sections.pl"), consult("courses.pl").
