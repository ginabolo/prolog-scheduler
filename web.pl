:- dynamic courses/3.

:- use_module(library(persistency)).
:- use_module(library(http/http_client)).

get(Data) :- http_get("https://m454hj0nc0.execute-api.ca-central-1.amazonaws.com/default/helloworldtest", Data, []).

save(Data, File) :- open(File, write, Stream), write(Stream, Data), close(Stream).

start :- get(Data), save(Data, "courses.pl"), consult("courses.pl").
