# prolog-scheduler: A Prolog utility for building UBC CS schedules given a set of constraints in (constrained) natural language

To download data, load \[web\]. then execute `start.`

Once data has been populated, load \[nlp\].
Run `generate(Schedule).` then input a command of the form: "generate schedule \[for | with | having\] (space-separated list of course symbols: cpsc_###) PROPOSITION+"

Available propositions: not taught by FNAME LNAME, without instructor FNAME LNAME, in term #, after H M, before H M, not on D1 D2 D3? 

Examples:
- generate schedule for courses cpsc_210 , without instructor elisa baniassad
- generate schedule for courses cpsc_312 , without instructor david poole
- generate schedule for courses cpsc_304 cpsc_311 cpsc_312 cpsc_313 cpsc_322 , in term 1
- generate schedule for courses cpsc_304 cpsc_311 cpsc_312 cpsc_313 cpsc_322 , after 9 0
- generate schedule for courses cpsc_310 cpsc_213 , not on Tue Thu


Or run `search(Result).` then input a question of the form "(what are)? TOPIC PROPOSITION+"
Topic: courses, sections, classes
Available propositions:  with VAL credits, with instructor FNAME LNAME, taught by FNAME LNAME, before, after, taught by, in BUILDING ROOM, at BUILDING ROOM, starting at / ending at / before / after H am/pm M, between H am/pm M and H2 am/pm M2
Propositions can be chained with comma, "and", "that is/are", and nothing.
- what are courses about logic
- sections that are after 5 pm.
- sections ending at 12 pm
- what are sections taught by david poole?


Limitations: Searches over sections given courses, in order to limit search space.
