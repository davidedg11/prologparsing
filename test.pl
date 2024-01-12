%'Approccio con uso dell append'
sentence(X) :- append(Y, Z, X), noun_phrase(Y), verb_phrase(Z).
noun_phrase(X) :- append(Y, Z, X), determiner(Y), noun(Z).
verb_phrase(X) :- verb(X).
verb_phrase(X) :- append(Y, Z, X), verb(Y), noun_phrase(Z).

determiner([il]).
determiner([un]).
noun([cane]).
noun([biscotto]).
verb([mangia]).

%'Approccio con liste di differenza'
sentence(Input, Rest) :- noun_phrase(Input, Part), verb_phrase(Part, Rest).
noun_phrase(Input, Rest) :- determiner(Input, Part), noun(Part, Rest).
verb_phrase(Input, Rest) :- verb(Input, Rest).
verb_phrase(Input, Rest) :- verb(Input, Part), noun_phrase(Part, Rest).

determiner([il|Rest], Rest).
determiner([un|Rest], Rest).
noun([cane|Rest], Rest).
noun([biscotto|Rest], Rest).
verb([mangia|Rest], Rest).

%'Approccio DCG'
sentence --> noun_phrase, verb_phrase.
noun_phrase --> determiner, noun.
verb_phrase --> verb.
verb_phrase --> verb, noun_phrase.

determiner --> [il].
determiner --> [un].
noun --> [cane].
noun --> [biscotto].
verb --> [mangia].

%'Aproccio DCG con gestione della concordanza numerica'
sentence(Number) --> noun_phrase(Number), verb_phrase(Number).
noun_phrase(Number) --> determiner(Number), noun(Number).
verb_phrase(Number) --> verb(Number).
verb_phrase(Number) --> verb(Number), noun_phrase(_).

determiner(singular) --> [il].
determiner(plural) --> [i].
noun(singular) --> [cane].
noun(plural) --> [cani].
noun(singular) --> [biscotto].
noun(plural) --> [biscotti].
verb(singular) --> [mangia].
verb(plural) --> [mangiano]. 

%'Aproccio DCG con gestione della concordanza numerica e produzione del parse tree'
sentence(Number,  sentence(NP, VP)) --> noun_phrase(Number, NP), 
         verb_phrase(Number, VP). 
noun_phrase(Number, noun_phrase(DET, N)) --> determiner(Number, DET), 
         noun(Number, N). 
verb_phrase(Number, verb_phrase(V)) --> verb(Number, V).
verb_phrase(Number, verb_phrase(V, NP)) --> verb(Number, V), noun_phrase(_, NP). 

determiner(singular, determiner(il)) --> [il]. 
determiner(plural, determiner(il)) --> [i].

noun(singular, noun(cane)) --> [cane]. 
noun(plural, noun(cani)) --> [cani].

noun(singular, noun(biscotto)) --> [biscotto]. 
noun(plural, noun(biscotti)) --> [biscotti].

verb(singular, verb(mangia)) --> [mangia].
verb(plural, verb(mangiano)) --> [mangiano].

