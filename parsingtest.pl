:- use_module(library(apply)).
:- use_module(library(gv)).
:- use_module(library(yall)).
:- use_module(library(term_ext)).


sentence(Number, Gender, sentence(NP, VP)) --> noun_phrase(Number, Gender, NP), verb_phrase(Number, Gender, VP).
% sentence(Number, Gender, sentence(NP, CONJ, NP, VP)) --> noun_phrase(Number, Gender, NP), conjuction(_, _, CONJ), noun_phrase(singular, ma, NP), verb_phrase(plural, _, VP).
sentence(Number, Gender, sentence(NP, PP, VP)) --> noun_phrase(Number, Gender, NP), prepositional_phrase(_, _, PP), verb_phrase(Number, Gender, VP).
sentence(Number, Gender, sentence(NP, VP, PP)) --> noun_phrase(Number, Gender, NP), verb_phrase(Number, Gender, VP), prepositional_phrase(_, _, PP) .

noun_phrase(Number, Gender, noun_phrase(DET, N)) --> determiner(Number, Gender, DET), noun(Number, Gender, N).
noun_phrase(Number, Gender, noun_phrase(DET, N, REL)) --> determiner(Number, Gender, DET), noun(Number, Gender, N), relative_clause(Number, _, REL).

noun_phrase(Number, Gender, noun_phrase(DET, N, ADJ)) --> determiner(Number, Gender, DET), noun(Number, Gender, N), adjective(Number, Gender,ADJ).
noun_phrase(Number, Gender, noun_phrase(DET, N, ADJ, REL)) --> determiner(Number, Gender, DET), noun(Number, Gender, N), adjective(Number, Gender, ADJ), relative_clause(Number, _, REL).

relative_clause(Number, _, relative_clause(RP, VP)) --> relative_pronoun(_, _, RP), verb_phrase(Number, _, VP).
prepositional_phrase(Number, Gender, prepositional_phrase(PREP, NP)) --> preposition(_, _, PREP), noun_phrase(Number, Gender, NP).

verb_phrase(Number, _, verb_phrase(V)) --> transitive_verb(Number, V).
verb_phrase(Number, _, verb_phrase(V, NP)) --> transitive_verb(Number, V), noun_phrase(_, _, NP).
verb_phrase(Number, _, verb_phrase(V)) --> intransitive_verb(Number, V).

verb_phrase(Number, _, verb_phrase(V, ADV, NP)) --> transitive_verb(Number, V), adverb(_, _, ADV), noun_phrase(_, _, NP).
verb_phrase(Number, _, verb_phrase(V, ADV)) --> transitive_verb(Number, V), adverb(_, _, ADV).
verb_phrase(Number, _, verb_phrase(V, ADV)) --> intransitive_verb(Number, V), adverb(_, _, ADV).

% verb_phrase(Number, _, verb_phrase(V, NP, PP)) --> transitive_verb(Number, V), noun_phrase(_, _, NP), prepositional_phrase(_, _, PP).
% verb_phrase(Number, _, verb_phrase(V, ADV, NP, PP)) --> transitive_verb(Number, V), adverb(_, _, ADV), noun_phrase(_, _, NP), prepositional_phrase(_, _, PP).

determiner(singular, ma, determiner(il)) --> [il].
determiner(singular, fem, determiner(la)) --> [la].
determiner(plural, ma, determiner(i)) --> [i].
determiner(plural, fem, determiner(le)) --> [le].

noun(singular, ma, noun(ragazzo)) --> [ragazzo].
noun(singular, fem, noun(ragazza)) --> [ragazza].
noun(plural, ma, noun(ragazzi)) --> [ragazzi].
noun(plural, fem, noun(ragazze)) --> [ragazze].

noun(singular, fem, noun(giraffa)) --> [giraffa].
noun(plural, fem, noun(giraffe)) --> [giraffe].

noun(singular, ma, noun(leone)) --> [leone].
noun(plural, ma, noun(leoni)) --> [leoni].

noun(singular, ma, noun(binocolo)) --> [binocolo].

adjective(singular, ma, adjective(biondo)) --> [biondo].
adjective(singular, fem, adjective(bionda)) --> [bionda].
adjective(plural, ma, adjective(biondi)) --> [biondi].
adjective(plural, fem, adjective(bionde)) --> [bionde].

adjective(singular, ma, adjective(alto)) --> [alto].
adjective(singular, fem, adjective(alta)) --> [alta].
adjective(plural, ma, adjective(alti)) --> [alti].
adjective(plural, fem, adjective(alte)) --> [alte].

adjective(singular, _, adjective(feroce)) --> [feroce].
adjective(plural, _, adjective(feroci)) --> [feroci].

adverb(_, _, adverb(attentamente)) --> [attentamente].

relative_pronoun(_, _, relative_pronoun(che)) --> [che].

preposition(_, _, preposition(con)) --> [con].

% conjuction(_, _,conjuction(e)) --> [e].

transitive_verb(singular, transitive_verb(osserva)) --> [osserva].
transitive_verb(plural, transitive_verb(osservano)) --> [osservano].
intransitive_verb(singular, intransitive_verb(dorme)) --> [dorme].
intransitive_verb(plural, intransitive_verb(dormono)) --> [dormono].

%'?- phrase(GrammarRule, [InputList]).'
test_sentence(X) :- 
   phrase(sentence(_, _, _), X).

% 'Tutte le frasi dove il soggetto è maschile singolare ?- sentence(singular, ma, _, X, []). '

%'?- generate_parse_tree(_, _, [InputList], Tree).'
%'In modo equivalente: ?- sentence(_, _, T, [InputList], []).'
generate_parse_tree(Sentence, Tree) :-
  sentence(_, _, Tree, Sentence, []).

% 'Parse tree di tutte le frasi possibili secondo la concordanza sintattica ?- sentence(_,_, Tree, X, []).'

%'Per esportare un parse tree in formato DOT, che può poi essere convertito in un file SVG utilizzando la libreria Graphviz.'
export_tree_(Out, Tree, Id) :-
  Tree =.. [Op|Trees],
  ascii_id(Id),
  dot_node_id(Out, Id, options{label: Op}),
  maplist(export_tree_(Out), Trees, Ids),
  maplist(dot_edge_id(Out, Id), Ids).

%'?- view_parse_tree(_, _, [InputList], Tree). Viene visualizzato a schermo il parse tree'
view_parse_tree(Sentence, Tree) :-
  sentence(_, _, Tree, Sentence, []),
  gv_view({Tree}/[Out0]>>export_tree_(Out0, Tree, _)).

%'?- export_parse_tree(_, _, [InputList], Tree). Viene esportato parse_tree.svg'
export_parse_tree(Sentence, Tree) :-
  sentence(_, _, Tree, Sentence, []),
  gv_export('parse_tree.svg', {Tree}/[Out0]>>export_tree_(Out0, Tree, _)).





