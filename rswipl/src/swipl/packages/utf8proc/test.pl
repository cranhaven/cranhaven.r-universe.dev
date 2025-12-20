:- asserta(user:file_search_path(foreign, '.')).
:- use_module(unicode).
:- use_module(library(apply_macros)).

ptest1(N) :-
    forall(between(1, N, _),
           unicode_nfc(hello, _)).

ptest2(N) :-
    forall(between(1, N, _),
           downcase_atom(hello, _)).
