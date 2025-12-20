/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/
:- encoding(utf8).
:- module(test_yaml,
          [ test_yaml/0
          ]).

:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '../clib')).
:- asserta(user:file_search_path(library, '..')).
:- asserta(user:file_search_path(library, '../plunit')).
:- asserta(user:file_search_path(foreign, '../clib')).
:- asserta(user:file_search_path(foreign, '.')).

:- use_module(library(plunit)).
:- use_module(library(pprint)).
:- use_module(library(filesex)).
:- use_module(library(yaml)).

test_yaml :-
    run_tests([yaml]).

parse(Base, DOM) :-
    source_file(test_yaml, TestFile),
    file_directory_name(TestFile, YAML),
    directory_file_path(YAML, examples, Ex),
    file_name_extension(Base, yaml, Yaml),
    directory_file_path(Ex, Yaml, File),
    yaml_read(File, DOM).

yaml_test(Base) :-
    parse(Base, DOM),
    (   correct(Base, DOM2)
    ->  assertion(DOM =@= DOM2)
    ;   format(user_error, '~N~w~n', [Base]),
        print_term(DOM, [output(user_error)]),
        format(user_error, '~N', [])
    ).

trip(PrologIn, YAML, PrologOut) :-
    with_output_to(
        string(YAML),
        yaml_write(current_output, PrologIn)),
    yaml_read(string(YAML), PrologOut).


:- begin_tests(yaml).

test(anchors)           :- yaml_test(anchors).
test(array)             :- yaml_test(array).
test('global-tag')      :- yaml_test('global-tag').
test(json)              :- yaml_test(json).
test(mapping)           :- yaml_test(mapping).
test(numbers)           :- yaml_test(numbers).
test(strings)           :- yaml_test(strings).
test(tags)              :- yaml_test(tags).
test('yaml-version')    :- yaml_test('yaml-version').
test('cyclic-mapping')  :- yaml_test('cyclic-mapping').
test('cyclic-sequence') :- yaml_test('cyclic-sequence').

test('forward-anchors', error(existence_error(anchor, base))) :-
    yaml_test('forward-anchors').

data(0).
data(1).
data(-1).
data(X) :- X is 1<<100.
data(1.2).
data(-1.2).
data(1.2e10).
data(X) :- X is nan.
data(X) :- X is inf.
data(X) :- X is -inf.
data([1,2,3]).
data(null).
data(false).
data(true).
data("0").
data("1").
data("-1").
data("true").
data(yaml{x:25}).
data(X) :- numlist(1040, 1100, L), string_codes(X, L).
data("hello\nworld").

test(trip, [forall(data(PrologIn)), PrologOut =@= PrologIn]) :-
    trip(PrologIn, YAML, PrologOut),
    (   PrologIn =@= PrologOut
    ->  true
    ;   format('~NYAML:~n~s~n', [YAML])
    ).
test(trip_null, error(domain_error(string_without_nul, _))) :-
    trip("hello\u0000world", _YAML, _PrologOut).

term_expansion(must_read(YAML, Term), Clause) :-
    Clause = (test(Name, Reply == Term) :-
		  yaml_read(string(YAML), Reply)),
    atom_string(Name, YAML).

must_read("1", 1).
must_read("-0", 0).
must_read("-0.0", -0.0).
must_read("0.0", 0.0).
must_read("0.", "0.").
must_read("1.0e3", 1000.0).
must_read("-2E+05", -200000.0).

:- end_tests(yaml).

correct(anchors,
        yaml{ bar:yaml{<< :yaml{name:"Everyone has same name"},age:20},
              base:yaml{name:"Everyone has same name"},
              foo:yaml{<< :yaml{name:"Everyone has same name"},age:10}
            }).
correct(array,
        ["member","member2"]).
correct('global-tag',
        tag('tag:clarkevans.com,2002:shape',
            [ tag('tag:clarkevans.com,2002:circle',
                  yaml{center:yaml{x:73,y:129},radius:7}),
              tag('tag:clarkevans.com,2002:line',
                  yaml{finish:yaml{x:89,y:102},start:yaml{x:73,y:129}}),
              tag('tag:clarkevans.com,2002:label',
                  yaml{ color:16772795,
                        start:yaml{x:73,y:129},
                        text:"Pretty vector drawing."
                      })
            ])).
correct(json,
        yaml{key:["value",3]}).
correct(mapping,
        yaml{key:"value",'other-key':"other-value"}).
correct(numbers,
        [100,12.5,-130,1300000000.0]).
correct(strings,
        yaml{ folded:"This entire block of text will be the value of \c
                      'folded', but this time, all newlines will be \c
                      replaced with a single space.\n",
              'literal-block':"This entire block of text will be the \c
                       value of the 'literal-block' key,\nwith line \c
                       breaks being preserved.\n",
              unqouted:"string"
            }).
correct(tags,
        yaml{ explicit_string:"0.5",
              gif_file:"GIF89a\f\000\\f\000\\204\\000\\000\ÿÿ÷õõîééåfff\000\\000\\000\ççç^^^óóí\216\\216\\216\ààà\237\\237\\237\\223\\223\\223\§§§\236\\236\\236\iiiccc£££\204\\204\\204\ÿþùÿþùÿþùÿþùÿþùÿþùÿþùÿþùÿþùÿþùÿþùÿþùÿþùÿþù!þ\016\Made with GIMP\000\,\000\\000\\000\\000\\f\000\\f\000\\000\\005\,  \216\\201\0\236\ã@\024\èi\020\ÄÑ\212\\b\034\Ï\200\M$zïÿ0\205\p¸°1f\r\033\Î\001\Ã\001\\036\\020\' \202\\n\001\\000\;",
              python_tag:tag('tag:yaml.org,2002:python/complex',"1.0+2.0j")
            }).
correct('yaml-version',
        [1, 2, 3]).
correct('cyclic-mapping',
        yaml{a:yaml{children:[B],name:"Ay",parents:[]},b:B}) :-
    B = yaml{ children:[],
              name:"Bee",
              parents:[yaml{children:[B],name:"Ay",parents:[]}]
            }.
correct('cyclic-sequence',
        [S1]) :-
    S1 = [1,2,S1].
