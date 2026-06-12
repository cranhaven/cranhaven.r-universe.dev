/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2025, SWI-Prolog Solutions b.v.
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

:- module(test_json_schema,
          [ test_json_schema/0,         % Entry point
                                        % Incremental debugging
            test_dir/1,                 % +Dir
            test_file/1,                % +JSON test file
            test_file/2,                % +JSON test file, +Schema/Test
            remote_server/0,
            show_failed/0,
            test_json_schema/0
          ]).
:- use_module(library(json_schema)).
:- if(exists_source(library(http/http_server))).
:- use_module(library(http/http_server)).
:- use_module(library(http/http_files)).
:- endif.
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(ansi_term)).

/** <module> Run json-schema.org test suite

This  module  is  used   to    test   library(json_schema)  against  the
json-schema.org test suite. The test data   is  __not included__. To run
these tests

  - Clone https://github.com/json-schema-org/JSON-Schema-Test-Suite.git
  - Set the environment variable ``JSON_SCHEMA_TEST_SUITE`` to point at
    the root directory of the above repository.
*/

:- debug(json_test(progress)).

test_version('draft2020-12').

%!  test_json_schema
%
%   Run the entire test suite. Note that running the tests launches an
%   HTTP server for localhost:1234.

test_json_schema :-
    exists_source(library(http/http_server)),
    test_version(Version),
    absolute_file_name(json_suite(tests/Version), Dir,
                       [ file_type(directory),
                         access(read),
                         file_errors(fail)
                       ]),
    !,
    test_dir(Dir).
test_json_schema :-
    ansi_format(warning, "Cannot find JSON Schema test data; skipping tests", []).

%!  blocked(+SchemaAndTest)
%
%   Define Schema/Test that is blocked.

:- dynamic blocked/1.

blocked("schema that uses custom metaschema with with no validation vocabulary"/_).
blocked("$dynamicRef skips over intermediate resources - direct reference"/_).
blocked("A $dynamicRef that initially resolves to a schema with a matching $dynamicAnchor resolves to the first $dynamicAnchor in the dynamic scope"/_).

:- multifile user:file_search_path/1.

user:file_search_path(json_suite, Dir) :-
    getenv('JSON_SCHEMA_TEST_SUITE', Dir),
    exists_directory(Dir),
    !.
user:file_search_path(json_suite, 'test/JSON-Schema-Test-Suite').


:- dynamic
    test_file_result/2.                 % ?File, ?Status

show_failed :-
    forall(test_file_result(File, failed(Passed, Failed)),
           format('~p: passed ~D, failed ~D~n', [File, Passed, Failed])).

%!  test_dir(+Dir) is semidet.
%
%   Run test_file/1 on all ``*.json`` files in Dir.

test_dir(Dir) :-
    remote_server,
    set_flag(passed, 0),
    set_flag(failed, 0),
    retractall(test_file_result(_,_)),
    directory_file_path(Dir, '*.json', Pattern),
    expand_file_name(Pattern, Files),
    maplist(test, Files),
    summary.

summary :-
    get_flag(passed, Passed),
    get_flag(failed, Failed),
    Total is Passed+Failed,
    (   Failed =:= 0
    ->  print_message(informational, test_summary(Total, Passed, Failed))
    ;   print_message(warning, test_summary(Total, Passed, Failed))
    ),
    Failed =:= 0.

%!  test_file(+File) is semidet.
%!  test_file(+File, +Limit) is semidet.
%
%   Run the tests from the JSON test specification File. Limit is either
%   a variable (run all tests) or a  term `Schema/Test`, where either is
%   either unbound or a string that should match the `description` field
%   of the schema or test.

test_file(File) :-
    test_file(File, _).

test_file(File, Limit) :-
    remote_server,
    set_flag(passed, 0),
    set_flag(failed, 0),
    test(File, Limit),
    summary.

test(File) :-
    test(File, _).

test(File, Limit) :-
    get_flag(failed, Failed0),
    get_flag(passed, Passed0),
    limit(Limit, SchemeName, TestName),
    json_schema:json_read_file(File, Tests),
    forall(member(Test, Tests),
           test_schema(File, Test, SchemeName, TestName)),
    get_flag(failed, Failed1),
    get_flag(passed, Passed1),
    Failed is Failed1 - Failed0,
    Passed is Passed1 - Passed0,
    (   Failed =:= 0
    ->  assertz(test_file_result(File, passed(Passed))),
        print_message(informational, test_file(passed(Passed), File))
    ;   assertz(test_file_result(File, failed(Passed, Failed))),
        print_message(warning, test_file(failed(Passed, Failed), File))
    ).

test_schema(_File, TestSet, _, _) :-
    blocked(TestSet.description/Test),
    var(Test),
    !.
test_schema(File, TestSet, SchemeName, TestName) :-
    matches_name(SchemeName, TestSet.description),
    testset_has_test(TestSet, TestName),
    !,
    debug(json_test(progress), 'Schema ~p', [TestSet.description]),
    (   catch(json_compile_schema(TestSet.schema, Type, []), E, true)
    ->  (   var(E)
        ->  forall(member(Test, TestSet.tests),
                   test_data(TestSet, File, Type, Test, TestName))
        ;   print_message(warning,
                          parse_schema_error(E, File, TestSet.description))
        )
    ;   print_message(warning,
                      parse_schema_failed(File, TestSet.description))
    ).
test_schema(_File, _TestSet, _SchemeName, _TestName).

test_data(TestSet, _File, _Type, Test, _TestName) :-
    blocked(TestSet.description/Test.description),
    !.
test_data(_TestSet, File, Type, Test, TestName) :-
    matches_name(TestName, Test.description),
    !,
    debug(json_test(progress), '  Test ~p ...', [Test.description]),
    (   catch(json_check(Type, Test.data, []), E, true)
    ->  (   var(E)
        ->  (   Test.valid == true
            ->  print_message(informational, valid_passed(File, Test.description))
            ;   Test.valid == false
            ->  print_message(warning, invalid_succeeded(File, Test.description))
            )
        ;   nonvar(E)
        ->  (   Test.valid == true
            ->  print_message(warning, valid_error(File, Test.description, E))
            ;   print_message(informational, invalid_passed(File, Test.description, E))
            )
        )
    ;   print_message(warning, failed(File, Test.description))
    ).
test_data(_TestSet, _File, _Type, _Test, _TestName).


limit(Var, _SchemeName, _TestName), var(Var) => true.
limit(Schema0/Test0, Schema, Test) =>
    Schema = Schema0,
    Test = Test0.

matches_name(Spec, Spec) => true.
matches_name(Spec, _Name), var(Spec) => true.
matches_name(_, _) => fail.

testset_has_test(_TestSet, TestName), var(TestName) =>
    true.
testset_has_test(TestSet, TestName) =>
    member(Test, TestSet.tests),
    matches_name(TestName, Test.description),
    !.

		 /*******************************
		 *             REMOTE		*
		 *******************************/

:- if(exists_source(library(http/http_server))).
:- http_handler(root(.),
                http_reply_from_files(json_suite(remotes), []),
                [ prefix ]).

%!  remote_server
%
%   Launch an HTTP server that serves the external documents used in the
%   tests.

remote_server :-
    http_current_server(http_server:http_dispatch, 1234),
    !.
remote_server :-
    http_server([port(localhost:1234)]).

:- else.

remote_server.

:- endif.

		 /*******************************
		 *            MESSAGES		*
		 *******************************/

:- multifile
    prolog:message//1.

prolog:message(valid_passed(_File, Description)) -->
    { flag(passed, N, N+1) },
    [ 'Valid test "~s" passed'-[Description] ].
prolog:message(invalid_succeeded(_File, Description)) -->
    { flag(failed, N, N+1) },
    [ 'Invalid test "~s" passed'-[Description] ].
prolog:message(valid_error(File, Description, E)) -->
    { flag(failed, N, N+1),
      message_to_string(E, Msg)
    },
    [ '~w: Valid test "~s" failed: ~s'-[File, Description, Msg] ].
prolog:message(invalid_passed(File, Description, E)) -->
    { flag(passed, N, N+1),
      message_to_string(E, Msg)
    },
    [ '~w: Invalid test "~s" failed:~s'-[File, Description, Msg] ].
prolog:message(failed(File, Description)) -->
    { flag(failed, N, N+1) },
    [ '~w: Test failed without exception: "~s"'-[File, Description] ].
prolog:message(test_file(passed(Passed), File)) -->
    [ 'All ~D tests from ~q passed'-[Passed, File] ].
prolog:message(test_file(failed(Passed, Failed), File)) -->
    [ '~D tests from ~q failed ~D passed'-[Failed, File, Passed] ].
prolog:message(parse_schema_error(Error, File, Description)) -->
    { flag(failed, N, N+1) },
    { message_to_string(Error, Msg) },
    [ '~w: could not parse schema for "~s": ~p'-[File, Description, Msg] ].
prolog:message(parse_schema_failed(File, Description)) -->
    { flag(failed, N, N+1) },
    [ '~w: could not parse schema for "~s": failed'-[File, Description] ].
prolog:message(test_summary(Total, Passed, Failed)) -->
    [ '~D tests.  ~D passed, ~D failed'-[Total, Passed, Failed] ].

