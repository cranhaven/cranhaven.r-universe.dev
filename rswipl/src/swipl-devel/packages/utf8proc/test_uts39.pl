/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

:- module(test_uts39, [ test_uts39/0 ]).
:- encoding(utf8).
:- use_module(library(plunit)).
:- use_module(library(unicode_security)).
:- use_module(library(lists)).

test_uts39 :-
    run_tests([ uts39_script,
                uts39_script_extensions,
                uts39_identifier_status,
                uts39_identifier_type,
                uts39_skeleton,
                uts39_confusable,
                uts39_resolved_scripts,
                uts39_restriction_level,
                uts39_check_integration
              ]).

:- begin_tests(uts39_script).

test(latin)     :- unicode_script(0x0041, latin).
test(latin_lc)  :- unicode_script(0x0061, latin).
test(cyrillic)  :- unicode_script(0x0410, cyrillic).
test(greek)     :- unicode_script(0x03A9, greek).
test(han)       :- unicode_script(0x4E2D, han).
test(hira)      :- unicode_script(0x3072, hiragana).
test(kata)      :- unicode_script(0x30AB, katakana).
test(hang)      :- unicode_script(0xAC00, hangul).
test(arab)      :- unicode_script(0x0627, arabic).
test(common)    :- unicode_script(0x0020, common).
test(common_d)  :- unicode_script(0x0030, common).
test(inherited) :- unicode_script(0x0300, inherited).
test(unassigned_fails, [fail]) :-
    unicode_script(0xE0000, _).      % no entry in Scripts.txt
test(out_of_range_fails, [fail]) :-
    unicode_script(0x110000, _).
test(negative_fails, [fail]) :-
    unicode_script(-1, _).

:- end_tests(uts39_script).

:- begin_tests(uts39_script_extensions).

test(latin_singleton, [true(X == [latin])]) :-
    unicode_script_extensions(0x0041, X).
test(middle_dot_multi) :-
    unicode_script_extensions(0x00B7, Xs),
    member(latin, Xs),
    member(greek, Xs).
test(hira_kata, [true(X == [hiragana, katakana])]) :-
    unicode_script_extensions(0x30FC, X).   % KATAKANA-HIRAGANA PROLONGED SOUND MARK
test(combining_grave) :-
    unicode_script_extensions(0x0300, Xs),
    member(latin, Xs),
    member(cyrillic, Xs),
    member(greek, Xs).
test(unassigned_fails, [fail]) :-
    unicode_script_extensions(0xE0000, _).
test(out_of_range_fails, [fail]) :-
    unicode_script_extensions(0x110000, _).

:- end_tests(uts39_script_extensions).

:- begin_tests(uts39_identifier_status).

test(latin_allowed)      :- unicode_identifier_status(0x0041, allowed).
test(digit_allowed)      :- unicode_identifier_status(0x0030, allowed).
test(underscore_allowed) :- unicode_identifier_status(0x005F, allowed).
test(control_fails, [fail]) :-
    unicode_identifier_status(0x0001, _).      % not Allowed
test(dingbat_fails, [fail]) :-
    unicode_identifier_status(0x2701, _).
test(out_of_range_fails, [fail]) :-
    unicode_identifier_status(0x110000, _).

:- end_tests(uts39_identifier_status).

:- begin_tests(uts39_identifier_type).

test(latin_recommended, [true(member(recommended, Ts))]) :-
    unicode_identifier_type(0x0041, Ts).
test(digit_recommended, [true(member(recommended, Ts))]) :-
    unicode_identifier_type(0x0030, Ts).
test(middle_dot_inclusion, [true(member(inclusion, Ts))]) :-
    unicode_identifier_type(0x00B7, Ts).
test(unassigned_fails, [fail]) :-
    unicode_identifier_type(0xE0000, _).
test(out_of_range_fails, [fail]) :-
    unicode_identifier_type(0x110000, _).

:- end_tests(uts39_identifier_type).

:- begin_tests(uts39_skeleton).

%   Identity on ASCII alphabetic input.
test(ascii_identity) :-
    unicode_skeleton("paypal", S),
    S == 'paypal'.

%   Cyrillic а (U+0430) is confusable with Latin a; skeleton collapses.
test(cyr_a_collapses, [true(Skel == 'paypal')]) :-
    unicode_skeleton("pаypal", Skel).

%   l versus ℓ (U+2113) — ℓ has confusable mapping to l.
test(script_l_collapses) :-
    unicode_skeleton("hello", S1),
    unicode_skeleton("heℓℓo", S2),
    S1 == S2.

%   Empty string is the empty skeleton.
test(empty) :-
    unicode_skeleton("", S),
    S == ''.

:- end_tests(uts39_skeleton).

:- begin_tests(uts39_confusable).

test(symmetric)        :- unicode_confusable("paypal", "pаypal").
test(symmetric_rev)    :- unicode_confusable("pаypal", "paypal").
test(reflexive)        :- unicode_confusable("hello",  "hello").
test(different_skeleton, [fail]) :-
    unicode_confusable("hello", "world").
test(latin_vs_cyrillic_a) :-
    unicode_confusable("a", "а").   % Latin a vs Cyrillic а

%   intentional.txt lists Latin A <-> Greek capital ALPHA.  With
%   ignore_intentional(true) the substitution is skipped, so they no
%   longer collapse to the same skeleton.
test(ignore_intentional) :-
    unicode_confusable("A", "Α"),                              % default: yes
    \+ unicode_confusable("A", "Α", [ignore_intentional(true)]).

:- end_tests(uts39_confusable).

:- begin_tests(uts39_resolved_scripts).

test(latin)    :- unicode_resolved_scripts("hello",  [latin]).
test(cyrillic) :- unicode_resolved_scripts("привет", [cyrillic]).
test(mixed_latin_cyrillic, [true(X == [])]) :-
    unicode_resolved_scripts("pаypal", X).
test(latin_plus_common) :- unicode_resolved_scripts("abc123", [latin]).
test(only_common, [true(X == [])]) :-
    unicode_resolved_scripts("123",   X).   % no script-bearing chars

%   Han + Hiragana resolves to {Japanese} via the §5.1 augmentation.
test(han_plus_hira_is_japanese, [true(member(japanese, X))]) :-
    unicode_resolved_scripts("漢字ひらがな", X).

:- end_tests(uts39_resolved_scripts).

:- begin_tests(uts39_restriction_level).

test(ascii_only)            :- unicode_restriction_level("hello",  ascii_only).
test(single_script_latin)   :- unicode_restriction_level("café",   single_script).
test(single_script_han)     :- unicode_restriction_level("中文",    single_script).
test(highly_latin_han)      :- unicode_restriction_level("hello中", highly_restrictive).
test(highly_latin_jpan)     :- unicode_restriction_level("abcひらがな",
                                                          highly_restrictive).
test(highly_latin_kore)     :- unicode_restriction_level("abc한글", highly_restrictive).
test(mod_latin_cyrillic)    :- unicode_restriction_level("pаypal",
                                                          moderately_restrictive).
test(mod_latin_greek)       :- unicode_restriction_level("helloΩ",
                                                          moderately_restrictive).
test(minimally_three)       :- unicode_restriction_level("中aЯ",
                                                          minimally_restrictive).
test(unrestricted_control)  :- unicode_restriction_level("a",
                                                          unrestricted).
test(empty_is_ascii_only)   :- unicode_restriction_level("", ascii_only).

:- end_tests(uts39_restriction_level).

:- use_module(library(check), [list_confusable_identifiers/0]).
:- use_module(confusable_demo).      % loaded at test-file load time

:- dynamic captured_check_term/1.

%   Capture `print_message(warning, check(...))` terms emitted by Goal,
%   returning the sorted list of distinct functors.  Both the dynamic
%   store and this helper live in the test_uts39 module so the
%   asserted clause's body references the same predicate as the
%   findall/3 below it.
capture_check_msgs(Goal, Functors) :-
    retractall(captured_check_term(_)),
    asserta((user:message_hook(check(Term), warning, _) :-
                 assertz(test_uts39:captured_check_term(Term))),
            Ref),
    setup_call_cleanup(true, ignore(Goal), erase(Ref)),
    findall(F,
            ( captured_check_term(T), functor(T, F, _) ),
            Functors0),
    retractall(captured_check_term(_)),
    sort(Functors0, Functors).

:- begin_tests(uts39_check_integration).

test(linter_reports_collision_and_mixed_script) :-
    test_uts39:capture_check_msgs(list_confusable_identifiers, Functors),
    memberchk(mixed_script_identifier,        Functors),
    memberchk(confusable_identifier_collision, Functors).

:- end_tests(uts39_check_integration).
