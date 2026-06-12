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

:- module(test_uniname, [ test_uniname/0 ]).
:- encoding(utf8).
:- use_module(library(plunit)).
:- use_module(library(uniname)).
:- use_module(library(lists)).
:- use_module(library(aggregate)).

test_uniname :-
    run_tests([ uniname_forward,
                uniname_reverse,
                uniname_check,
                uniname_enumerate
              ]).

:- begin_tests(uniname_forward).

test(ascii,        N == 'LATIN CAPITAL LETTER A') :-
    unicode_name(0'A, N).
test(latin1,       N == 'LATIN SMALL LETTER E WITH ACUTE') :-
    unicode_name(0xE9, N).
test(symbol,       N == 'EURO SIGN') :-
    unicode_name(0x20AC, N).
test(bracket,      N == 'MATHEMATICAL LEFT ANGLE BRACKET') :-
    unicode_name(0x27E8, N).
test(cjk,          N == 'CJK UNIFIED IDEOGRAPH-4E2D') :-
    unicode_name(0x4E2D, N).
test(hangul_ga,    N == 'HANGUL SYLLABLE GA') :-
    unicode_name(0xAC00, N).
test(hangul_han,   N == 'HANGUL SYLLABLE HAN') :-
    unicode_name(0xD55C, N).
test(cjk_compat,   N == 'CJK COMPATIBILITY IDEOGRAPH-F900') :-
    unicode_name(0xF900, N).
test(egyptian,     N == 'EGYPTIAN HIEROGLYPH A001') :-
    unicode_name(0x13000, N).
test(emoji,        N == 'GRINNING FACE') :-
    unicode_name(0x1F600, N).
test(control_has_no_name,        fail) :-
    unicode_name(0x0000, _).
test(unassigned_has_no_name,     fail) :-
    unicode_name(0x10FFFF, _).
test(forward_is_semidet)  :-
    findall(N, unicode_name(0'A, N), Ns),
    Ns == ['LATIN CAPITAL LETTER A'].

:- end_tests(uniname_forward).

:- begin_tests(uniname_reverse).

test(symbol,      C == 0x20AC) :-
    unicode_name(C, 'EURO SIGN').
test(tokenised,   C == 0x41) :-
    unicode_name(C, 'LATIN CAPITAL LETTER A').
test(cjk,         C == 0x4E2D) :-
    unicode_name(C, 'CJK UNIFIED IDEOGRAPH-4E2D').
test(hangul,      C == 0xD55C) :-
    unicode_name(C, 'HANGUL SYLLABLE HAN').
test(hex_range,   C == 0xF900) :-
    unicode_name(C, 'CJK COMPATIBILITY IDEOGRAPH-F900').
test(unknown_name, fail) :-
    unicode_name(_, 'NO SUCH CHARACTER NAME').
test(reverse_is_semidet) :-
    findall(C, unicode_name(C, 'EURO SIGN'), Cs),
    Cs == [0x20AC].
test(string_accepted, C == 0x20AC) :-
    unicode_name(C, "EURO SIGN").

:- end_tests(uniname_reverse).

:- begin_tests(uniname_check).

test(true)  :- unicode_name(0x20AC, 'EURO SIGN').
test(false, fail) :- unicode_name(0x20AC, 'GRINNING FACE').

:- end_tests(uniname_check).

:- begin_tests(uniname_enumerate).

test(count_is_large) :-
    aggregate_all(count, unicode_name(_, _), N),
    N > 100_000.                       % CJK + Hangul + explicit
test(first_is_space, C-N == 0x20-'SPACE') :-
    once(unicode_name(C, N)).
test(roundtrip_sample) :-
    forall(member(CP, [0'A, 0xE9, 0x20AC, 0x27E8, 0x4E2D,
                        0xAC00, 0xF900, 0x1F600]),
           ( unicode_name(CP, Name),
             unicode_name(Back, Name),
             Back =:= CP )).
test(enumeration_contains_known) :-
    once(( unicode_name(C, 'EURO SIGN'), C =:= 0x20AC )).

:- end_tests(uniname_enumerate).
