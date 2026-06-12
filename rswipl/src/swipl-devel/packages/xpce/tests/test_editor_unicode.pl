/*  Part of XPCE --- The SWI-Prolog GUI toolkit

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

:- module(test_editor_unicode, [test_editor_unicode/0]).
:- encoding(utf8).

/** <module> Tests for Unicode visual-column tracking in xpce editor

Validates that `Editor <-visual_column` returns correct visual column
counts for ASCII, CJK double-width characters, and combining marks.

Run with:

    swipl -g test_editor_unicode -t halt \
          packages/xpce/tests/test_editor_unicode.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

setup_headless :-
    set_prolog_flag('SDL_VIDEODRIVER', dummy).
:- initialization(setup_headless, now).

test_editor_unicode :-
    run_tests(editor_unicode).

make_editor(E) :-
    new(E, editor(new(_, text_buffer), 10, 40)).

type_chars(_, []).
type_chars(E, [H|T]) :-
    send(E, typed, H),
    type_chars(E, T).

:- begin_tests(editor_unicode).

test(ascii_column) :-                   % baseline: 3 ASCII chars → col 3
    make_editor(E),
    type_chars(E, [0'a, 0'b, 0'c]),
    get(E, visual_column, 3).

test(cjk_column) :-                     % each CJK char occupies 2 visual cols
    make_editor(E),
    type_chars(E, [0x4E2D, 0x6587]),    % 中文
    get(E, visual_column, 4).

test(combining_column) :-               % combining accent = 0 extra cols
    make_editor(E),
    type_chars(E, [0'e, 0x0301]),       % e + combining acute = é
    get(E, visual_column, 1).

test(mixed_column) :-                   % ASCII + CJK + combining
    make_editor(E),
    type_chars(E, [0'a,              % col 1
                   0x4E2D,           % cols 2–3  (中)
                   0'e, 0x0301]),    % col 4     (é, combining doesn't advance)
    get(E, visual_column, 4).

test(updown_goal_column) :-             % up/down preserves visual column
    make_editor(E),
    %% Line 0: 中 (cols 0–1), then newline
    type_chars(E, [0x4E2D, 0'\n]),
    %% Line 1: a (col 0), then move up
    type_chars(E, [0'a]),
    %% goal col after 'a' is 1; move up — 中 spans cols 0–1, caret lands at col 0
    send(E, cursor_up),
    get(E, visual_column, Col),
    ( Col =:= 0 -> true
    ; format("Expected visual_column 0, got ~w~n", [Col]), fail
    ).

:- end_tests(editor_unicode).
