/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
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

:- module(test_utf8proc, [ test_utf8proc/0 ]).
:- encoding(utf8).
:- use_module(library(plunit)).
:- use_module(library(unicode)).
:- use_module(library(aggregate)).
:- use_module(library(lists)).

test_utf8proc :-
    run_tests([ utf8proc_normalise,
                utf8proc_property,
                utf8proc_map,
                utf8proc_graphemes,
                utf8proc_version,
                utf8proc_codepoint_valid,
                utf8proc_casefold,
                utf8proc_unicode_atoms
              ]).

%   Optional-feature probes.  Older distro libutf8proc (Ubuntu LTS)
%   ships a version without charwidth, bidi_mirrored,
%   ambiguous_width, indic_conjunct_break, the
%   EXTENDED_PICTOGRAPHIC boundclass, and single-codepoint case
%   mappings.  Tests that need those features are guarded with
%   `condition(has_property(P))` so they are skipped instead of
%   reported as failures when the binding was compiled without the
%   feature.  We detect absence via the `domain_error` that
%   unicode_property/2 raises on an unknown property name — the
%   property may still legitimately fail (e.g. `uppercase(_)` for
%   `'A'`) when present, and that is treated as "supported".

has_property(Prop) :-
    catch( ( unicode_property(0'a, Prop), true ; true ),
           error(domain_error(unicode_property, _), _),
           (!, fail) ).

/*******************************
 *        NORMALISATION         *
 *******************************/

:- begin_tests(utf8proc_normalise).

% "café" precomposed vs decomposed (NFC vs NFD).
test(nfd_precomposed) :-
    unicode_nfd('café', D),
    atom_codes(D, [0'c, 0'a, 0'f, 0'e, 0x0301]).
test(nfc_composed) :-
    atom_codes(A, [0'c, 0'a, 0'f, 0'e, 0x0301]),
    unicode_nfc(A, C),
    C == 'café'.
test(nfd_nfc_roundtrip) :-
    unicode_nfd('café', D),
    unicode_nfc(D, C),
    C == 'café'.

% "Å" (U+00C5) ↔ "A" + U+030A.
test(nfd_a_ring) :-
    unicode_nfd('\u00C5', D),
    atom_codes(D, [0'A, 0x030A]).

% "Å" (U+212B ANGSTROM SIGN) → "Å" (U+00C5) under NFC.
test(nfc_angstrom) :-
    atom_codes(A, [0x212B]),
    unicode_nfc(A, C),
    atom_codes(C, [0x00C5]).

% NFKC expands the "ffi" ligature.
test(nfkc_ligature) :-
    unicode_nfkc('ﬃ', X),
    X == 'ffi'.

% NFKD expands the "ffi" ligature too.
test(nfkd_ligature) :-
    unicode_nfkd('ﬃ', X),
    X == 'ffi'.

% NFC leaves the ligature alone (canonical equivalence only).
test(nfc_ligature_preserved) :-
    unicode_nfc('ﬃ', X),
    X == 'ﬃ'.

:- end_tests(utf8proc_normalise).


/*******************************
 *  unicode_atoms POLICY        *
 *******************************/

% Loading library(unicode) registers utf8proc as the kernel's
% Unicode normalisation hook.  These tests exercise the
% multi-valued unicode_atoms policy (Prolog flag, stream property,
% read_term option) and the writeq combining-mark force-quoting
% behaviour.

:- begin_tests(utf8proc_unicode_atoms).

test(read_term_unicode_atoms_nfc) :-
    atom_codes(NFD,         [0'c, 0'a, 0'f, 0'e, 0x0301]),
    atom_codes(Precomposed, [0'c, 0'a, 0'f, 0xe9]),
    read_term_from_atom(NFD,         T1, [unicode_atoms(nfc)]),
    read_term_from_atom(Precomposed, T2, [unicode_atoms(nfc)]),
    T1 == T2.

test(read_term_accept_keeps_distinct) :-
    atom_codes(NFD,         [0'c, 0'a, 0'f, 0'e, 0x0301]),
    atom_codes(Precomposed, [0'c, 0'a, 0'f, 0xe9]),
    read_term_from_atom(NFD,         T1, []),
    read_term_from_atom(Precomposed, T2, []),
    T1 \== T2.

test(read_term_nfc_does_not_touch_quoted) :-
    atom_codes(QuotedNFD, [0'', 0'c, 0'a, 0'f, 0'e, 0x0301, 0'']),
    atom_codes(NFD,       [0'c, 0'a, 0'f, 0'e, 0x0301]),
    read_term_from_atom(QuotedNFD, T, [unicode_atoms(nfc)]),
    atom_codes(T, NFDCodes),
    atom_codes(NFD, NFDCodes).

test(read_term_error_rejects_nfd, [error(syntax_error(non_nfc_atom))]) :-
    atom_codes(NFD, [0'c, 0'a, 0'f, 0'e, 0x0301]),
    read_term_from_atom(NFD, _, [unicode_atoms(error)]).

test(read_term_error_accepts_nfc) :-
    atom_codes(NFC, [0'c, 0'a, 0'f, 0xe9]),
    read_term_from_atom(NFC, T, [unicode_atoms(error)]),
    atom(T).

test(read_term_reject_rejects_unicode, [error(syntax_error(non_ascii_atom))]) :-
    atom_codes(A, [0'c, 0'a, 0'f, 0xe9]),
    read_term_from_atom(A, _, [unicode_atoms(reject)]).

test(read_term_reject_passes_quoted) :-
    atom_codes(A, [0'', 0'c, 0'a, 0'f, 0xe9, 0'']),
    read_term_from_atom(A, T, [unicode_atoms(reject)]),
    atom_codes(T, [0'c, 0'a, 0'f, 0xe9]).

test(flag_default_nfc) :-
    setup_call_cleanup(
        ( current_prolog_flag(unicode_atoms, Old),
          set_prolog_flag(unicode_atoms, nfc) ),
        ( atom_codes(A1, [0'c, 0'a, 0'f, 0'e, 0x0301]),
          atom_codes(A2, [0'c, 0'a, 0'f, 0xe9]),
          read_term_from_atom(A1, T1, []),
          read_term_from_atom(A2, T2, []),
          T1 == T2 ),
        set_prolog_flag(unicode_atoms, Old)).

test(stream_property_unicode_atoms) :-
    setup_call_cleanup(
        open_string("foo", S),
        ( set_stream(S, unicode_atoms(nfc)),
          stream_property(S, unicode_atoms(nfc)) ),
        close(S)).

test(writeq_quotes_combining_atom) :-
    atom_codes(NFD, [0'c, 0'a, 0'f, 0'e, 0x0301]),
    with_output_to(string(S), writeq(NFD)),
    sub_string(S, 0, 1, _, "'"),
    sub_string(S, _, 1, 0, "'").

test(writeq_does_not_quote_nfc_atom) :-
    atom_codes(NFC, [0'c, 0'a, 0'f, 0xe9]),
    with_output_to(string(S), writeq(NFC)),
    \+ sub_string(S, 0, 1, _, "'").

% --- Trojan-source bidi rejection (always on) -------------------------

test(bidi_in_unquoted_atom_is_error,
     [error(syntax_error(bidi_override(0x202E)))]) :-
    atom_codes(A, [0'a, 0x202E, 0'b]),
    read_term_from_atom(A, _, []).

test(bidi_in_quoted_atom_is_error,
     [error(syntax_error(bidi_override(0x202E)))]) :-
    atom_codes(A, [0'', 0'a, 0x202E, 0'b, 0'']),
    read_term_from_atom(A, _, []).

test(bidi_via_escape_is_allowed) :-
    atom_codes(A, [0'', 0'a, 0'\\, 0'u, 0'2, 0'0, 0'2, 0'E, 0'b, 0'']),
    read_term_from_atom(A, T, []),
    atom_codes(T, [0'a, 0x202E, 0'b]).

:- end_tests(utf8proc_unicode_atoms).


/*******************************
 *        PROPERTIES            *
 *******************************/

:- begin_tests(utf8proc_property).

test(category_det) :-
    unicode_property(0'A, category('Lu')).
test(category_lowercase_a) :-
    unicode_property(0'a, category('Ll')).
test(category_enumerate, Cat == 'Lu') :-
    unicode_property(0'A, category(Cat)).
test(category_shorthand_letter) :-
    unicode_property(0'A, category('L')).
test(category_shorthand_number) :-
    unicode_property(0'5, category('N')).
test(category_shorthand_punct) :-
    unicode_property(0'., category('P')).
test(category_mark_nonspacing) :-
    unicode_property(0x0301, category('Mn')).          % combining acute

test(combining_class_zero) :-
    unicode_property(0'A, combining_class(0)).
test(combining_class_grave, C == 230) :-
    unicode_property(0x0300, combining_class(C)).      % above

test(bidi_class_ltr) :-
    unicode_property(0'A, bidi_class(l)).
test(bidi_class_european, C == en) :-
    unicode_property(0'5, bidi_class(C)).

test(bidi_mirrored_paren,
     [condition(has_property(bidi_mirrored(_)))]) :-
    unicode_property(0'(, bidi_mirrored(true)).
test(bidi_not_mirrored_a,
     [condition(has_property(bidi_mirrored(_)))]) :-
    unicode_property(0'A, bidi_mirrored(false)).

test(width_ascii,
     [condition(has_property(width(_))), W == 1]) :-
    unicode_property(0'A, width(W)).
test(width_combining,
     [condition(has_property(width(_))), W == 0]) :-
    unicode_property(0x0301, width(W)).
test(width_cjk,
     [condition(has_property(width(_))), W == 2]) :-
    unicode_property(0x4E2D, width(W)).                % 中
test(width_emoji,
     [condition(has_property(width(_))), W == 2]) :-
    unicode_property(0x1F929, width(W)).               % 🤩
test(width_control,
     [condition(has_property(width(_))), W == 0]) :-
    unicode_property(0x0000, width(W)).

test(case_toupper,
     [condition(has_property(uppercase(_))), U == 0'A]) :-
    unicode_property(0'a, uppercase(U)).
test(case_tolower,
     [condition(has_property(lowercase(_))), L == 0'a]) :-
    unicode_property(0'A, lowercase(L)).
test(case_titlecase_a,
     [condition(has_property(titlecase(_))), T == 0'A]) :-
    unicode_property(0'a, titlecase(T)).
test(case_no_upper_for_A,
     [condition(has_property(uppercase(_))), fail]) :-
    unicode_property(0'A, uppercase(_)).                % already upper
test(case_no_lower_for_a,
     [condition(has_property(lowercase(_))), fail]) :-
    unicode_property(0'a, lowercase(_)).                % already lower
test(case_no_mapping_for_digit,
     [condition(has_property(uppercase(_))), fail]) :-
    unicode_property(0'5, uppercase(_)).

test(decomp_type_compat, D == compat) :-
    unicode_property(0xFB03, decomp_type(D)).          % ﬃ
test(decomp_type_missing_fails, fail) :-
    unicode_property(0'A, decomp_type(_)).             % plain ASCII

test(ignorable_softhyphen) :-
    unicode_property(0x00AD, ignorable(true)).          % SOFT HYPHEN
test(not_ignorable_a) :-
    unicode_property(0'a, ignorable(false)).

test(boundclass_cr) :-
    unicode_property(0'\r, boundclass(cr)).
test(boundclass_lf) :-
    unicode_property(0'\n, boundclass(lf)).
test(boundclass_emoji,
     [condition(unicode_property(0x1F929,
                                 boundclass(extended_pictographic))),
      C == extended_pictographic]) :-
    unicode_property(0x1F929, boundclass(C)).
test(boundclass_zwj) :-
    unicode_property(0x200D, boundclass(zwj)).

test(ambiguous_width_greek,
     [condition(has_property(ambiguous_width(_)))]) :-
    unicode_property(0x03B1, ambiguous_width(true)).    % α is ambiguous
test(ambiguous_width_ascii,
     [condition(has_property(ambiguous_width(_)))]) :-
    unicode_property(0'A, ambiguous_width(false)).

test(indic_conjunct_break_none,
     [condition(has_property(indic_conjunct_break(_)))]) :-
    unicode_property(0'A, indic_conjunct_break(none)).

% Code-point argument can also be a single-char atom.
test(code_as_atom_A) :-
    unicode_property('A', category('Lu')).

% Enumerate properties for a single code point.  The minimum five
% (category, combining_class, bidi_class, ignorable, boundclass) are
% always available.
test(backtrack_properties) :-
    findall(P, unicode_property(0'A, P), Ps),
    length(Ps, N),
    N >= 5.

% Invalid range
test(out_of_range, error(domain_error(code, _))) :-
    unicode_property(0x200000, category(_)).

:- end_tests(utf8proc_property).


/*******************************
 *       STRING MAPPINGS        *
 *******************************/

:- begin_tests(utf8proc_map).

test(casefold_hello) :-
    unicode_map('Hello', X, [casefold]),
    X == 'hello'.
test(casefold_sharp_s) :-
    unicode_map('Straße', X, [casefold]),
    X == 'strasse'.

test(stripmark_cafe) :-
    unicode_map('café', X, [stable, decompose, stripmark]),
    X == 'cafe'.

test(lump_hyphen) :-
    atom_codes(In, [0'a, 0x2010, 0'b]),                % HYPHEN
    unicode_map(In, X, [stable, lump]),
    X == 'a-b'.

test(lump_rightsingle_quote) :-
    atom_codes(In, [0'a, 0x2019, 0'b]),                % ’
    unicode_map(In, X, [stable, lump]),
    X == 'a\'b'.

test(stripcc_tab_becomes_space) :-
    atom_codes(In, [0'a, 0'\t, 0'b]),
    unicode_map(In, X, [stable, stripcc]),
    X == 'a b'.

% With charbound, U+00FF is inserted at each grapheme-cluster start.
test(charbound_cafe_acute) :-
    atom_codes(In, [0'a, 0x0301, 0'b]),                % a+grave, b
    unicode_map(In, Raw, [stable, charbound]),
    atom_codes(Raw, Codes),
    % One 0xFF before each grapheme cluster.
    aggregate_all(count, member(0xFF, Codes), N),
    N == 2.

:- end_tests(utf8proc_map).


/*******************************
 *         GRAPHEMES            *
 *******************************/

:- begin_tests(utf8proc_graphemes,
               [condition(current_predicate(atom_graphemes/2))]).

test(ascii) :-
    atom_graphemes('hello', Gs),
    Gs == [h, e, l, l, o].

test(combining_joined) :-
    atom_codes(In, [0'c, 0'a, 0'f, 0'e, 0x0301]),      % "cafe" + ́
    atom_graphemes(In, Gs),
    atom_codes(G4, [0'e, 0x0301]),
    Gs == [c, a, f, G4].

test(stacked_combiners) :-
    atom_codes(In, [0'a, 0x0301, 0x0302, 0'b]),         % a + 2 combiners, b
    atom_graphemes(In, [G1, G2]),
    atom_codes(G1, [0'a, 0x0301, 0x0302]),
    G2 == b.

test(emoji) :-
    atom_graphemes('🤩', Gs),
    Gs = [G],
    atom_codes(G, [0x1F929]).

% Regional-indicator pair: 🇳 (U+1F1F3) + 🇱 (U+1F1F1) → one flag grapheme.
test(flag_grapheme) :-
    atom_codes(In, [0x1F1F3, 0x1F1F1]),
    atom_graphemes(In, [_]).

% ZWJ emoji sequence: 👨 + ZWJ + 👩 + ZWJ + 👧 → single cluster.
test(zwj_family) :-
    atom_codes(In, [0x1F468, 0x200D, 0x1F469, 0x200D, 0x1F467]),
    atom_graphemes(In, [_]).

% CRLF is a single grapheme cluster per UAX#29.
test(crlf_single) :-
    atom_codes(In, [0'\r, 0'\n]),
    atom_graphemes(In, [_]).

test(empty_atom) :-
    atom_graphemes('', Gs),
    Gs == [].

% Reverse mode
test(reverse_abc) :-
    atom_graphemes(A, [a, b, c]),
    A == abc.
test(reverse_with_combined) :-
    atom_codes(Part, [0'e, 0x0301]),
    atom_graphemes(A, [c, a, f, Part]),
    atom_codes(A, [0'c, 0'a, 0'f, 0'e, 0x0301]).
test(reverse_empty) :-
    atom_graphemes(A, []),
    A == ''.

% Both directions must agree.
test(round_trip_unicode) :-
    atom_graphemes('café🇳🇱', Gs),
    atom_graphemes(Back, Gs),
    Back == 'café🇳🇱'.

% string_graphemes mirrors atom_graphemes but with strings.
test(string_ascii) :-
    string_graphemes("hello", Gs),
    Gs == ["h", "e", "l", "l", "o"].

test(string_reverse) :-
    string_graphemes(S, ["a", "b", "c"]),
    S == "abc".

test(string_mixed_graphemes) :-
    string_codes(In, [0'c, 0'a, 0'f, 0'e, 0x0301]),
    string_graphemes(In, Gs),
    string_codes(S4, [0'e, 0x0301]),
    Gs == ["c", "a", "f", S4].

:- end_tests(utf8proc_graphemes).


/*******************************
 *     VERSION / VALIDITY       *
 *******************************/

:- begin_tests(utf8proc_version,
               [condition(current_predicate(unicode_version/1))]).

test(version_shape) :-
    unicode_version(V),
    atom(V),
    atom_length(V, L),
    L > 0,
    split_string(V, ".", "", Parts),
    length(Parts, 3),
    forall(member(P, Parts),
           (   number_string(N, P),
               integer(N),
               N >= 0
           )).

:- end_tests(utf8proc_version).


:- begin_tests(utf8proc_codepoint_valid,
               [condition(current_predicate(unicode_codepoint_valid/1))]).

test(valid_A) :-
    unicode_codepoint_valid(0'A).
test(valid_emoji) :-
    unicode_codepoint_valid(0x1F929).
test(valid_combining) :-
    unicode_codepoint_valid(0x0301).
test(invalid_negative, fail) :-
    unicode_codepoint_valid(-1).
test(invalid_too_large, fail) :-
    unicode_codepoint_valid(0x110000).
test(invalid_surrogate, fail) :-
    unicode_codepoint_valid(0xD800).
test(invalid_unassigned, fail) :-
    unicode_codepoint_valid(0x10FFFE).                  % non-character

:- end_tests(utf8proc_codepoint_valid).


/*******************************
 *         CASEFOLD             *
 *******************************/

:- begin_tests(utf8proc_casefold).

test(casefold_sharp_s) :-
    unicode_casefold('Straße', X),
    X == 'strasse'.

test(casefold_greek_sigma) :-
    atom_codes(In, [0x03A3]),                           % Σ
    unicode_casefold(In, X),
    atom_codes(X, [0x03C3]).                            % σ

test(nfkc_casefold_ligature) :-
    unicode_nfkc_casefold('ﬃ', X),                    % NFKC splits + fold
    X == 'ffi'.

test(nfkc_casefold_mixed) :-
    unicode_nfkc_casefold('Ωhm', X),
    atom_codes(X, [0x03C9, 0'h, 0'm]).                  % ω + hm

:- end_tests(utf8proc_casefold).
