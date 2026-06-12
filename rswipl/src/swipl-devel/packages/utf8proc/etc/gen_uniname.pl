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

:- module(gen_uniname, [main/1]).
:- use_module(library(main)).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(readutil)).
:- use_module(library(filesex)).
:- use_module(library(pairs)).
:- use_module(library(aggregate)).

:- initialization(main, main).

/** <module> Generate a compact Unicode code-point -> name table

Reads `UnicodeData.txt` and emits `uniname_data.c` and `uniname_data.h`.
The representation follows the classic ICU / GNU libunistring scheme:

  1. *Algorithmic ranges* carry no per-code-point data.  Hangul
     syllables are synthesised from their Jamo decomposition; CJK
     unified ideographs, Tangut ideographs and every explicit
     `PREFIX-<cphex>` family (Egyptian Hieroglyph, CJK Compatibility
     Ideograph, Khitan, Nushu, ...) are `PREFIX` + the uppercase hex
     of the code point.

  2. The remaining (~34k) names are split into space-separated
     *words*; a shared word table is built and each name is stored as
     a sequence of 16-bit word ids.  Unicode names are highly
     repetitive ("LATIN", "LETTER", "SMALL", "WITH", ...), so this is
     where the bulk of the compression comes from.

Usage (from packages/utf8proc/):

    swipl etc/gen_uniname.pl -- --ucd-dir data --out . --version 17.0.0

The output is deterministic so re-running on unchanged inputs produces
no diff.
*/

main(Argv) :-
    parse_args(Argv, Opts),
    option(ucd_dir(UCD), Opts),
    option(out(Out), Opts),
    option(version(V), Opts),
    directory_file_path(UCD, 'UnicodeData.txt', Path),
    load_unicode_data(Path),
    build_model,
    emit(Out, V),
    report.

parse_args(Argv, Opts) :-
    parse_args_(Argv, Opts0),
    defaults(Opts0, Opts).

parse_args_([], []).
parse_args_(['--ucd-dir', D | T], [ucd_dir(D) | R]) :- !, parse_args_(T, R).
parse_args_(['--out', O | T],     [out(O) | R])     :- !, parse_args_(T, R).
parse_args_(['--version', V | T], [version(V) | R]) :- !, parse_args_(T, R).
parse_args_([A | _], _) :- domain_error(option, A).

defaults(In, Out) :-
    (   option(ucd_dir(_), In) -> In1 = In ; In1 = [ucd_dir(data)|In] ),
    (   option(out(_), In1)    -> In2 = In1 ; In2 = [out('.')|In1] ),
    (   option(version(_), In2) -> Out = In2 ; Out = [version('17.0.0')|In2] ).

% ----------------------------------------------------------------------
%  Read UnicodeData.txt
% ----------------------------------------------------------------------

:- dynamic
    explicit/2,                 % Cp - NameAtom  (explicit, has a name)
    range_marker/3,             % StartCp - EndCp - DescriptionString
    nrows/1.

load_unicode_data(Path) :-
    retractall(explicit(_,_)),
    retractall(range_marker(_,_,_)),
    setup_call_cleanup(
        open(Path, read, In, [encoding(utf8)]),
        read_udata(In),
        close(In)).

read_udata(In) :-
    read_line_to_string(In, Line),
    (   Line == end_of_file
    ->  true
    ;   split_string(Line, ";", "", [CodeS, NameS | _]),
        hex_atom_number(CodeS, Cp),
        (   string_concat(Pre, ", First>", NameS),
            string_concat("<", Desc, Pre)
        ->  read_udata_last(In, Cp, Desc)
        ;   sub_string(NameS, 0, 1, _, "<")
        ->  read_udata(In)             % <control>, surrogates, etc.: no name
        ;   atom_string(Name, NameS),
            assertz(explicit(Cp, Name)),
            read_udata(In)
        )
    ).

read_udata_last(In, StartCp, Desc) :-
    read_line_to_string(In, Line),
    split_string(Line, ";", "", [CodeS|_]),
    hex_atom_number(CodeS, EndCp),
    assertz(range_marker(StartCp, EndCp, Desc)),
    read_udata(In).

hex_atom_number(S, N) :-
    string_concat("0x", S, S2),
    term_string(N, S2).

% ----------------------------------------------------------------------
%  Build the model: algorithmic ranges + tokenised names
% ----------------------------------------------------------------------

:- dynamic
    range/4,                    % Start - End - Kind - PrefixAtom
    word_id/2,                  % Word - Id     (id assigned by frequency)
    named/2.                    % Cp - list(Word)

build_model :-
    retractall(range(_,_,_,_)),
    retractall(word_id(_,_)),
    retractall(named(_,_)),
    forall(range_marker(S,E,Desc), classify_marker(S,E,Desc)),
    findall(Cp-Name, explicit(Cp,Name), Pairs0),
    sort(Pairs0, Pairs),
    foldl(model_explicit, Pairs, [], RevHexRuns),
    flush_hex_runs(RevHexRuns),
    assign_word_ids.

%   Assign word ids by descending frequency so the most common words
%   ("LETTER", "WITH", "SMALL", ...) get the lowest ids and hence the
%   shortest varint encodings.  Range prefixes are interned too.
assign_word_ids :-
    findall(W, (named(_,Ws), member(W,Ws)), Occ0),
    findall(P, range(_,_,_,P), Prefixes),
    append(Occ0, Prefixes, Occ),
    msort(Occ, Sorted),
    freq_pairs(Sorted, Freqs),          % Word-Count
    sort(2, @>=, Freqs, ByFreq),
    forall(nth0(Id, ByFreq, W-_), assertz(word_id(W, Id))).

freq_pairs([], []).
freq_pairs([W|T], [W-N|R]) :-
    count_run(T, W, 1, N, Rest),
    freq_pairs(Rest, R).

count_run([W|T], W, N0, N, Rest) :- !, N1 is N0+1, count_run(T, W, N1, N, Rest).
count_run(Rest, _, N, N, Rest).

%   Range markers: CJK / Tangut ideographs -> HEX rule; Hangul -> HANGUL.
classify_marker(S, E, Desc) :-
    (   sub_string(Desc, _, _, _, "CJK Ideograph")
    ->  assertz(range(S,E,hex,'CJK UNIFIED IDEOGRAPH'))
    ;   sub_string(Desc, _, _, _, "Tangut Ideograph")
    ->  assertz(range(S,E,hex,'TANGUT IDEOGRAPH'))
    ;   sub_string(Desc, _, _, _, "Hangul Syllable")
    ->  assertz(range(S,E,hangul,'HANGUL SYLLABLE'))
    ;   true                    % surrogates / private use: no name
    ).

%   Explicit names: detect the generic `PREFIX-<cphex>` rule and fold
%   maximal contiguous runs that share PREFIX into hex ranges; tokenise
%   everything else.
model_explicit(Cp-Name, Run0, Run) :-
    (   hex_suffix(Cp, Name, Prefix)
    ->  add_hex(Cp, Prefix, Run0, Run)
    ;   flush_hex_runs(Run0),
        Run = [],
        tokenise(Cp, Name)
    ).

add_hex(Cp, Prefix, [run(Prefix,S,E)|T], Run) :-
    Cp =:= E+1, !,
    Run = [run(Prefix,S,Cp)|T].
add_hex(Cp, Prefix, Run0, [run(Prefix,Cp,Cp)|Run0]) :-
    flush_hex_runs(Run0).

flush_hex_runs(Runs) :-
    forall(member(run(Prefix,S,E), Runs),
           assertz(range(S,E,hex,Prefix))).

hex_suffix(Cp, Name, Prefix) :-
    format(atom(Canon), "~|~`0t~16R~4+", [Cp]),  % uppercase, >=4 digits
    atom_concat('-', Canon, Suffix),
    atom_concat(Prefix, Suffix, Name),
    Prefix \== ''.

tokenise(Cp, Name) :-
    split_string(Name, " ", "", Parts),
    maplist(atom_string, Words, Parts),
    assertz(named(Cp, Words)).

% ----------------------------------------------------------------------
%  Emit C
% ----------------------------------------------------------------------

emit(Dir, V) :-
    directory_file_path(Dir, 'uniname_data.h', HPath),
    directory_file_path(Dir, 'uniname_data.c', CPath),
    setup_call_cleanup(open(HPath, write, H, [encoding(utf8)]),
                       emit_header(H, V), close(H)),
    setup_call_cleanup(open(CPath, write, C, [encoding(utf8)]),
                       emit_source(C, V), close(C)).

file_header(Stream, Subject, V) :-
    format(Stream, "/* ~w~n", [Subject]),
    format(Stream, " *~n", []),
    format(Stream, " * GENERATED FILE -- DO NOT EDIT.~n", []),
    format(Stream, " * Regenerate with `ninja regen-uniname` in the build dir.~n", []),
    format(Stream, " *~n", []),
    format(Stream, " * Source: UnicodeData.txt, Unicode ~w.~n", [V]),
    format(Stream, " * Input file vendored under packages/utf8proc/data/.~n", []),
    format(Stream, " */~n~n", []).

emit_header(H, V) :-
    file_header(H, "Declarations for the compact Unicode name table.", V),
    word_count(NW), range_count(NR), named_count(NN),
    prefixes(Prefixes), length(Prefixes, NP),
    names_blob(Blob), length(Blob, BL),
    words_blob_len(WL),
    format(H, "#ifndef UNINAME_DATA_H_INCLUDED~n", []),
    format(H, "#define UNINAME_DATA_H_INCLUDED~n~n", []),
    format(H, "#include <stdint.h>~n#include <stddef.h>~n~n", []),
    format(H, "#define UNINAME_UNICODE_VERSION \"~w\"~n", [V]),
    format(H, "#define UNINAME_WORD_COUNT   ~d~n", [NW]),
    format(H, "#define UNINAME_RANGE_COUNT  ~d~n", [NR]),
    format(H, "#define UNINAME_PREFIX_COUNT ~d~n", [NP]),
    format(H, "#define UNINAME_NAMED_COUNT  ~d~n", [NN]),
    format(H, "#define UNINAME_WORDS_LEN    ~d~n", [WL]),
    format(H, "#define UNINAME_NAMES_LEN    ~d~n~n", [BL]),
    format(H, "#define UNINAME_K_HEX    0  /* PREFIX + '-' + upperhex(cp) */~n", []),
    format(H, "#define UNINAME_K_HANGUL 1  /* HANGUL SYLLABLE <jamo>      */~n~n", []),
    format(H, "/* uniname_names is a flat byte stream.  For each explicit name,~n", []),
    format(H, " * in ascending code-point order: an unsigned-LEB128 code-point~n", []),
    format(H, " * delta, then one unsigned-LEB128 (word-id+1) per word, then a~n", []),
    format(H, " * 0x00 terminator.  uniname_words is the word table, NUL-~n", []),
    format(H, " * separated in ascending id (descending frequency) order.~n", []),
    format(H, " */~n~n", []),
    format(H, "typedef struct {~n", []),
    format(H, "    uint32_t start;~n", []),
    format(H, "    uint32_t end;~n", []),
    format(H, "    uint16_t prefix;   /* index into uniname_prefixes */~n", []),
    format(H, "    uint8_t  kind;~n", []),
    format(H, "} uniname_range_t;~n~n", []),
    format(H, "extern const char            uniname_words[UNINAME_WORDS_LEN];~n", []),
    format(H, "extern const char            uniname_prefixes[];~n", []),
    format(H, "extern const uniname_range_t uniname_ranges[UNINAME_RANGE_COUNT];~n", []),
    format(H, "extern const uint8_t         uniname_names[UNINAME_NAMES_LEN];~n~n", []),
    format(H, "#endif /*UNINAME_DATA_H_INCLUDED*/~n", []).

emit_source(C, V) :-
    file_header(C, "Compact Unicode name table.", V),
    format(C, "#include \"uniname_data.h\"~n~n", []),
    emit_words(C),
    emit_prefixes(C),
    emit_ranges(C),
    emit_names_blob(C).

%   Word table: NUL-separated, ascending id (descending frequency).
emit_words(C) :-
    ordered_words(Words),
    format(C, "const char uniname_words[UNINAME_WORDS_LEN] =~n", []),
    foldl(emit_word_chunk(C), Words, 0, _),
    format(C, ";~n~n", []).

emit_word_chunk(C, W, Col0, Col) :-
    (   Col0 =:= 0 -> format(C, "  ", []) ; true ),
    format(C, "\"~w\\0\"", [W]),
    Col1 is Col0+1,
    (   Col1 >= 8 -> format(C, "~n", []), Col = 0 ; Col = Col1 ).

ordered_words(Words) :-
    findall(Id-W, word_id(W,Id), P0), keysort(P0, P1), pairs_values(P1, Words).

words_blob_len(Len) :-
    aggregate_all(sum(L1), (word_id(W,_), atom_length(W,L0), L1 is L0+1), Len).

%   Distinct range prefixes, NUL-separated, indexed by uniname_ranges.
prefixes(Prefixes) :-
    findall(P, range(_,_,_,P), Ps0),
    sort(Ps0, Prefixes).

prefix_index(P, I) :-
    prefixes(Ps), nth0(I, Ps, P), !.

emit_prefixes(C) :-
    prefixes(Ps),
    format(C, "const char uniname_prefixes[] =~n  ", []),
    forall(member(P, Ps), format(C, "\"~w\\0\"", [P])),
    format(C, ";~n~n", []).

%   Ranges (sorted by start)
emit_ranges(C) :-
    findall(S-r(S,E,K,P), range(S,E,K,P), P0), keysort(P0, P1), pairs_values(P1, Rs),
    format(C, "const uniname_range_t uniname_ranges[UNINAME_RANGE_COUNT] = {~n", []),
    forall(member(r(S,E,K,P), Rs),
           ( kind_code(K, KC),
             prefix_index(P, PI),
             format(C, "  { 0x~16r, 0x~16r, ~d, ~d },~n", [S,E,PI,KC]) )),
    format(C, "};~n~n", []).

kind_code(hex, 0).
kind_code(hangul, 1).

%   Flat name byte stream
emit_names_blob(C) :-
    names_blob(Blob),
    format(C, "const uint8_t uniname_names[UNINAME_NAMES_LEN] = {~n", []),
    emit_bytes(C, Blob, 0),
    format(C, "~n};~n", []).

emit_bytes(_, [], _).
emit_bytes(C, [B|T], Col0) :-
    (   Col0 =:= 0 -> format(C, "  ", []) ; true ),
    format(C, "~d,", [B]),
    Col1 is Col0+1,
    (   Col1 >= 20 -> format(C, "~n", []), Col = 0 ; Col = Col1 ),
    emit_bytes(C, T, Col).

%   Build the flat byte stream once; cached for reuse by header/report.
:- dynamic names_blob_cache/1.

names_blob(Blob) :-
    (   names_blob_cache(B)
    ->  Blob = B
    ;   findall(Cp-Ws, named(Cp,Ws), P0), keysort(P0, P1),
        name_records(P1, 0, Blob),
        assertz(names_blob_cache(Blob))
    ).

name_records([], _, []).
name_records([Cp-Ws|T], Prev, Bytes) :-
    Delta is Cp-Prev,
    leb(Delta, DB),
    name_words(Ws, WB),
    append(DB, WB, Rec0),
    append(Rec0, [0|Rest], Bytes),
    name_records(T, Cp, Rest).

name_words([], []).
name_words([W|T], Bytes) :-
    word_id(W, Id), I1 is Id+1,
    leb(I1, LB),
    append(LB, Rest, Bytes),
    name_words(T, Rest).

%   Unsigned LEB128.
leb(N, [B]) :-
    N < 128, !,
    B = N.
leb(N, [B|T]) :-
    B is (N /\ 0x7f) \/ 0x80,
    N1 is N >> 7,
    leb(N1, T).

% ----------------------------------------------------------------------
%  Counts / report
% ----------------------------------------------------------------------

word_count(N)  :- aggregate_all(count, word_id(_,_), N).
range_count(N) :- aggregate_all(count, range(_,_,_,_), N).
named_count(N) :- aggregate_all(count, named(_,_), N).

report :-
    word_count(NW), range_count(NR), named_count(NN),
    words_blob_len(WL),
    names_blob(Blob), length(Blob, BL),
    prefixes(Ps),
    aggregate_all(sum(PL1), (member(P,Ps), atom_length(P,PL0), PL1 is PL0+1), PB),
    RangeBytes is NR*(4+4+2+1),
    Total is WL+PB+RangeBytes+BL,
    format(user_error, "~`-t~64|~n", []),
    format(user_error, "uniname (Unicode names, compact): ~D words, ~D ranges, ~D names~n",
           [NW,NR,NN]),
    format(user_error, "  word table     : ~D bytes~n", [WL]),
    format(user_error, "  range prefixes : ~D bytes~n", [PB]),
    format(user_error, "  ranges         : ~D bytes (~D entries)~n", [RangeBytes,NR]),
    format(user_error, "  name stream    : ~D bytes (LEB128 cp-delta + tokens)~n", [BL]),
    format(user_error, "  TOTAL static   : ~D bytes (~1f KB)~n",
           [Total, Total/1024]),
    format(user_error, "~`-t~64|~n", []).
