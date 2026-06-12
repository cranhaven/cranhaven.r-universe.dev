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

:- module(gen_uts39, [main/1]).
:- use_module(library(main)).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(readutil)).
:- use_module(library(filesex)).
:- use_module(library(pairs)).
:- use_module(library(yall)).

:- initialization(main, main).

/** <module> Generate UTS #39 / UAX #24 data tables

Reads vendored UCD files from a directory (default `data/`) and emits
`uts39_data.c` and `uts39_data.h` (default into the parent directory of
the generator).  The generated files are checked in; this script is
re-run only when the underlying UCD files change.

Usage (from packages/utf8proc/):

    swipl etc/gen_uts39.pl --ucd-dir data --out .

The output is deterministic so re-running on unchanged inputs produces
no diff.
*/

main(Argv) :-
    parse_args(Argv, Opts),
    option(ucd_dir(UCD), Opts),
    option(out(Out), Opts),
    setup(UCD),
    emit(Out),
    format(user_error, "Wrote ~w/uts39_data.{c,h}~n", [Out]).

parse_args(Argv, Opts) :-
    parse_args_(Argv, Opts0),
    defaults(Opts0, Opts).

parse_args_([], []).
parse_args_(['--ucd-dir', D | T], [ucd_dir(D) | R]) :- !, parse_args_(T, R).
parse_args_(['--out', O | T],     [out(O) | R])     :- !, parse_args_(T, R).
parse_args_([A | _], _) :- domain_error(option, A).

defaults(In, Out) :-
    (   option(ucd_dir(_), In) -> In1 = In
    ;   In1 = [ucd_dir('data')|In]
    ),
    (   option(out(_), In1)    -> Out = In1
    ;   Out = [out('.')|In1]
    ).

% ----------------------------------------------------------------------
%  Loading phase
% ----------------------------------------------------------------------

:- dynamic
    script_alias/2,           % +Short, +Long
    script_id/2,              % +Short, -Id (integer; assigned at finalise)
    raw_script/3,             % +Start, +End, +Short
    raw_scx/3,                % +Start, +End, +ListOfShort
    raw_idstatus/3,           % +Start, +End, +Status (allowed|restricted)
    raw_idtype_set/3,         % +Start, +End, +ListOfTypeAtoms
    raw_skel/3,               % +Src, +TargetCps, +Type
    raw_intentional/2,        % +Src, +Tgt
    unicode_version/1.

setup(UCD) :-
    retractall(script_alias(_,_)),
    retractall(script_id(_,_)),
    retractall(raw_script(_,_,_)),
    retractall(raw_scx(_,_,_)),
    retractall(raw_idstatus(_,_,_)),
    retractall(raw_idtype_set(_,_,_)),
    retractall(raw_skel(_,_,_)),
    retractall(raw_intentional(_,_)),
    retractall(unicode_version(_)),
    load_aliases(UCD),
    load_synthetic_aliases,
    load_scripts(UCD),
    load_scx(UCD),
    load_idstatus(UCD),
    load_idtype(UCD),
    load_skeleton(UCD),
    load_intentional(UCD),
    assign_script_ids,
    detect_version(UCD).

% UTS #39 augmented-script-only codes (ISO 15924).  These don't appear
% in PropertyValueAliases.txt as `sc` lines because no single character
% has them as its primary script, but they are referenced by the §5.1
% augmentation rules.  Long names follow ISO 15924.
load_synthetic_aliases :-
    forall(member(Short-Long,
                  [ 'Hanb'-'Han_With_Bopomofo',
                    'Jpan'-'Japanese',
                    'Kore'-'Korean'
                  ]),
           ensure_script_known(Short, Long)).

detect_version(UCD) :-
    directory_file_path(UCD, 'confusables.txt', Path),
    read_first_lines(Path, 30, Lines),
    (   member(Line, Lines),
        sub_atom(Line, _, _, _, 'Version:'),
        split_string(Line, ":", " \t", [_, V0|_]),
        string_to_atom(V0, V),
        assertz(unicode_version(V)), !
    ;   assertz(unicode_version('unknown'))
    ).

read_first_lines(Path, N, Lines) :-
    setup_call_cleanup(
        open(Path, read, In, [encoding(utf8)]),
        read_n_lines(In, N, Lines),
        close(In)).

read_n_lines(_, 0, []) :- !.
read_n_lines(In, N, [L|T]) :-
    read_line_to_string(In, S),
    (   S == end_of_file -> T = []
    ;   atom_string(L, S),
        N1 is N-1,
        read_n_lines(In, N1, T)
    ).

% ----------------------------------------------------------------------
%  PropertyValueAliases.txt -> sc ; SHORT ; LONG (; ALT)*
% ----------------------------------------------------------------------

load_aliases(UCD) :-
    foreach_data_line(UCD, 'PropertyValueAliases.txt', read_alias_line).

read_alias_line(Line) :-
    split_string(Line, ";", " \t", Parts),
    Parts = [P|_],
    string_to_atom(P, sc), !,
    Parts = [_, ShortS, LongS | _],
    string_to_atom(ShortS, Short),
    canonical_atom(LongS, Long),
    assertz(script_alias(Short, Long)).
read_alias_line(_).

canonical_atom(S, A) :-
    string_lower(S, L0),
    atom_string(A, L0).

% ----------------------------------------------------------------------
%  Scripts.txt    -> RANGE ; LongScriptName
% ----------------------------------------------------------------------

load_scripts(UCD) :-
    foreach_data_line(UCD, 'Scripts.txt', read_script_line).

read_script_line(Line) :-
    split_codepoints_field(Line, Start, End, [LongS | _]),
    canonical_atom(LongS, Long),
    long_to_short(Long, Short),
    ensure_script_known(Short, Long),
    assertz(raw_script(Start, End, Short)).

% Reverse lookup: long -> short via aliases (case-insensitive on long).
long_to_short(Long, Short) :-
    script_alias(Short, Long), !.
long_to_short(Long, _) :-
    format(user_error, "Unknown script (no sc alias): ~w~n", [Long]),
    fail.

ensure_script_known(Short, Long) :-
    (   script_alias(Short, _) -> true
    ;   assertz(script_alias(Short, Long))
    ).

% ----------------------------------------------------------------------
%  ScriptExtensions.txt  -> RANGE ; SHORT SHORT SHORT ...
% ----------------------------------------------------------------------

load_scx(UCD) :-
    foreach_data_line(UCD, 'ScriptExtensions.txt', read_scx_line).

read_scx_line(Line) :-
    split_codepoints_field(Line, Start, End, [ShortsS | _]),
    split_string(ShortsS, " \t", " \t", Shorts0),
    exclude(=(""), Shorts0, Shorts1),
    maplist([S,A]>>string_to_atom(S,A), Shorts1, Shorts),
    sort(Shorts, ShortsSorted),
    assertz(raw_scx(Start, End, ShortsSorted)).

% ----------------------------------------------------------------------
%  IdentifierStatus.txt  -> RANGE ; Allowed|Restricted
% ----------------------------------------------------------------------

load_idstatus(UCD) :-
    foreach_data_line(UCD, 'IdentifierStatus.txt', read_idstatus_line).

read_idstatus_line(Line) :-
    split_codepoints_field(Line, Start, End, [StatusS | _]),
    canonical_atom(StatusS, Status),
    assertz(raw_idstatus(Start, End, Status)).

% ----------------------------------------------------------------------
%  IdentifierType.txt    -> RANGE ; TYPE TYPE TYPE ...
% ----------------------------------------------------------------------

load_idtype(UCD) :-
    foreach_data_line(UCD, 'IdentifierType.txt', read_idtype_line).

read_idtype_line(Line) :-
    split_codepoints_field(Line, Start, End, [TypesS | _]),
    split_string(TypesS, " \t", " \t", T0),
    exclude(=(""), T0, T1),
    maplist([S,A]>>canonical_atom(S,A), T1, Types),
    sort(Types, TypesSorted),
    assertz(raw_idtype_set(Start, End, TypesSorted)).

% ----------------------------------------------------------------------
%  confusables.txt       -> SRC ; TGT TGT TGT ; TYPE
% ----------------------------------------------------------------------

load_skeleton(UCD) :-
    foreach_data_line(UCD, 'confusables.txt', read_skel_line).

read_skel_line(Line) :-
    split_string(Line, ";", " \t", Parts),
    Parts = [SrcS, TgtS, TypeS | _],
    hex_codepoint(SrcS, Src),
    parse_cp_list(TgtS, Cps),
    string_to_atom(TypeS, Type),
    assertz(raw_skel(Src, Cps, Type)).

parse_cp_list(S, Cps) :-
    split_string(S, " \t", " \t", Parts0),
    exclude(=(""), Parts0, Parts),
    maplist(hex_codepoint, Parts, Cps).

hex_codepoint(S, Cp) :-
    (   string(S) -> atom_string(A, S) ; A = S ),
    atom_codes(A, Codes),
    catch(number_codes(Cp, [0'0,0'x | Codes]), _, fail).

% ----------------------------------------------------------------------
%  intentional.txt       -> SRC ; TGT
% ----------------------------------------------------------------------

load_intentional(UCD) :-
    foreach_data_line(UCD, 'intentional.txt', read_intentional_line).

read_intentional_line(Line) :-
    split_string(Line, ";", " \t", Parts),
    Parts = [SrcS, TgtS | _],
    % Strip a trailing comment from TgtS if any (#* ..., # ...)
    split_string(TgtS, "#", " \t", [TgtClean|_]),
    hex_codepoint(SrcS, Src),
    string_to_atom(TgtClean, TgtAtom),
    hex_codepoint(TgtAtom, Tgt),
    assertz(raw_intentional(Src, Tgt)).

% ----------------------------------------------------------------------
%  Common line parsing
% ----------------------------------------------------------------------

foreach_data_line(Dir, File, Goal) :-
    directory_file_path(Dir, File, Path),
    setup_call_cleanup(
        open(Path, read, In, [encoding(utf8)]),
        process_lines(In, Goal),
        close(In)).

process_lines(In, Goal) :-
    read_line_to_string(In, S),
    (   S == end_of_file -> true
    ;   strip_comment(S, Clean),
        (   Clean == "" -> true
        ;   atom_string(Line, Clean),
            (   call(Goal, Line) -> true ; true )
        ),
        process_lines(In, Goal)
    ).

strip_comment(S0, S) :-
    sub_string(S0, B, _, _, "#"), !,
    sub_string(S0, 0, B, _, S1),
    string_trim_trailing(S1, S).
strip_comment(S0, S) :-
    string_trim_trailing(S0, S).

string_trim_trailing(S0, S) :-
    string_codes(S0, Cs0),
    reverse(Cs0, Rs),
    drop_ws(Rs, RsClean),
    reverse(RsClean, Cs),
    string_codes(S, Cs).

drop_ws([C|T], R) :- (C == 0' ; C == 0'\t), !, drop_ws(T, R).
drop_ws(L, L).

split_codepoints_field(Line, Start, End, RestFields) :-
    split_string(Line, ";", " \t", [RangeS | RestFields]),
    parse_range(RangeS, Start, End).

parse_range(S, Start, End) :-
    (   sub_string(S, B, 2, _, "..")
    ->  sub_string(S, 0, B, _, AS),
        BAfter is B+2,
        sub_string(S, BAfter, _, 0, BS),
        hex_codepoint(AS, Start),
        hex_codepoint(BS, End)
    ;   hex_codepoint(S, Start),
        End = Start
    ).

% ----------------------------------------------------------------------
%  Assign script IDs
%
%  Common=0 and Inherited=1 are pinned (they're special in the UTS #39
%  algorithm: they don't participate in mixed-script detection).
%  Everything else is assigned alphabetically by short name so the
%  generated C is stable diff-wise.
% ----------------------------------------------------------------------

assign_script_ids :-
    findall(Short, script_alias(Short, _), All0),
    sort(All0, All),
    select('Zyyy', All, Rest1),       % Common
    select('Zinh', Rest1, Rest2),     % Inherited
    assertz(script_id('Zyyy', 0)),
    assertz(script_id('Zinh', 1)),
    assign_ids(Rest2, 2).

assign_ids([], _).
assign_ids([S|T], N) :-
    assertz(script_id(S, N)),
    N1 is N+1,
    assign_ids(T, N1).

% ----------------------------------------------------------------------
%  Range coalescing
% ----------------------------------------------------------------------

%! coalesce(+Triples, -Coalesced)
%
%  Triples = [Start-End-Value, ...] sorted by Start; merge adjacent
%  ranges with identical Value.

coalesce([], []).
coalesce([S-E-V|T], R) :- coalesce_(T, S, E, V, R).

coalesce_([], S, E, V, [S-E-V]).
coalesce_([S2-E2-V|T], S, E, V, R) :-
    S2 =:= E+1, !,
    coalesce_(T, S, E2, V, R).
coalesce_([S2-E2-V2|T], S, E, V, [S-E-V|R]) :-
    coalesce_(T, S2, E2, V2, R).

% ----------------------------------------------------------------------
%  Identifier type vocabulary
%
%  Bitset over the set of types ever seen; assigned alphabetically so
%  diffs are stable.
% ----------------------------------------------------------------------

idtype_vocabulary(Types) :-
    findall(Tlist, raw_idtype_set(_,_,Tlist), Lists),
    flatten(Lists, Flat),
    sort(Flat, Types).

idtype_bit(Types, Type, Bit) :-
    nth0(Bit, Types, Type).

idtype_set_bits(Vocab, TypeList, Bits) :-
    findall(B, ( member(T, TypeList), idtype_bit(Vocab, T, B) ), Bs0),
    sort(Bs0, Bs),
    foldl([B,Acc0,Acc]>>(Acc is Acc0 \/ (1<<B)), Bs, 0, Bits).

% ----------------------------------------------------------------------
%  scx set uniqueing
% ----------------------------------------------------------------------

scx_unique_sets(VocabularySorted, IdMap, Sets) :-
    findall(Shorts, raw_scx(_,_,Shorts), All0),
    sort(All0, Shorts1),
    % Encode each set as a sorted list of script ids
    maplist(shorts_to_ids(VocabularySorted), Shorts1, IdLists0),
    sort(IdLists0, Sets),
    length(Sets, _),
    % IdMap: shorts -> index into Sets
    findall(Shorts-Idx, ( member(Shorts, Shorts1),
                          shorts_to_ids(VocabularySorted, Shorts, Ids),
                          nth0(Idx, Sets, Ids) ),
            IdMap).

shorts_to_ids(_, Shorts, Ids) :-
    findall(Id, ( member(S, Shorts), script_id(S, Id) ), Ids0),
    sort(Ids0, Ids).

% ----------------------------------------------------------------------
%  Emission
% ----------------------------------------------------------------------

emit(Dir) :-
    directory_file_path(Dir, 'uts39_data.h', HPath),
    directory_file_path(Dir, 'uts39_data.c', CPath),
    setup_call_cleanup(
        open(HPath, write, H, [encoding(utf8)]),
        emit_header(H), close(H)),
    setup_call_cleanup(
        open(CPath, write, C, [encoding(utf8)]),
        emit_source(C), close(C)).

% --- Header --------------------------------------------------------------

emit_header(H) :-
    file_header(H, "Declarations for the UTS #39 / UAX #24 data tables."),
    unicode_version(V),
    format(H, "#ifndef UTS39_DATA_H_INCLUDED~n", []),
    format(H, "#define UTS39_DATA_H_INCLUDED~n~n", []),
    format(H, "#include <stdint.h>~n#include <stddef.h>~n~n", []),
    format(H, "#define UTS39_UNICODE_VERSION \"~w\"~n~n", [V]),
    idtype_vocabulary(Vocab),
    length(Vocab, NTypes),
    format(H, "#define UTS39_IDTYPE_COUNT ~d~n", [NTypes]),
    forall(nth0(I, Vocab, T),
           ( upcase_atom(T, U),
             format(H, "#define UTS39_IDTYPE_~w ~d~n", [U, I]) )),
    nl(H),
    % Identifier_Status
    format(H, "#define UTS39_ID_ALLOWED    1~n", []),
    format(H, "#define UTS39_ID_RESTRICTED 0~n~n", []),
    % Restriction-level codes
    format(H, "typedef enum {~n", []),
    format(H, "    UTS39_RL_ASCII_ONLY = 0,~n", []),
    format(H, "    UTS39_RL_SINGLE_SCRIPT,~n", []),
    format(H, "    UTS39_RL_HIGHLY_RESTRICTIVE,~n", []),
    format(H, "    UTS39_RL_MODERATELY_RESTRICTIVE,~n", []),
    format(H, "    UTS39_RL_MINIMALLY_RESTRICTIVE,~n", []),
    format(H, "    UTS39_RL_UNRESTRICTED~n", []),
    format(H, "} uts39_restriction_level_t;~n~n", []),
    % Script ids
    findall(Short-Id, script_id(Short, Id), Pairs0),
    keysort(Pairs0, _),
    findall(Id-Short, script_id(Short, Id), IPairs0),
    keysort(IPairs0, IPairs),
    length(IPairs, NScripts),
    format(H, "#define UTS39_SCRIPT_COUNT ~d~n", [NScripts]),
    forall(member(I-Short, IPairs),
           format(H, "#define UTS39_SC_~w ~d~n", [Short, I])),
    nl(H),
    NBits is (NScripts + 63) // 64,
    format(H, "#define UTS39_SCRIPT_BITSET_WORDS ~d~n~n", [NBits]),
    format(H, "/* A range covers start..start+len-1; value is the lookup payload */~n", []),
    format(H, "typedef struct {~n", []),
    format(H, "    uint32_t start;~n", []),
    format(H, "    uint16_t len;~n", []),
    format(H, "    uint16_t value;~n", []),
    format(H, "} uts39_range_t;~n~n", []),
    % Tables
    format(H, "extern const char *const uts39_script_short[UTS39_SCRIPT_COUNT];~n", []),
    format(H, "extern const char *const uts39_script_long [UTS39_SCRIPT_COUNT];~n~n", []),
    format(H, "extern const uts39_range_t uts39_script_ranges[];~n", []),
    format(H, "extern const size_t        uts39_script_ranges_count;~n~n", []),
    format(H, "extern const uts39_range_t uts39_scx_ranges[];~n", []),
    format(H, "extern const size_t        uts39_scx_ranges_count;~n", []),
    format(H, "extern const uint64_t      uts39_scx_sets[][UTS39_SCRIPT_BITSET_WORDS];~n", []),
    format(H, "extern const size_t        uts39_scx_sets_count;~n~n", []),
    format(H, "extern const uts39_range_t uts39_idstatus_ranges[];~n", []),
    format(H, "extern const size_t        uts39_idstatus_ranges_count;~n~n", []),
    format(H, "extern const uts39_range_t uts39_idtype_ranges[];~n", []),
    format(H, "extern const size_t        uts39_idtype_ranges_count;~n~n", []),
    format(H, "typedef struct {~n", []),
    format(H, "    uint32_t src;~n", []),
    format(H, "    uint32_t offset;~n", []),
    format(H, "    uint16_t length;~n", []),
    format(H, "} uts39_skeleton_entry_t;~n~n", []),
    format(H, "extern const uts39_skeleton_entry_t uts39_skeleton_entries[];~n", []),
    format(H, "extern const size_t                 uts39_skeleton_entries_count;~n", []),
    format(H, "extern const uint32_t               uts39_skeleton_chars[];~n~n", []),
    format(H, "typedef struct { uint32_t a, b; } uts39_pair_t;~n", []),
    format(H, "extern const uts39_pair_t uts39_intentional[];~n", []),
    format(H, "extern const size_t       uts39_intentional_count;~n~n", []),
    format(H, "#endif~n", []).

% --- Source --------------------------------------------------------------

emit_source(C) :-
    file_header(C, "Generated UTS #39 / UAX #24 data tables."),
    format(C, "#include \"uts39_data.h\"~n~n", []),
    emit_script_names(C),
    emit_script_ranges(C),
    emit_scx(C),
    emit_idstatus(C),
    emit_idtype(C),
    emit_skeleton(C),
    emit_intentional(C).

emit_script_names(C) :-
    findall(I-Short, script_id(Short, I), IPairs0),
    keysort(IPairs0, IPairs),
    format(C, "const char *const uts39_script_short[UTS39_SCRIPT_COUNT] = {~n", []),
    forall(member(I-Short, IPairs),
           format(C, "    [~d] = \"~w\",~n", [I, Short])),
    format(C, "};~n~n", []),
    format(C, "const char *const uts39_script_long[UTS39_SCRIPT_COUNT] = {~n", []),
    forall(member(I-Short, IPairs),
           ( script_alias(Short, Long),
             format(C, "    [~d] = \"~w\",~n", [I, Long]) )),
    format(C, "};~n~n", []).

emit_script_ranges(C) :-
    findall(S-E-Short, raw_script(S, E, Short), R0),
    sort(R0, R1),
    maplist([S-E-Short, S-E-Id]>>script_id(Short, Id), R1, R2),
    coalesce(R2, R3),
    split_oversize(R3, R4),
    length(R4, N),
    format(C, "const size_t uts39_script_ranges_count = ~d;~n", [N]),
    format(C, "const uts39_range_t uts39_script_ranges[] = {~n", []),
    forall(member(S-E-Id, R4),
           emit_range(C, S, E, Id)),
    format(C, "};~n~n", []).

%! split_oversize(+Ranges, -Out)
%
%   Break any S-E-V whose length exceeds 65535 into smaller pieces, so
%   the generated table's len field fits a uint16_t.

split_oversize([], []).
split_oversize([S-E-V|T], Out) :-
    Len is E - S + 1,
    (   Len =< 0xFFFF
    ->  Out = [S-E-V|R],
        split_oversize(T, R)
    ;   E1 is S + 0xFFFF - 1,
        S1 is E1 + 1,
        Out = [S-E1-V|R],
        split_oversize([S1-E-V|T], R)
    ).

emit_range(C, S, E, V) :-
    Len is E - S + 1,
    format(C, "    { 0x~|~`0t~16r~6+, ~|~t~d~5+, ~d },~n", [S, Len, V]).

emit_scx(C) :-
    % Build vocabulary of script-id sets from raw_scx
    findall(Shorts, raw_scx(_,_,Shorts), All0),
    sort(All0, ShortsList),
    maplist([Shorts, Ids]>>shorts_to_ids(_, Shorts, Ids), ShortsList, IdSets0),
    sort(IdSets0, UniqueSets),
    length(UniqueSets, NSets),
    findall(S-E-SetIdx,
            ( raw_scx(S, E, Shorts),
              shorts_to_ids(_, Shorts, Ids),
              nth0(SetIdx, UniqueSets, Ids) ),
            R0),
    sort(R0, R1),
    coalesce(R1, R2),
    split_oversize(R2, R3),
    length(R3, NRanges),
    % Emit the unique bitsets
    nscripts(NScripts),
    NBits is (NScripts + 63) // 64,
    format(C, "const size_t uts39_scx_sets_count = ~d;~n", [NSets]),
    format(C, "const uint64_t uts39_scx_sets[][~d] = {~n", [NBits]),
    forall(nth0(I, UniqueSets, Ids),
           emit_bitset(C, I, Ids, NBits)),
    format(C, "};~n~n", []),
    format(C, "const size_t uts39_scx_ranges_count = ~d;~n", [NRanges]),
    format(C, "const uts39_range_t uts39_scx_ranges[] = {~n", []),
    forall(member(S-E-Idx, R3),
           emit_range(C, S, E, Idx)),
    format(C, "};~n~n", []).

nscripts(N) :-
    aggregate_all(count, script_id(_,_), N).

emit_bitset(C, Idx, Ids, NBits) :-
    bits_to_words(Ids, NBits, Words),
    format(C, "    [~d] = {", [Idx]),
    emit_words(C, Words),
    format(C, "},~n", []).

bits_to_words(Ids, NBits, Words) :-
    length(Words0, NBits),
    maplist(=(0), Words0),
    foldl(set_bit, Ids, Words0, Words).

set_bit(Bit, In, Out) :-
    W is Bit // 64,
    B is Bit mod 64,
    nth0(W, In, V0),
    V is V0 \/ (1<<B),
    replace_nth(In, W, V, Out).

replace_nth([_|T], 0, V, [V|T]) :- !.
replace_nth([H|T], N, V, [H|R]) :- N1 is N-1, replace_nth(T, N1, V, R).

emit_words(_, []) :- !.
emit_words(C, [W]) :- !,
    format(C, " 0x~|~`0t~16r~16+ULL ", [W]).
emit_words(C, [W|T]) :-
    format(C, " 0x~|~`0t~16r~16+ULL,", [W]),
    emit_words(C, T).

emit_idstatus(C) :-
    findall(S-E-V, raw_idstatus(S, E, allowed), R0),
    sort(R0, R1),
    maplist([S-E-_, S-E-1]>>true, R1, R2),
    coalesce(R2, R3),
    split_oversize(R3, R4),
    length(R4, N),
    format(C, "const size_t uts39_idstatus_ranges_count = ~d;~n", [N]),
    format(C, "const uts39_range_t uts39_idstatus_ranges[] = {~n", []),
    forall(member(S-E-V, R4),
           emit_range(C, S, E, V)),
    format(C, "};~n~n", []).

emit_idtype(C) :-
    idtype_vocabulary(Vocab),
    findall(S-E-Bits,
            ( raw_idtype_set(S, E, Types),
              idtype_set_bits(Vocab, Types, Bits) ),
            R0),
    sort(R0, R1),
    coalesce(R1, R2),
    split_oversize(R2, R3),
    length(R3, N),
    format(C, "const size_t uts39_idtype_ranges_count = ~d;~n", [N]),
    format(C, "const uts39_range_t uts39_idtype_ranges[] = {~n", []),
    forall(member(S-E-V, R3),
           emit_range(C, S, E, V)),
    format(C, "};~n~n", []).

emit_skeleton(C) :-
    findall(S-T, raw_skel(S, T, _), R0),
    sort(R0, R1),
    % First pass: compute offsets and total chars
    skel_offsets(R1, 0, Entries, AllChars),
    length(Entries, N),
    length(AllChars, NC),
    format(C, "const size_t uts39_skeleton_entries_count = ~d;~n", [N]),
    format(C, "const uts39_skeleton_entry_t uts39_skeleton_entries[] = {~n", []),
    forall(member(Src-Off-Len, Entries),
           format(C, "    { 0x~|~`0t~16r~6+, ~d, ~d },~n", [Src, Off, Len])),
    format(C, "};~n~n", []),
    format(C, "/* total target chars: ~d */~n", [NC]),
    format(C, "const uint32_t uts39_skeleton_chars[] = {~n   ", []),
    emit_cp_array(C, AllChars, 0),
    format(C, "~n};~n~n", []).

skel_offsets([], _, [], []).
skel_offsets([Src-Cps | T], Off, [Src-Off-Len | TE], AllChars) :-
    length(Cps, Len),
    OffNext is Off + Len,
    skel_offsets(T, OffNext, TE, Rest),
    append(Cps, Rest, AllChars).

emit_cp_array(_, [], _) :- !.
emit_cp_array(C, [Cp|T], Col) :-
    format(C, " 0x~|~`0t~16r~6+,", [Cp]),
    Col1 is Col+1,
    (   Col1 mod 8 =:= 0
    ->  format(C, "~n   ", [])
    ;   true
    ),
    emit_cp_array(C, T, Col1).

emit_intentional(C) :-
    findall(A-B, raw_intentional(A, B), R0),
    sort(R0, R1),
    length(R1, N),
    format(C, "const size_t uts39_intentional_count = ~d;~n", [N]),
    format(C, "const uts39_pair_t uts39_intentional[] = {~n", []),
    forall(member(A-B, R1),
           format(C, "    { 0x~|~`0t~16r~6+, 0x~|~`0t~16r~6+ },~n", [A, B])),
    format(C, "};~n~n", []).

% --- shared file header --------------------------------------------------

file_header(Stream, Subject) :-
    unicode_version(V),
    format(Stream, "/* ~w~n", [Subject]),
    format(Stream, " *~n", []),
    format(Stream, " * GENERATED FILE -- DO NOT EDIT.~n", []),
    format(Stream, " * Regenerate with `ninja regen-uts39` in the build dir.~n", []),
    format(Stream, " *~n", []),
    format(Stream, " * Source: UTS #39 ~w (Unicode Security Mechanisms) and~n", [V]),
    format(Stream, " *         UAX #24 (Script_Property, Script_Extensions).~n", []),
    format(Stream, " * Input files vendored under packages/utf8proc/data/.~n", []),
    format(Stream, " */~n~n", []).
