/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2026, University of Amsterdam
                              CWI, Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- module(unicode,
          [ unicode_property/2,         % ?Code, ?Property
            unicode_map/3,              % +In, -Out, +Options
            unicode_nfd/2,              % +In, -Out
            unicode_nfc/2,              % +In, -Out
            unicode_nfkd/2,             % +In, -Out
            unicode_nfkc/2,             % +In, -Out
            unicode_nfkc_casefold/2,    % +In, -Out
            unicode_casefold/2,         % +In, -Out
            unicode_version/1,          % -Version
            unicode_codepoint_valid/1,  % +Code
            atom_graphemes/2,           % ?Atom, ?Graphemes
            string_graphemes/2          % ?String, ?Graphemes
          ]).
:- use_foreign_library(foreign(unicode4pl)).

/** <module> Unicode string handling

This library wraps the [utf8proc](https://github.com/JuliaStrings/utf8proc)
library, giving Prolog code access to Unicode character properties,
string normalization, case folding, and grapheme-cluster iteration.

Three levels of API are provided:

1. **Normalization**: unicode_nfd/2, unicode_nfc/2, unicode_nfkd/2,
   unicode_nfkc/2 implement the four standard Unicode normalization
   forms (NFD, NFC, NFKD, NFKC; see UAX#15).  unicode_nfkc_casefold/2
   combines NFKC with case folding for caseless identifier matching
   (see UAX#31).
2. **Per-codepoint properties**: unicode_property/2 queries the
   Unicode property database (general category, bidi class,
   decomposition type, display width, case mappings, grapheme
   boundary class, ...).
3. **Mixed string-level transformations**: unicode_map/3 is the
   workhorse; it accepts a list of flags chosen from a fixed set and
   performs the corresponding composition of decompose / compose /
   strip / lump / case-fold / grapheme-boundary-mark operations in a
   single pass.  unicode_casefold/2 is a convenience wrapper.

Grapheme clusters (user-perceived characters) can be iterated with
atom_graphemes/2 and string_graphemes/2.

Loading this library also installs a Unicode NFC normalisation hook
into the SWI-Prolog kernel.  The kernel's `unicode_atoms` policy
(Prolog flag, stream property and `read_term/2,3` option) uses this
hook for its `nfc` and `error` modes; without the library loaded
those modes raise `existence_error(hook, unicode_normalize)`.  The
kernel's quoted-write rule that force-quotes atoms containing
combining marks is independent and works even without this library
loaded.

Lump handling:

==
U+0020      <-- all space characters (general category Zs)
U+0027  '   <-- left/right single quotation mark U+2018..2019,
                modifier letter apostrophe U+02BC,
                modifier letter vertical line U+02C8
U+002D  -   <-- all dash characters (general category Pd),
                minus U+2212
U+002F  /   <-- fraction slash U+2044,
                division slash U+2215
U+003A  :   <-- ratio U+2236
U+003C  <   <-- single left-pointing angle quotation mark U+2039,
                left-pointing angle bracket U+2329,
                left angle bracket U+3008
U+003E  >   <-- single right-pointing angle quotation mark U+203A,
                right-pointing angle bracket U+232A,
                right angle bracket U+3009
U+005C  \   <-- set minus U+2216
U+005E  ^   <-- modifier letter up arrowhead U+02C4,
                modifier letter circumflex accent U+02C6,
                caret U+2038,
                up arrowhead U+2303
U+005F  _   <-- all connector characters (general category Pc),
                modifier letter low macron U+02CD
U+0060  `   <-- modifier letter grave accent U+02CB
U+007C  |   <-- divides U+2223
U+007E  ~   <-- tilde operator U+223C
==

@see http://www.unicode.org/reports/tr15/  (UAX#15 Normalization)
@see http://www.unicode.org/reports/tr29/  (UAX#29 Grapheme Clusters)
@see http://www.unicode.org/reports/tr31/  (UAX#31 Identifiers)
@see https://github.com/JuliaStrings/utf8proc
*/

system:goal_expansion(unicode_map(In, Out, Options),
                      unicode_map(In, Out, Mask)) :-
    is_list(Options),
    unicode_option_mask(Options, Mask).

%!  unicode_map(+In, -Out, +Options) is det.
%
%   Perform a Unicode mapping on In, returning Out.  Options is a list
%   that may contain any combination of the flags below; a call is
%   roughly equivalent to `utf8proc_map(In, Options)` in the C API.
%
%       * stable
%       Respect Unicode versioning stability --- the result does not
%       depend on which (recent) version of Unicode is in use.
%       * compat
%       Use compatibility decomposition (i.e. formatting information is
%       lost).
%       * compose
%       Produce a composed result (e.g. NFC or NFKC, depending on the
%       presence of `compat`).
%       * decompose
%       Produce a decomposed result (NFD/NFKD).
%       * ignore
%       Strip "default ignorable" characters (e.g. soft hyphen, zero-width
%       space).
%       * rejectna
%       Raise an error instead of returning output when the input contains
%       unassigned code points.
%       * nlf2ls
%       Convert all NLF-sequences (LF, CRLF, CR, NEL) to U+2028 LINE
%       SEPARATOR.
%       * nlf2ps
%       Convert all NLF-sequences to U+2029 PARAGRAPH SEPARATOR.
%       * nlf2lf
%       Convert all NLF-sequences to U+000A LINE FEED.
%       * stripcc
%       Strip or convert control characters.  NLF-sequences become a
%       space, except if one of the NLF-conversion flags is set; HT and
%       FF are treated as NLF in this case.  All other control
%       characters are removed.
%       * casefold
%       Apply Unicode case folding (for caseless comparison).
%       * charbound
%       Insert a U+00FF byte at the beginning of every grapheme cluster
%       (UAX#29).  The result can be split on 0xFF to recover individual
%       graphemes; atom_graphemes/2 wraps this pattern.
%       * lump
%       Normalise typographic variants to their ASCII equivalents
%       (see module header for the full list).  Combined with
%       `nlf2lf`, paragraph and line separators become U+000A as well.
%       * stripmark
%       Strip all combining marks (non-spacing, spacing, enclosing).
%       Must be combined with `compose` or `decompose`.

%!  unicode_nfd(+In, -Out) is det.
%
%   Characters in In are decomposed by canonical equivalence (NFD).
%   Precomposed characters expand into base + combining marks.  For
%   example U+00C5 (LATIN CAPITAL LETTER A WITH RING ABOVE) becomes
%   the two-code sequence `A` + U+030A.
%
%   @see http://www.unicode.org/reports/tr15/

unicode_nfd(In, Out) :-
    unicode_map(In, Out, [stable,decompose]).

%!  unicode_nfc(+In, -Out) is det.
%
%   Characters in In are decomposed and then recomposed by canonical
%   equivalence (NFC).  Precomposed code points are preferred; for
%   example `A` + U+030A becomes the single code point U+00C5.
%
%   @see http://en.wikipedia.org/wiki/Unicode_equivalence#Normal_forms

unicode_nfc(In, Out) :-
    unicode_map(In, Out, [stable,compose]).

%!  unicode_nfkd(+In, -Out) is det.
%
%   Characters in In are decomposed by compatibility equivalence
%   (NFKD).  Compatibility decomposition expands presentation forms
%   (ligatures, subscripts, fullwidth letters) into their base forms;
%   for example U+FB03 (LATIN SMALL LIGATURE FFI) becomes `f f i`.

unicode_nfkd(In, Out) :-
    unicode_map(In, Out, [stable,decompose,compat]).

%!  unicode_nfkc(+In, -Out) is det.
%
%   Characters in In are decomposed by compatibility equivalence, then
%   recomposed by canonical equivalence (NFKC).

unicode_nfkc(In, Out) :-
    unicode_map(In, Out, [stable,compose,compat]).

%!  unicode_nfkc_casefold(+In, -Out) is det.
%
%   Equivalent to unicode_nfkc/2 followed by unicode_casefold/2 done
%   in a single pass.  This is the normalisation form recommended by
%   UAX#31 for caseless identifier matching.  For example German
%   `'Strasse'` written with U+00DF (LATIN SMALL LETTER SHARP S)
%   maps to `'strasse'`, and U+FB03 (LATIN SMALL LIGATURE FFI) maps
%   to `'ffi'`.

unicode_nfkc_casefold(In, Out) :-
    unicode_map(In, Out, [stable,compose,compat,casefold]).

%!  unicode_casefold(+In, -Out) is det.
%
%   Out is the case-folded form of In.  Use this for caseless
%   comparison that does not require NFKC; otherwise prefer
%   unicode_nfkc_casefold/2.

unicode_casefold(In, Out) :-
    unicode_map(In, Out, [stable,casefold]).


%!  unicode_property(?Code, ?Property) is nondet.
%
%   Query the Unicode character database for Code.  Code is an integer
%   code point (0 .. 0x10FFFF) or a single-character atom; Property is
%   a term of the form Name(Value) drawn from the list below.
%
%   This predicate is a thin wrapper over utf8proc's property struct,
%   so its vocabulary matches the utf8proc documentation.  In the
%   modes (+,?) and (-,?) the predicate enumerates properties for
%   the given code (or the code for the given property); in (+,+) it
%   is a deterministic test.
%
%   Supported properties:
%
%       * category(Atom)
%       Unicode general category.  Atom is one of `Cc`, `Cf`, `Cn`,
%       `Co`, `Cs`, `Ll`, `Lm`, `Lo`, `Lt`, `Lu`, `Mc`, `Me`, `Mn`,
%       `Nd`, `Nl`, `No`, `Pc`, `Pd`, `Pe`, `Pf`, `Pi`, `Po`, `Ps`,
%       `Sc`, `Sk`, `Sm`, `So`, `Zl`, `Zp`, `Zs`.  When querying, the
%       single capital letter of a subcategory stands for all its
%       subcategories; e.g.
%
%           ==
%           ?- unicode_property(0'A, category('L')).
%           true.
%           ==
%
%       * combining_class(Integer)
%       Canonical combining class (0 for base characters, 230 for
%       accents above, etc.).
%       * bidi_class(Atom)
%       Bidirectional class.  One of `l`, `lre`, `lro`, `r`, `al`,
%       `rle`, `rlo`, `pdf`, `en`, `es`, `et`, `an`, `cs`, `nsm`,
%       `bn`, `b`, `s`, `ws`, `on`.
%       * bidi_mirrored(Bool)
%       `true` if the character is mirrored for bidi (parentheses,
%       brackets, math operators, ...).
%       * decomp_type(Atom)
%       Compatibility decomposition type.  One of `font`, `nobreak`,
%       `initial`, `medial`, `final`, `isolated`, `circle`, `super`,
%       `sub`, `vertical`, `wide`, `narrow`, `small`, `square`,
%       `fraction`, `compat`.  Fails when there is no decomposition.
%       * ignorable(Bool)
%       `true` if the character is a "default ignorable" code point.
%       * boundclass(Atom)
%       UAX#29 grapheme-cluster break class.  One of `start`,
%       `other`, `cr`, `lf`, `control`, `extend`, `l`, `v`, `t`,
%       `lv`, `lvt`, `regional_indicator`, `spacingmark`, `prepend`,
%       `zwj`, `extended_pictographic`, `e_zwg`.
%       * width(Integer)
%       Display width in fixed-width cells, 0..3.  Zero for combining
%       marks and control characters, 1 for most, 2 for "wide"
%       characters (CJK, emoji).
%       * ambiguous_width(Bool)
%       `true` if the character has East-Asian Ambiguous width ---
%       normally one column, but two in a legacy CJK context.
%       * uppercase(Code)
%       * lowercase(Code)
%       * titlecase(Code)
%       Single-code-point case mapping.  Fails when the code point
%       has no mapping of that kind (e.g. `unicode_property(0'A,
%       uppercase(_))` fails because `'A'` is already upper-case).
%       For characters whose case mapping produces more than one code
%       point (e.g. U+00DF LATIN SMALL LETTER SHARP S maps to "SS"),
%       use unicode_map/3 with the `[casefold]` option or
%       unicode_casefold/2 for a full string-level transformation.
%       * indic_conjunct_break(Atom)
%       Indic_Conjunct_Break property (Unicode 15+; UAX#44).  One of
%       `none`, `linker`, `consonant`, `extend`.  Used by the
%       grapheme-cluster-break algorithm for Devanagari, Bengali,
%       etc.
%
%   @see http://www.unicode.org/reports/tr44/ (Unicode property database)

unicode_property(Code, Property) :-
    nonvar(Code), nonvar(Property),
    !,
    '$unicode_property'(Code, Property, false).
unicode_property(Code, Property) :-
    nonvar(Code),
    !,
    property(Property),
    '$unicode_property'(Code, Property, true).
unicode_property(Code, Property) :-
    var(Code),
    !,
    between(0, 0x10ffff, Code),
    property(Property),
    '$unicode_property'(Code, Property, true).

%   The third argument of `$unicode_property/3` is a `Silent` flag.
%   When `true`, the C layer returns `false` instead of raising
%   `domain_error(unicode_property, ...)` for a property name the
%   binding was compiled without (older libutf8proc versions lack
%   some fields).  Enumeration modes set it to `true` so
%   property/1 clauses for absent features are silently skipped.

property(category(_)).
property(combining_class(_)).
property(bidi_class(_)).
property(bidi_mirrored(_)).
property(decomp_type(_)).
property(ignorable(_)).
property(boundclass(_)).
property(width(_)).
property(ambiguous_width(_)).
property(uppercase(_)).
property(lowercase(_)).
property(titlecase(_)).
property(indic_conjunct_break(_)).


%!  atom_graphemes(?Atom, ?Graphemes) is det.
%
%   Relate Atom to a list of its grapheme clusters.  Grapheme clusters
%   are "user-perceived characters" as defined by UAX#29 --- e.g. the
%   precomposed U+00E9 (LATIN SMALL LETTER E WITH ACUTE) and the
%   decomposed sequence `e` + U+0301 are both one grapheme, an emoji
%   ZWJ sequence such as `MAN + ZWJ + WOMAN + ZWJ + GIRL` is one
%   grapheme, and a regional-indicator pair (e.g. U+1F1F3 U+1F1F1,
%   rendered as the Dutch flag) is one grapheme.
%
%   In the forward mode (+Atom, ?Graphemes), Atom is decomposed into
%   a list of atoms, each covering one cluster.  In the reverse mode
%   (?Atom, +Graphemes), the elements of Graphemes are concatenated
%   into Atom.  Both arguments instantiated means both modes run and
%   the result must agree.
%
%   ```
%   ?- atom_codes(A, [0'c, 0'a, 0'f, 0'e, 0x0301]),
%      atom_graphemes(A, Gs).
%   Gs = [c, a, f, G],
%   atom_codes(G, [0'e, 0x0301]).
%
%   ?- atom_graphemes(A, [a, b, c]).
%   A = abc.
%   ```
%
%   @see string_graphemes/2 for the string analogue.

%!  string_graphemes(?String, ?Graphemes) is det.
%
%   As atom_graphemes/2, but the elements of Graphemes are strings.

%!  unicode_version(-Version) is det.
%
%   Version is an atom describing the Unicode version implemented by
%   the linked utf8proc library, e.g. `'15.1.0'`.  This drives the
%   normalisation, case-folding and grapheme-cluster predicates in
%   this module, and may differ from the Unicode version of the
%   SWI-Prolog source syntax classifier reported by the read-only
%   Prolog flag `unicode_syntax_version`.

%!  unicode_codepoint_valid(+Code) is semidet.
%
%   True when Code is a non-negative integer that is a valid and
%   _assigned_ Unicode code point.  Unassigned code points (general
%   category `Cn`), surrogate halves (`Cs`), and integers outside
%   `0..0x10FFFF` all fail.


                /*******************************
                *           SANDBOX            *
                *******************************/

:- multifile
    sandbox:safe_primitive/1.

sandbox:safe_primitive(unicode:unicode_property(_,_)).
sandbox:safe_primitive(unicode:unicode_map(_,_,_)).
sandbox:safe_primitive(unicode:unicode_property(_,_)).
sandbox:safe_primitive(unicode:unicode_version(_)).
sandbox:safe_primitive(unicode:unicode_codepoint_valid(_)).
sandbox:safe_primitive(unicode:atom_graphemes(_,_)).
sandbox:safe_primitive(unicode:string_graphemes(_,_)).
