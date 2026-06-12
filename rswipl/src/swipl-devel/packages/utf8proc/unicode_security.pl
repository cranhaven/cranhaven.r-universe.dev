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

:- module(unicode_security,
          [ unicode_script/2,             % +Code, -Script
            unicode_script_extensions/2,  % +Code, -Scripts
            unicode_identifier_status/2,  % +Code, -Status
            unicode_identifier_type/2,    % +Code, -Types

            unicode_skeleton/2,           % +Text, -Skeleton
            unicode_confusable/2,         % +T1, +T2
            unicode_confusable/3,         % +T1, +T2, +Options

            unicode_resolved_scripts/2,   % +Text, -Scripts
            unicode_restriction_level/2   % +Text, -Level
          ]).
:- use_foreign_library(foreign(unicode_security4pl)).

/** <module> Unicode security helpers (UTS #39, UAX #24)

This  library  implements  helpers  from   [UTS  #39  (Unicode  Security
Mechanisms)](https://www.unicode.org/reports/tr39/)   and   the   script
properties of [UAX #24](https://www.unicode.org/reports/tr24/).   It  is
intended for linters, identifier validators and   any code that needs to
reason about confusable look-alike text  or mixed-script identifiers. It
does **not** alter  the  Prolog  reader;   UTS  #39  is  deliberately  a
library-level facility.

The library ships its  own  UCD-derived   tables  and  is independent of
`library(unicode)`  (which  wraps  libutf8proc   for  normalisation  and
per-code-point  properties).  See  `etc/gen_uts39.pl`   in  the  package
directory to regenerate the tables on a Unicode-version bump.

Predicates fall into three groups:

  - Per-code-point lookups: unicode_script/2,
    unicode_script_extensions/2, unicode_identifier_status/2,
    unicode_identifier_type/2.
  - Skeleton and confusable test (UTS #39 §4): unicode_skeleton/2,
    unicode_confusable/2, unicode_confusable/3.
  - String-level identifier checks (UTS #39 §5):
    unicode_resolved_scripts/2, unicode_restriction_level/2.
*/

%!  unicode_script(+Code:integer, -Script:atom) is semidet.
%
%   True when Script is the UAX #24 Script_Property of Code. Script is a
%   lower-case atom of the  long   property  value (`latin`, `cyrillic`,
%   `han`, `common`, `inherited`, ...). Fails   for  code points outside
%   the Unicode range or with no entry in Scripts.txt.

%!  unicode_script_extensions(+Code:integer, -Scripts:list(atom)) is semidet.
%
%   Scripts is the sorted list of UAX #24 Script_Extensions of Code. For
%   most code points this is  a   singleton  `[Script]`.  Fails for code
%   points outside the Unicode range and for   code points with no entry
%   in either ScriptExtensions.txt or Scripts.txt.

%!  unicode_identifier_status(+Code:integer, -Status:atom) is semidet.
%
%   Succeeds, unifying Status with `allowed`, when Code is listed in UTS
%   #39 IdentifierStatus.txt with status ``Allowed``.  Fails otherwise —
%   per UTS #39 every code  point  not   listed  there  is Restricted by
%   default; rather than return `restricted`   for everything else, this
%   predicate simply fails.

%!  unicode_identifier_type(+Code:integer, -Types:list(atom)) is semidet.
%
%   Types is the sorted list of UTS   #39 Identifier_Type atoms for Code
%   (`recommended`, `inclusion`, `technical`, `obsolete`, `limited_use`,
%   `exclusion`,     `not_nfkc`,     `not_xid`,     `default_ignorable`,
%   `deprecated`, `uncommon_use`). Fails for  code   points  outside the
%   Unicode range or with no entry in IdentifierType.txt.

%!  unicode_skeleton(+Text, -Skeleton:atom) is det.
%
%   Compute the UTS #39 §4 skeleton of  Text: apply NFD, substitute each
%   code point with its confusables.txt prototype string, then apply NFD
%   again. Two strings are confusable iff their skeletons compare equal.

%!  unicode_confusable(+T1, +T2) is semidet.
%
%   True when unicode_skeleton/2 of T1 and T2 are equal.

%!  unicode_confusable(+T1, +T2, +Options) is semidet.
%
%   As unicode_confusable/2.  Options:
%
%   * ignore_intentional(+Bool)
%     If `true`, skip the per-character substitution when the source
%     and target form a pair listed in UTS #39 intentional.txt (e.g.
%     Latin A versus Greek capital Alpha).  Default `false`.

%!  unicode_resolved_scripts(+Text, -Scripts:list(atom)) is det.
%
%   Scripts is the UTS #39 §5.1 resolved augmented Script_Extensions set
%   of   Text:   the   intersection    of     `augscx(c)`    over    all
%   non-Common/non-Inherited characters, with the augmentation rules for
%   Han, Hiragana, Katakana, Hangul and Bopomofo applied. The empty list
%   signals a mixed-script string.

%!  unicode_restriction_level(+Text, -Level:atom) is det.
%
%   Classify Text under UTS #39 §5.2 at   the most restrictive level for
%   which it qualifies. Level is one of:
%
%   * `ascii_only` — every code point in U+0020..U+007E and Allowed.
%   * `single_script` — augmented resolved-script-set non-empty and
%     every code point Allowed.
%   * `highly_restrictive` — covered by Latin plus one of ``Hanb``,
%     ``Jpan`` or ``Kore`` (UTS #39 §5.1 augmented profiles).
%   * `moderately_restrictive` — covered by Latin plus a single
%     non-Latin Recommended script (``Cyrl`` or ``Grek``).
%   * `minimally_restrictive` — every code point has Identifier_Type
%     in `{recommended, inclusion}`.
%   * `unrestricted` — otherwise.
%
%   A linter that walks  source  clauses   and  reports  atoms  with the
%   confusability issues above is registered  in `library(check)` itself
%   (predicate      `list_confusable_identifiers/0`);      see       the
%   `library(check)` documentation for details.
