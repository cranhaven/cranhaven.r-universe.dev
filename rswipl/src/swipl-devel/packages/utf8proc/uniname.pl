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

:- module(uniname,
          [ unicode_name/2             % ?CodePoint, ?Name
          ]).
:- use_foreign_library(foreign(uniname4pl)).

/** <module> Unicode character names

This library relates  Unicode  code  points   to  their  formal  Unicode
character names (the `Name` property of `UnicodeData.txt`). It ships its
own compact UCD-derived table  (about  360   KB)  and  is independent of
library(unicode) and library(unicode_security).

Algorithmic name ranges (Hangul syllables, CJK and Tangut ideographs and
the various ``PREFIX-<hex>`` families)  are   synthesised  from the code
point and carry no per-code-point storage;   the remaining ~34,600 names
are stored as a shared  word  table   plus  a  packed  token stream. See
``etc/gen_uniname.pl`` in the package directory  to regenerate the table
on a Unicode-version bump.
*/

%!  unicode_name(?CodePoint:integer, ?Name:atom) is nondet.
%
%   True when Name is the Unicode character name of CodePoint.  Usage:
%
%     - `unicode_name(+CodePoint, -Name)` is semidet: the name of
%       CodePoint, failing when it has none (control, surrogate,
%       private-use or unassigned code points).
%     - `unicode_name(-CodePoint, +Name)` is semidet: the (unique)
%       code point with the given name.
%     - `unicode_name(-CodePoint, -Name)` is nondet: enumerate every
%       named code point on backtracking.
%
%   Name is an atom of the formal Unicode name in upper case, e.g.
%
%   ```
%   ?- unicode_name(0'A, N).
%   N = 'LATIN CAPITAL LETTER A'.
%
%   ?- unicode_name(C, 'EURO SIGN').
%   C = 8364.
%
%   ?- unicode_name(0xAC00, N).
%   N = 'HANGUL SYLLABLE GA'.
%   ```
