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

#include <SWI-Prolog.h>
#include <stdlib.h>
#include <string.h>
#include "uniname.h"

#define NAME_BUF 256

static int
unify_name(term_t t, const char *s, size_t len)
{ return PL_unify_chars(t, PL_ATOM|REP_UTF8, len, s);
}

/** unicode_name(?CodePoint, ?Name) is nondet.
 *
 *  Relates a Unicode code point to its Unicode character name.  Modes:
 *
 *    - (+,?)  deterministic: the name of CodePoint, or failure when it
 *      has none (controls, surrogates, private use, unassigned).
 *    - (-,+)  semidet reverse lookup (names are unique).
 *    - (-,-)  enumerates every named code point on backtracking.
 */
static foreign_t
pl_unicode_name(term_t code, term_t name, control_t h)
{ uniname_iter *it;
  char buf[NAME_BUF];

  switch( PL_foreign_control(h) )
  { case PL_FIRST_CALL:
    { if ( PL_is_integer(code) )
      { int cp;
        if ( !PL_get_integer_ex(code, &cp) )
          return false;
        if ( cp < 0 || cp > 0x10FFFF )
          return false;
        size_t n = uniname_get((uint32_t)cp, buf, sizeof buf);
        if ( n == 0 )
          return false;
        return unify_name(name, buf, n);
      }

      if ( !PL_is_variable(name) )
      { char *s;
        size_t len;
        uint32_t cp;
        if ( !PL_get_nchars(name, &len, &s,
                            CVT_ATOM|CVT_STRING|CVT_LIST|
                            REP_UTF8|CVT_EXCEPTION) )
          return false;
        if ( !uniname_lookup(s, &cp) )
          return false;
        return PL_unify_integer(code, (int)cp);
      }

      it = malloc(sizeof *it);
      if ( !it )
        return PL_resource_error("memory");
      uniname_iter_init(it);
      goto redo;
    }

    case PL_REDO:
    { it = PL_foreign_context_address(h);
    redo:
      for(;;)
      { uint32_t cp;
        if ( !uniname_iter_next(it, &cp, buf, sizeof buf) )
        { free(it);
          return false;
        }
        fid_t fid = PL_open_foreign_frame();
        if ( PL_unify_integer(code, (int)cp) &&
             unify_name(name, buf, strlen(buf)) )
        { PL_close_foreign_frame(fid);
          PL_retry_address(it);
        }
        PL_discard_foreign_frame(fid);   /* undo, try next cp */
      }
    }

    case PL_PRUNED:
    { it = PL_foreign_context_address(h);
      free(it);
      return true;
    }

    default:
      return false;
  }
}

install_t
install_uniname4pl(void)
{ PL_register_foreign("unicode_name", 2, pl_unicode_name,
                      PL_FA_NONDETERMINISTIC);
}
