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
#include <utf8proc.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>
#include "uts39_data.h"

		 /*******************************
		 *	   ATOM INTERNING	*
		 *******************************/

/* Script atoms: long name, lowercased, '_' for spaces.  Built once at
 * install time so unicode_script/2 can return a shared atom_t.
 */
static atom_t *script_atom;            /* indexed by UTS39_SC_<short> */
static atom_t *idtype_atom;            /* indexed by UTS39_IDTYPE_<TYPE> */

static atom_t ATOM_allowed;

static atom_t ATOM_ignore_intentional;
static atom_t ATOM_true;
static atom_t ATOM_false;

static atom_t ATOM_ascii_only;
static atom_t ATOM_single_script;
static atom_t ATOM_highly_restrictive;
static atom_t ATOM_moderately_restrictive;
static atom_t ATOM_minimally_restrictive;
static atom_t ATOM_unrestricted;

static atom_t
script_long_atom(size_t id)
{ /* "Old_North_Arabian" -> "old_north_arabian" */
  const char *s = uts39_script_long[id];
  size_t len = strlen(s);
  char buf[64];
  if ( len >= sizeof(buf) )
    len = sizeof(buf) - 1;
  for(size_t i=0; i<len; i++)
    buf[i] = (char)tolower((unsigned char)s[i]);
  buf[len] = 0;
  return PL_new_atom_nchars(len, buf);
}

static atom_t
idtype_lc_atom(const char *name)
{ /* "Default_Ignorable" -> "default_ignorable" */
  size_t len = strlen(name);
  char buf[40];
  if ( len >= sizeof(buf) )
    len = sizeof(buf) - 1;
  for(size_t i=0; i<len; i++)
    buf[i] = (char)tolower((unsigned char)name[i]);
  buf[len] = 0;
  return PL_new_atom_nchars(len, buf);
}

/* Mirror of the enum order used by gen_uts39.pl: alphabetical by atom. */
static const char *const idtype_short_names[UTS39_IDTYPE_COUNT] = {
  [UTS39_IDTYPE_DEFAULT_IGNORABLE] = "Default_Ignorable",
  [UTS39_IDTYPE_DEPRECATED]        = "Deprecated",
  [UTS39_IDTYPE_EXCLUSION]         = "Exclusion",
  [UTS39_IDTYPE_INCLUSION]         = "Inclusion",
  [UTS39_IDTYPE_LIMITED_USE]       = "Limited_Use",
  [UTS39_IDTYPE_NOT_NFKC]          = "Not_NFKC",
  [UTS39_IDTYPE_NOT_XID]           = "Not_XID",
  [UTS39_IDTYPE_OBSOLETE]          = "Obsolete",
  [UTS39_IDTYPE_RECOMMENDED]       = "Recommended",
  [UTS39_IDTYPE_TECHNICAL]         = "Technical",
  [UTS39_IDTYPE_UNCOMMON_USE]      = "Uncommon_Use",
};

		 /*******************************
		 *	  RANGE LOOKUP		*
		 *******************************/

/* Binary search over a sorted, non-overlapping range table.  Each
 * range covers `start .. start + len - 1`.  Returns the value field
 * or -1 if cp is not in any range. */

static int
range_lookup(const uts39_range_t *r, size_t n, uint32_t cp)
{ size_t lo = 0, hi = n;
  while ( lo < hi )
  { size_t m = (lo + hi) >> 1;
    if ( cp < r[m].start )                  hi = m;
    else if ( cp >= r[m].start + r[m].len ) lo = m + 1;
    else                                    return r[m].value;
  }
  return -1;
}

static size_t
skeleton_lookup(uint32_t cp, size_t *out_len)
{ size_t lo = 0, hi = uts39_skeleton_entries_count;
  while ( lo < hi )
  { size_t m = (lo + hi) >> 1;
    uint32_t s = uts39_skeleton_entries[m].src;
    if ( cp < s )      hi = m;
    else if ( cp > s ) lo = m + 1;
    else { *out_len = uts39_skeleton_entries[m].length;
           return     uts39_skeleton_entries[m].offset; }
  }
  return (size_t)-1;
}

static bool
intentional_lookup(uint32_t a, uint32_t b)
{ /* Pairs are stored sorted; first canonicalize. */
  uint32_t lo = a < b ? a : b;
  uint32_t hi = a < b ? b : a;
  size_t l = 0, h = uts39_intentional_count;
  while ( l < h )
  { size_t m = (l + h) >> 1;
    const uts39_pair_t *p = &uts39_intentional[m];
    uint32_t pl = p->a < p->b ? p->a : p->b;
    uint32_t ph = p->a < p->b ? p->b : p->a;
    if ( lo < pl )                    h = m;
    else if ( lo > pl )               l = m + 1;
    else if ( hi < ph )               h = m;
    else if ( hi > ph )               l = m + 1;
    else                              return true;
  }
  return false;
}

		 /*******************************
		 *    AUGMENTED SCRIPT SETS	*
		 *******************************/

/* Cached IDs for the scripts referenced by the augmentation rules and
 * the restriction-level profiles.  Filled in install_unicode_security4pl().
 * -1 if the data version lacks the script (none of these have ever been
 * retired, but check anyway). */
static int sc_Hani = -1, sc_Bopo = -1, sc_Hira = -1, sc_Kana = -1, sc_Hang = -1;
static int sc_Hanb = -1, sc_Jpan = -1, sc_Kore = -1;
static int sc_Latn = -1, sc_Cyrl = -1, sc_Grek = -1;

#define WORDS UTS39_SCRIPT_BITSET_WORDS

static inline void
bs_zero(uint64_t *b)
{ memset(b, 0, sizeof(uint64_t)*WORDS); }

static inline void
bs_set(uint64_t *b, int bit)
{ b[bit >> 6] |= (uint64_t)1 << (bit & 63); }

static inline bool
bs_get(const uint64_t *b, int bit)
{ return (b[bit >> 6] >> (bit & 63)) & 1; }

static inline void
bs_copy(uint64_t *dst, const uint64_t *src)
{ memcpy(dst, src, sizeof(uint64_t)*WORDS); }

static inline void
bs_universal(uint64_t *b)
{ for(size_t i=0; i<WORDS; i++) b[i] = ~(uint64_t)0;
  /* clear bits past UTS39_SCRIPT_COUNT to keep counts clean */
  size_t tail = UTS39_SCRIPT_COUNT % 64;
  if ( tail ) b[WORDS-1] &= ((uint64_t)1 << tail) - 1;
}

static inline void
bs_and(uint64_t *dst, const uint64_t *src)
{ for(size_t i=0; i<WORDS; i++) dst[i] &= src[i]; }

static inline bool
bs_empty(const uint64_t *b)
{ for(size_t i=0; i<WORDS; i++) if (b[i]) return false;
  return true;
}

/* Fill `out` with the (un-augmented) Script_Extensions of cp.
 * For codepoints without an explicit scx entry, the set is {sc(cp)}.
 * For unassigned codepoints, the set is {Common}. */
static void
scx_of(uint32_t cp, uint64_t *out)
{ bs_zero(out);
  int sx = range_lookup(uts39_scx_ranges, uts39_scx_ranges_count, cp);
  if ( sx >= 0 )
  { bs_copy(out, uts39_scx_sets[sx]);
    return;
  }
  int s = range_lookup(uts39_script_ranges, uts39_script_ranges_count, cp);
  if ( s < 0 ) s = UTS39_SC_Zyyy;
  bs_set(out, s);
}

static void
augment_scx(uint64_t *b)
{ if ( sc_Hani >= 0 && bs_get(b, sc_Hani) )
  { if ( sc_Hanb >= 0 ) bs_set(b, sc_Hanb);
    if ( sc_Jpan >= 0 ) bs_set(b, sc_Jpan);
    if ( sc_Kore >= 0 ) bs_set(b, sc_Kore);
  }
  if ( sc_Hira >= 0 && bs_get(b, sc_Hira) )
  { if ( sc_Jpan >= 0 ) bs_set(b, sc_Jpan); }
  if ( sc_Kana >= 0 && bs_get(b, sc_Kana) )
  { if ( sc_Jpan >= 0 ) bs_set(b, sc_Jpan); }
  if ( sc_Hang >= 0 && bs_get(b, sc_Hang) )
  { if ( sc_Kore >= 0 ) bs_set(b, sc_Kore); }
  if ( sc_Bopo >= 0 && bs_get(b, sc_Bopo) )
  { if ( sc_Hanb >= 0 ) bs_set(b, sc_Hanb); }
}

/* Unify `b` (a bitset of script ids) with a Prolog list of script atoms. */
static bool
unify_script_set(term_t list, const uint64_t *b)
{ term_t tail = PL_copy_term_ref(list);
  term_t head = PL_new_term_ref();
  for(int i = 0; i < UTS39_SCRIPT_COUNT; i++)
  { if ( bs_get(b, i) )
    { if ( !PL_unify_list(tail, head, tail) ||
           !PL_unify_atom(head, script_atom[i]) )
        return false;
    }
  }
  return PL_unify_nil(tail);
}

		 /*******************************
		 *	PER-CP PREDICATES	*
		 *******************************/

/** unicode_script(+Code, -Script) is semidet.
 *
 * Fails for code points outside the Unicode range and for unassigned
 * code points (no entry in Scripts.txt).
 */
static foreign_t
pl_unicode_script(term_t code, term_t script)
{ int cp;
  if ( !PL_get_integer_ex(code, &cp) ) return false;
  if ( cp < 0 || cp > 0x10FFFF ) return false;
  int sid = range_lookup(uts39_script_ranges, uts39_script_ranges_count, cp);
  if ( sid < 0 ) return false;
  return PL_unify_atom(script, script_atom[sid]);
}

/** unicode_script_extensions(+Code, -Scripts) is semidet.
 *
 * Fails for code points outside the Unicode range and for code points
 * with no entry in either ScriptExtensions.txt or Scripts.txt
 * (unassigned).  Code points listed in Scripts.txt but not in
 * ScriptExtensions.txt resolve to a singleton `[Script]`.
 */
static foreign_t
pl_unicode_script_extensions(term_t code, term_t scripts)
{ int cp;
  if ( !PL_get_integer_ex(code, &cp) ) return false;
  if ( cp < 0 || cp > 0x10FFFF ) return false;
  uint64_t b[WORDS];
  int sx = range_lookup(uts39_scx_ranges,
                        uts39_scx_ranges_count, (uint32_t)cp);
  if ( sx >= 0 )
  { bs_copy(b, uts39_scx_sets[sx]);
  } else
  { int s = range_lookup(uts39_script_ranges,
                         uts39_script_ranges_count, (uint32_t)cp);
    if ( s < 0 ) return false;
    bs_zero(b);
    bs_set(b, s);
  }
  return unify_script_set(scripts, b);
}

/** unicode_identifier_status(+Code, -Status) is semidet.
 *
 * Succeeds when Code is listed as `Allowed` in
 * UTS #39 IdentifierStatus.txt, unifying Status with `allowed`.
 * Fails otherwise (out of Unicode range or not listed — per UTS #39
 * the default for unlisted code points is Restricted).
 */
static foreign_t
pl_unicode_identifier_status(term_t code, term_t status)
{ int cp;
  if ( !PL_get_integer_ex(code, &cp) ) return false;
  if ( cp < 0 || cp > 0x10FFFF ) return false;
  int v = range_lookup(uts39_idstatus_ranges,
                        uts39_idstatus_ranges_count, cp);
  if ( v != UTS39_ID_ALLOWED ) return false;
  return PL_unify_atom(status, ATOM_allowed);
}

/** unicode_identifier_type(+Code, -Types) is semidet.
 *
 * Types is the sorted list of Identifier_Type atoms for Code.  Fails
 * for code points outside the Unicode range and for code points with
 * no entry in IdentifierType.txt.
 */
static foreign_t
pl_unicode_identifier_type(term_t code, term_t types)
{ int cp;
  if ( !PL_get_integer_ex(code, &cp) ) return false;
  if ( cp < 0 || cp > 0x10FFFF ) return false;
  int bits = range_lookup(uts39_idtype_ranges,
                            uts39_idtype_ranges_count, cp);
  if ( bits < 0 ) return false;
  term_t tail = PL_copy_term_ref(types);
  term_t head = PL_new_term_ref();
  for(int i = 0; i < UTS39_IDTYPE_COUNT; i++)
  { if ( bits & (1 << i) )
    { if ( !PL_unify_list(tail, head, tail) ||
           !PL_unify_atom(head, idtype_atom[i]) )
        return false;
    }
  }
  return PL_unify_nil(tail);
}

		 /*******************************
		 *	    SKELETON		*
		 *******************************/

/* Decompose UTF-8 input via utf8proc.  Returns malloc()ed buffer the
 * caller must free.  Returns NULL on error; *len is set to length. */
static uint8_t *
nfd(const uint8_t *in, size_t inlen, size_t *outlen)
{ uint8_t *out = NULL;
  utf8proc_ssize_t r = utf8proc_map(in, (utf8proc_ssize_t)inlen, &out,
                                    UTF8PROC_DECOMPOSE|UTF8PROC_STABLE);
  if ( r < 0 ) { free(out); return NULL; }
  *outlen = (size_t)r;
  return out;
}

/* Build the per-codepoint substituted form: for each cp in `in`, append
 * its skeleton mapping (or the cp itself) to a growing UTF-8 buffer.
 * Honours ignore_intentional: if the skeleton mapping is a single cp
 * and (cp, target) is an intentional pair, the original cp is kept. */
static bool
substitute_skeleton(const uint8_t *in, size_t inlen, bool ignore_intentional,
                    uint8_t **outp, size_t *outlenp)
{ size_t cap = inlen * 2 + 16;
  uint8_t *out = (uint8_t*)malloc(cap);
  if ( !out ) return false;
  size_t off = 0;
  const uint8_t *s = in, *end = in + inlen;

  while ( s < end )
  { utf8proc_int32_t cp;
    utf8proc_ssize_t r = utf8proc_iterate(s, end - s, &cp);
    if ( r < 0 ) { free(out); return false; }

    size_t tlen = 0;
    size_t toff = skeleton_lookup((uint32_t)cp, &tlen);
    const uint32_t *tcps;
    size_t tn;
    if ( toff == (size_t)-1 )
    { /* No mapping — keep original */
      tcps = (const uint32_t*)&cp;
      tn = 1;
    } else if ( ignore_intentional && tlen == 1 &&
                intentional_lookup((uint32_t)cp, uts39_skeleton_chars[toff]) )
    { /* Skip this substitution: this is an intentional confusable */
      tcps = (const uint32_t*)&cp;
      tn = 1;
    } else
    { tcps = &uts39_skeleton_chars[toff];
      tn = tlen;
    }

    /* Encode each target cp to UTF-8 into `out`, growing as needed. */
    for(size_t i = 0; i < tn; i++)
    { uint8_t enc[4];
      utf8proc_ssize_t el = utf8proc_encode_char((utf8proc_int32_t)tcps[i], enc);
      if ( el <= 0 ) { free(out); return false; }
      if ( off + (size_t)el > cap )
      { while ( off + (size_t)el > cap ) cap *= 2;
        uint8_t *nb = (uint8_t*)realloc(out, cap);
        if ( !nb ) { free(out); return false; }
        out = nb;
      }
      memcpy(out + off, enc, (size_t)el);
      off += (size_t)el;
    }
    s += r;
  }
  *outp = out;
  *outlenp = off;
  return true;
}

/* Compute UTS #39 §4 skeleton: NFD, substitute confusables, NFD.
 * `out` is malloc()ed (free with free()).  Returns false on error. */
static bool
skeleton(const uint8_t *in, size_t inlen, bool ignore_intentional,
         uint8_t **outp, size_t *outlenp)
{ size_t n1len;
  uint8_t *n1 = nfd(in, inlen, &n1len);
  if ( !n1 ) return false;

  uint8_t *sub;
  size_t sublen;
  bool ok = substitute_skeleton(n1, n1len, ignore_intentional, &sub, &sublen);
  free(n1);
  if ( !ok ) return false;

  uint8_t *n2 = nfd(sub, sublen, outlenp);
  free(sub);
  if ( !n2 ) return false;
  *outp = n2;
  return true;
}

static bool
get_options_ignore_intentional(term_t opts, bool *flag)
{ *flag = false;
  if ( PL_skip_list(opts, 0, NULL) != PL_LIST )
    return PL_type_error("list", opts);
  term_t tail = PL_copy_term_ref(opts);
  term_t head = PL_new_term_ref();
  while ( PL_get_list(tail, head, tail) )
  { atom_t name;
    size_t arity;
    if ( PL_get_name_arity(head, &name, &arity) &&
         name == ATOM_ignore_intentional && arity == 1 )
    { term_t arg = PL_new_term_ref();
      _PL_get_arg(1, head, arg);
      atom_t v;
      if ( !PL_get_atom_ex(arg, &v) ) return false;
      if ( v == ATOM_true )       *flag = true;
      else if ( v == ATOM_false ) *flag = false;
      else return PL_domain_error("boolean", arg);
    }
  }
  return true;
}

/** unicode_skeleton(+Text, -Skeleton) is det. */
static foreign_t
pl_unicode_skeleton(term_t in, term_t out)
{ size_t inlen;
  char *utf8;
  if ( !PL_get_nchars(in, &inlen, &utf8,
                      CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION|REP_UTF8) )
    return false;
  uint8_t *sk;
  size_t sklen;
  if ( !skeleton((const uint8_t*)utf8, inlen, false, &sk, &sklen) )
    return PL_resource_error("memory");
  bool rc = PL_unify_chars(out, PL_ATOM|REP_UTF8, sklen, (char*)sk);
  free(sk);
  return rc;
}

static bool
skeleton_for_term(term_t t, bool ignore_intentional,
                  uint8_t **outp, size_t *outlenp)
{ size_t len;
  char *utf8;
  if ( !PL_get_nchars(t, &len, &utf8,
                      CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION|REP_UTF8) )
    return false;
  return skeleton((const uint8_t*)utf8, len, ignore_intentional, outp, outlenp);
}

/** unicode_confusable(+T1, +T2) is semidet. */
static foreign_t
pl_unicode_confusable2(term_t t1, term_t t2)
{ uint8_t *s1, *s2;
  size_t l1, l2;
  if ( !skeleton_for_term(t1, false, &s1, &l1) ) return false;
  if ( !skeleton_for_term(t2, false, &s2, &l2) ) { free(s1); return false; }
  bool eq = (l1 == l2) && memcmp(s1, s2, l1) == 0;
  free(s1); free(s2);
  return eq;
}

/** unicode_confusable(+T1, +T2, +Options) is semidet. */
static foreign_t
pl_unicode_confusable3(term_t t1, term_t t2, term_t opts)
{ bool ignore;
  if ( !get_options_ignore_intentional(opts, &ignore) ) return false;
  uint8_t *s1, *s2;
  size_t l1, l2;
  if ( !skeleton_for_term(t1, ignore, &s1, &l1) ) return false;
  if ( !skeleton_for_term(t2, ignore, &s2, &l2) ) { free(s1); return false; }
  bool eq = (l1 == l2) && memcmp(s1, s2, l1) == 0;
  free(s1); free(s2);
  return eq;
}

		 /*******************************
		 *	RESOLVED SCRIPTS	*
		 *******************************/

/* Compute the resolved augmented script set (UTS #39 §5.1) over `text`.
 * Returns false on UTF-8 decode error; sets *empty = true if the
 * intersection ended up empty (mixed-script).  Otherwise fills `out`
 * with the survivors. */
static bool
resolved_scripts(const uint8_t *text, size_t len, uint64_t *out, bool *empty)
{ uint64_t cur[WORDS], scx[WORDS];
  bs_universal(cur);
  bool first = true;
  const uint8_t *s = text, *end = text + len;
  while ( s < end )
  { utf8proc_int32_t cp;
    utf8proc_ssize_t r = utf8proc_iterate(s, end - s, &cp);
    if ( r < 0 ) return false;
    scx_of((uint32_t)cp, scx);
    bool is_common    = bs_get(scx, UTS39_SC_Zyyy);
    bool is_inherited = bs_get(scx, UTS39_SC_Zinh);
    if ( !is_common && !is_inherited )
    { augment_scx(scx);
      if ( first ) { bs_copy(cur, scx); first = false; }
      else         { bs_and(cur, scx); }
    }
    s += r;
  }
  if ( first )                 /* all chars were Common/Inherited */
    bs_zero(cur);              /* no scripts identified */
  bs_copy(out, cur);
  *empty = bs_empty(cur);
  return true;
}

/** unicode_resolved_scripts(+Text, -Scripts) is det. */
static foreign_t
pl_unicode_resolved_scripts(term_t in, term_t out)
{ size_t inlen;
  char *utf8;
  if ( !PL_get_nchars(in, &inlen, &utf8,
                      CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION|REP_UTF8) )
    return false;
  uint64_t b[WORDS];
  bool empty;
  if ( !resolved_scripts((const uint8_t*)utf8, inlen, b, &empty) )
    return PL_domain_error("utf8_string", in);
  return unify_script_set(out, b);
}

		 /*******************************
		 *	RESTRICTION LEVEL	*
		 *******************************/

/* UTS #39 §5.2 — restriction levels, assigned at the most restrictive
 * level for which the text qualifies.
 *
 *   ascii_only             all in U+0020..U+007E and Allowed
 *   single_script          augmented Resolved Script Set non-empty
 *   highly_restrictive     covers Latn + (Hanb | Jpan | Kore)
 *   moderately_restrictive covers Latn + (Cyrl | Grek)
 *   minimally_restrictive  all chars Identifier_Type ⊆
 *                          {Recommended, Inclusion}
 *   unrestricted           otherwise
 *
 * "Covers profile P" = for every non-Common / non-Inherited character,
 * augmented Script_Extensions ∩ P ≠ ∅.  The single_script / highly /
 * moderately levels additionally require all characters Allowed. */

static bool
covers_profile(const uint8_t *text, size_t len, const uint64_t *profile)
{ const uint8_t *s = text, *end = text + len;
  uint64_t scx[WORDS];
  while ( s < end )
  { utf8proc_int32_t cp;
    utf8proc_ssize_t r = utf8proc_iterate(s, end - s, &cp);
    if ( r < 0 ) return false;
    scx_of((uint32_t)cp, scx);
    if ( !bs_get(scx, UTS39_SC_Zyyy) && !bs_get(scx, UTS39_SC_Zinh) )
    { augment_scx(scx);
      uint64_t tmp[WORDS]; bs_copy(tmp, scx); bs_and(tmp, profile);
      if ( bs_empty(tmp) ) return false;
    }
    s += r;
  }
  return true;
}

static void
profile_pair(uint64_t *p, int a, int b)
{ bs_zero(p);
  if ( a >= 0 ) bs_set(p, a);
  if ( b >= 0 ) bs_set(p, b);
}

/* (Old block-comment removed in this revision.)
 *
 * A condensed UTS #39 §5.2 implementation.
 *
 * ascii_only:             all chars in [
 * single_script:          RSS non-empty AND all chars Allowed
 * highly_restrictive:     RSS ⊆ {Latn, Hanb, Jpan, Kore} (Latin plus
 *                         one CJK augmented script) AND all Allowed
 * moderately_restrictive: like above plus a single recommended script
 *                         from {Cyrl, Grek} AND all Allowed
 * minimally_restrictive:  all chars have Identifier_Type ⊆ {Recommended,
 *                         Inclusion}
 * unrestricted:           otherwise
 *
 * The check walks the text once and computes everything in one pass. */

static int
classify_text(const uint8_t *text, size_t len, atom_t *level)
{ const uint8_t *s = text, *end = text + len;
  bool only_ascii = true;
  bool all_allowed = true;
  bool all_recommended_or_inclusion = true;
  uint64_t rss[WORDS], scx[WORDS];
  bs_universal(rss);
  bool first = true;
  int allowed_types = (1 << UTS39_IDTYPE_RECOMMENDED) |
                      (1 << UTS39_IDTYPE_INCLUSION);

  while ( s < end )
  { utf8proc_int32_t cp;
    utf8proc_ssize_t r = utf8proc_iterate(s, end - s, &cp);
    if ( r < 0 ) return -1;
    if ( cp > 0x7F ) only_ascii = false;

    int st = range_lookup(uts39_idstatus_ranges,
                           uts39_idstatus_ranges_count, (uint32_t)cp);
    if ( st != UTS39_ID_ALLOWED ) all_allowed = false;

    int bits = range_lookup(uts39_idtype_ranges,
                              uts39_idtype_ranges_count, (uint32_t)cp);
    if ( bits < 0 ) bits = 0;
    if ( bits == 0 || (bits & ~allowed_types) != 0 )
      all_recommended_or_inclusion = false;

    scx_of((uint32_t)cp, scx);
    if ( !bs_get(scx, UTS39_SC_Zyyy) && !bs_get(scx, UTS39_SC_Zinh) )
    { augment_scx(scx);
      if ( first ) { bs_copy(rss, scx); first = false; }
      else         { bs_and(rss, scx); }
    }
    s += r;
  }
  if ( first ) bs_zero(rss);

  if ( only_ascii && all_allowed )
  { *level = ATOM_ascii_only; return 0; }

  if ( all_allowed )
  { if ( !bs_empty(rss) )
    { *level = ATOM_single_script; return 0; }

    uint64_t p[WORDS];
    profile_pair(p, sc_Latn, sc_Hanb);
    if ( covers_profile(text, len, p) )
    { *level = ATOM_highly_restrictive; return 0; }
    profile_pair(p, sc_Latn, sc_Jpan);
    if ( covers_profile(text, len, p) )
    { *level = ATOM_highly_restrictive; return 0; }
    profile_pair(p, sc_Latn, sc_Kore);
    if ( covers_profile(text, len, p) )
    { *level = ATOM_highly_restrictive; return 0; }

    profile_pair(p, sc_Latn, sc_Cyrl);
    if ( covers_profile(text, len, p) )
    { *level = ATOM_moderately_restrictive; return 0; }
    profile_pair(p, sc_Latn, sc_Grek);
    if ( covers_profile(text, len, p) )
    { *level = ATOM_moderately_restrictive; return 0; }
  }

  if ( all_recommended_or_inclusion )
  { *level = ATOM_minimally_restrictive; return 0; }

  *level = ATOM_unrestricted; return 0;
}

/** unicode_restriction_level(+Text, -Level) is det. */
static foreign_t
pl_unicode_restriction_level(term_t in, term_t out)
{ size_t inlen;
  char *utf8;
  if ( !PL_get_nchars(in, &inlen, &utf8,
                      CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION|REP_UTF8) )
    return false;
  atom_t lvl;
  if ( classify_text((const uint8_t*)utf8, inlen, &lvl) != 0 )
    return PL_domain_error("utf8_string", in);
  return PL_unify_atom(out, lvl);
}

		 /*******************************
		 *	  INSTALLATION		*
		 *******************************/

static int
find_script(const char *short_name)
{ for(int i = 0; i < UTS39_SCRIPT_COUNT; i++)
    if ( strcmp(uts39_script_short[i], short_name) == 0 ) return i;
  return -1;
}

install_t
install_unicode_security4pl(void)
{ script_atom = (atom_t*)PL_malloc(UTS39_SCRIPT_COUNT * sizeof(atom_t));
  for(int i = 0; i < UTS39_SCRIPT_COUNT; i++)
    script_atom[i] = script_long_atom(i);

  idtype_atom = (atom_t*)PL_malloc(UTS39_IDTYPE_COUNT * sizeof(atom_t));
  for(int i = 0; i < UTS39_IDTYPE_COUNT; i++)
    idtype_atom[i] = idtype_lc_atom(idtype_short_names[i]);

  sc_Hani = find_script("Hani");
  sc_Bopo = find_script("Bopo");
  sc_Hira = find_script("Hira");
  sc_Kana = find_script("Kana");
  sc_Hang = find_script("Hang");
  sc_Hanb = find_script("Hanb");
  sc_Jpan = find_script("Jpan");
  sc_Kore = find_script("Kore");
  sc_Latn = find_script("Latn");
  sc_Cyrl = find_script("Cyrl");
  sc_Grek = find_script("Grek");

  ATOM_allowed    = PL_new_atom("allowed");
  ATOM_ignore_intentional = PL_new_atom("ignore_intentional");
  ATOM_true  = PL_new_atom("true");
  ATOM_false = PL_new_atom("false");
  ATOM_ascii_only             = PL_new_atom("ascii_only");
  ATOM_single_script          = PL_new_atom("single_script");
  ATOM_highly_restrictive     = PL_new_atom("highly_restrictive");
  ATOM_moderately_restrictive = PL_new_atom("moderately_restrictive");
  ATOM_minimally_restrictive  = PL_new_atom("minimally_restrictive");
  ATOM_unrestricted           = PL_new_atom("unrestricted");

  PL_register_foreign("unicode_script",              2,
                      pl_unicode_script,              0);
  PL_register_foreign("unicode_script_extensions",   2,
                      pl_unicode_script_extensions,   0);
  PL_register_foreign("unicode_identifier_status",   2,
                      pl_unicode_identifier_status,   0);
  PL_register_foreign("unicode_identifier_type",     2,
                      pl_unicode_identifier_type,     0);
  PL_register_foreign("unicode_skeleton",            2,
                      pl_unicode_skeleton,            0);
  PL_register_foreign("unicode_confusable",          2,
                      pl_unicode_confusable2,         0);
  PL_register_foreign("unicode_confusable",          3,
                      pl_unicode_confusable3,         0);
  PL_register_foreign("unicode_resolved_scripts",    2,
                      pl_unicode_resolved_scripts,    0);
  PL_register_foreign("unicode_restriction_level",   2,
                      pl_unicode_restriction_level,   0);
}
