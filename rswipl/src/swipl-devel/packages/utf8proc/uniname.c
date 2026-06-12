/* Compact Unicode code-point <-> name lookup (prototype).
 *
 * Decoder for the tables emitted by etc/gen_uniname.pl.  Optimised for
 * size, not speed: name -> code point and the per-id word index are
 * linear scans.  See uniname_data.h for the on-disk format.
 *
 * Build the size / self-test harness with:
 *   cc -O2 -DUNINAME_TEST uniname.c uniname_data.c -o /tmp/uniname
 *   /tmp/uniname
 */

#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "uniname.h"

/* --- Hangul algorithmic names (Unicode 3.12) ------------------------- */

#define HANGUL_SBASE  0xAC00
#define HANGUL_LCOUNT 19
#define HANGUL_VCOUNT 21
#define HANGUL_TCOUNT 28
#define HANGUL_NCOUNT (HANGUL_VCOUNT*HANGUL_TCOUNT)          /* 588   */
#define HANGUL_SCOUNT (HANGUL_LCOUNT*HANGUL_NCOUNT)          /* 11172 */

static const char *const hangul_l[HANGUL_LCOUNT] = {
  "G","GG","N","D","DD","R","M","B","BB","S","SS","","J","JJ",
  "C","K","T","P","H" };
static const char *const hangul_v[HANGUL_VCOUNT] = {
  "A","AE","YA","YAE","EO","E","YEO","YE","O","WA","WAE","OE","YO",
  "U","WEO","WE","WI","YU","EU","YI","I" };
static const char *const hangul_t[HANGUL_TCOUNT] = {
  "","G","GG","GS","N","NJ","NH","D","L","LG","LM","LB","LS","LT",
  "LP","LH","M","B","BS","S","SS","NG","J","C","K","T","P","H" };

/* --- small helpers --------------------------------------------------- */

static const char *
str_at(const char *blob, unsigned idx)
{ const char *p = blob;
  while ( idx-- )
    p += strlen(p) + 1;
  return p;
}

/* Lazily built id -> word pointer index over uniname_words. */
static const char *word_ptr[UNINAME_WORD_COUNT];
static int word_ptr_ready = 0;

static void
build_word_index(void)
{ const char *p = uniname_words;
  for (unsigned i = 0; i < UNINAME_WORD_COUNT; i++)
  { word_ptr[i] = p;
    p += strlen(p) + 1;
  }
  word_ptr_ready = 1;
}

static uint32_t
leb_get(const uint8_t **pp)
{ uint32_t v = 0;
  int shift = 0;
  const uint8_t *p = *pp;
  for (;;)
  { uint8_t b = *p++;
    v |= (uint32_t)(b & 0x7f) << shift;
    if ( !(b & 0x80) )
      break;
    shift += 7;
  }
  *pp = p;
  return v;
}

static int
range_lookup(uint32_t cp, const uniname_range_t **rp)
{ int lo = 0, hi = UNINAME_RANGE_COUNT - 1;
  while ( lo <= hi )
  { int mid = (lo + hi) / 2;
    const uniname_range_t *r = &uniname_ranges[mid];
    if ( cp < r->start )      hi = mid - 1;
    else if ( cp > r->end )   lo = mid + 1;
    else { *rp = r; return 1; }
  }
  return 0;
}

/* --- public: code point -> name ------------------------------------- */

/* Writes the name of `cp` into buf (NUL-terminated) and returns its
 * length, or 0 if `cp` has no Unicode name (controls, surrogates,
 * private use, unassigned).
 */
size_t
uniname_get(uint32_t cp, char *buf, size_t buflen)
{ const uniname_range_t *r;
  size_t n = 0;

  if ( buflen == 0 )
    return 0;

  if ( range_lookup(cp, &r) )
  { if ( r->kind == UNINAME_K_HEX )
    { const char *pfx = str_at(uniname_prefixes, r->prefix);
      n = (size_t)snprintf(buf, buflen, "%s-%04X", pfx, cp);
    } else /* UNINAME_K_HANGUL */
    { uint32_t s = cp - HANGUL_SBASE;
      uint32_t l = s / HANGUL_NCOUNT;
      uint32_t v = (s % HANGUL_NCOUNT) / HANGUL_TCOUNT;
      uint32_t t = s % HANGUL_TCOUNT;
      n = (size_t)snprintf(buf, buflen, "HANGUL SYLLABLE %s%s%s",
                           hangul_l[l], hangul_v[v], hangul_t[t]);
    }
    return n < buflen ? n : 0;
  }

  if ( !word_ptr_ready )
    build_word_index();

  const uint8_t *p = uniname_names;
  const uint8_t *end = uniname_names + UNINAME_NAMES_LEN;
  uint32_t cur = 0;
  while ( p < end )
  { cur += leb_get(&p);
    if ( cur == cp )
    { int first = 1;
      while ( *p )
      { uint32_t id = leb_get(&p) - 1;
        const char *w = word_ptr[id];
        size_t wl = strlen(w);
        if ( !first && n + 1 < buflen ) buf[n++] = ' ';
        first = 0;
        if ( n + wl < buflen ) { memcpy(buf + n, w, wl); n += wl; }
        else return 0;
      }
      buf[n] = '\0';
      return n;
    }
    while ( *p ) leb_get(&p);    /* skip this name's tokens */
    p++;                          /* skip 0 terminator       */
  }
  return 0;
}

/* --- public: name -> code point ------------------------------------- */

/* Reverse lookup.  Linear scan of the explicit-name stream plus a
 * structural check of the algorithmic ranges.  Returns 1 and sets *cp
 * on success, 0 otherwise.  Unicode names are unique, so this is
 * semidet.
 */
int
uniname_lookup(const char *name, uint32_t *cp)
{ char buf[256];

  /* Algorithmic ranges. */
  for (unsigned i = 0; i < UNINAME_RANGE_COUNT; i++)
  { const uniname_range_t *r = &uniname_ranges[i];
    if ( r->kind == UNINAME_K_HEX )
    { const char *pfx = str_at(uniname_prefixes, r->prefix);
      size_t pl = strlen(pfx);
      if ( strncmp(name, pfx, pl) == 0 && name[pl] == '-' )
      { char *e;
        unsigned long v = strtoul(name + pl + 1, &e, 16);
        if ( *e == '\0' && v >= r->start && v <= r->end )
        { snprintf(buf, sizeof buf, "%s-%04lX", pfx, v);
          if ( strcmp(buf, name) == 0 ) { *cp = (uint32_t)v; return 1; }
        }
      }
    } else /* UNINAME_K_HANGUL: brute the 11k block (reverse is rare) */
    { if ( strncmp(name, "HANGUL SYLLABLE ", 16) == 0 )
      { for (uint32_t c = r->start; c <= r->end; c++)
        { uniname_get(c, buf, sizeof buf);
          if ( strcmp(buf, name) == 0 ) { *cp = c; return 1; }
        }
      }
    }
  }

  /* Explicit, tokenised names. */
  { const uint8_t *p = uniname_names;
    const uint8_t *end = uniname_names + UNINAME_NAMES_LEN;
    uint32_t cur = 0;
    if ( !word_ptr_ready ) build_word_index();
    while ( p < end )
    { cur += leb_get(&p);
      size_t n = 0;
      int first = 1;
      while ( *p )
      { uint32_t id = leb_get(&p) - 1;
        const char *w = word_ptr[id];
        size_t wl = strlen(w);
        if ( !first ) buf[n++] = ' ';
        first = 0;
        memcpy(buf + n, w, wl); n += wl;
      }
      buf[n] = '\0';
      p++;
      if ( strcmp(buf, name) == 0 ) { *cp = cur; return 1; }
    }
  }
  return 0;
}

/* --- public: enumeration ------------------------------------------- */

/* Stateful iterator over every named code point.  Phase 0 walks the
 * explicit-name stream (ascending cp); phase 1 expands the algorithmic
 * ranges.  uniname_iter_next() returns 1 with *cp and the name in buf,
 * or 0 when exhausted.
 */
void
uniname_iter_init(uniname_iter *it)
{ it->p = uniname_names;
  it->cur = 0;
  it->ri = 0;
  it->rc = UNINAME_RANGE_COUNT ? uniname_ranges[0].start : 0;
  it->phase = 0;
}

int
uniname_iter_next(uniname_iter *it, uint32_t *cp, char *buf, size_t buflen)
{ if ( it->phase == 0 )
  { const uint8_t *end = uniname_names + UNINAME_NAMES_LEN;
    if ( it->p < end )
    { if ( !word_ptr_ready ) build_word_index();
      it->cur += leb_get(&it->p);
      size_t n = 0;
      int first = 1;
      while ( *it->p )
      { uint32_t id = leb_get(&it->p) - 1;
        const char *w = word_ptr[id];
        size_t wl = strlen(w);
        if ( !first && n + 1 < buflen ) buf[n++] = ' ';
        first = 0;
        if ( n + wl < buflen ) { memcpy(buf + n, w, wl); n += wl; }
      }
      buf[n] = '\0';
      it->p++;
      *cp = it->cur;
      return 1;
    }
    it->phase = 1;
  }
  while ( it->ri < UNINAME_RANGE_COUNT )
  { const uniname_range_t *r = &uniname_ranges[it->ri];
    if ( it->rc <= r->end )
    { uint32_t c = it->rc++;
      uniname_get(c, buf, buflen);
      *cp = c;
      return 1;
    }
    if ( ++it->ri < UNINAME_RANGE_COUNT )
      it->rc = uniname_ranges[it->ri].start;
  }
  return 0;
}

/* --- size / self-test harness --------------------------------------- */

#ifdef UNINAME_TEST
struct { uint32_t cp; const char *name; } cases[] = {
  { 0x0041, "LATIN CAPITAL LETTER A" },
  { 0x00E9, "LATIN SMALL LETTER E WITH ACUTE" },
  { 0x03B1, "GREEK SMALL LETTER ALPHA" },
  { 0x20AC, "EURO SIGN" },
  { 0x27E8, "MATHEMATICAL LEFT ANGLE BRACKET" },
  { 0x4E2D, "CJK UNIFIED IDEOGRAPH-4E2D" },     /* algorithmic */
  { 0xAC00, "HANGUL SYLLABLE GA" },             /* algorithmic */
  { 0xD55C, "HANGUL SYLLABLE HAN" },            /* algorithmic */
  { 0xF900, "CJK COMPATIBILITY IDEOGRAPH-F900" },/* hex range   */
  { 0x13000,"EGYPTIAN HIEROGLYPH A001" },       /* explicit    */
  { 0x2F800,"CJK COMPATIBILITY IDEOGRAPH-2F800" },/* hex range */
  { 0x1F600,"GRINNING FACE" },
  { 0x10FFFF, NULL },                           /* no name     */
};

/* Full-coverage check: stream UnicodeData.txt and compare uniname_get()
 * for every explicitly named code point (skipping <...> markers).
 */
static int
verify_ucd(const char *path)
{ FILE *f = fopen(path, "r");
  char line[512], buf[256];
  long checked = 0, bad = 0;
  if ( !f ) { perror(path); return 2; }
  while ( fgets(line, sizeof line, f) )
  { char *semi = strchr(line, ';');
    if ( !semi ) continue;
    *semi = 0;
    char *name = semi + 1;
    char *semi2 = strchr(name, ';');
    if ( semi2 ) *semi2 = 0;
    if ( name[0] == '<' ) continue;          /* control/range marker */
    uint32_t cp = (uint32_t)strtoul(line, NULL, 16);
    size_t n = uniname_get(cp, buf, sizeof buf);
    checked++;
    if ( n == 0 || strcmp(buf, name) != 0 )
    { if ( bad < 10 )
        fprintf(stderr, "MISMATCH U+%04X: got \"%s\" want \"%s\"\n",
                cp, n ? buf : "(none)", name);
      bad++;
    }
  }
  fclose(f);
  printf("verify: %ld checked, %ld mismatches\n", checked, bad);
  return bad ? 1 : 0;
}

int
main(int argc, char **argv)
{ char buf[256];
  int fail = 0;

  if ( argc == 3 && strcmp(argv[1], "--verify") == 0 )
    return verify_ucd(argv[2]);

  for (size_t i = 0; i < sizeof(cases)/sizeof(cases[0]); i++)
  { size_t n = uniname_get(cases[i].cp, buf, sizeof buf);
    const char *got = n ? buf : "(none)";
    const char *exp = cases[i].name ? cases[i].name : "(none)";
    int ok = strcmp(got, exp) == 0;
    if ( !ok ) fail++;
    printf("%s U+%04X  %-34s %s\n",
           ok ? "ok  " : "FAIL", cases[i].cp, got,
           ok ? "" : exp);
  }

  size_t words  = sizeof uniname_words;
  size_t pfx    = strlen(uniname_prefixes)+1; /* approx; multi-string */
  size_t ranges = sizeof uniname_ranges;
  size_t names  = sizeof uniname_names;
  size_t total  = words + ranges + names;     /* + prefixes blob */
  printf("\nstatic tables:\n");
  printf("  uniname_words   : %8zu bytes\n", words);
  printf("  uniname_ranges  : %8zu bytes\n", ranges);
  printf("  uniname_names   : %8zu bytes\n", names);
  printf("  TOTAL (>=)      : %8zu bytes (%.1f KB)\n",
         total, total/1024.0);
  (void)pfx;
  return fail ? 1 : 0;
}
#endif
