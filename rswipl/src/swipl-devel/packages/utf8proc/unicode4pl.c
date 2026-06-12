/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2010-2026, VU University, Amsterdam
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

#include <config.h>
#include <SWI-Prolog.h>
#include <utf8proc.h>
#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

static atom_t ATOM_stable;
static atom_t ATOM_compat;
static atom_t ATOM_compose;
static atom_t ATOM_decompose;
static atom_t ATOM_ignore;
static atom_t ATOM_rejectna;
static atom_t ATOM_nlf2ls;
static atom_t ATOM_nlf2ps;
static atom_t ATOM_nlf2lf;
static atom_t ATOM_stripcc;
static atom_t ATOM_casefold;
static atom_t ATOM_charbound;
static atom_t ATOM_lump;
static atom_t ATOM_stripmark;

static atom_t ATOM_category;
static atom_t ATOM_combining_class;
static atom_t ATOM_bidi_class;
#ifdef HAVE_UTF8PROC_BIDI_MIRRORED
static atom_t ATOM_bidi_mirrored;
#endif
static atom_t ATOM_decomp_type;
static atom_t ATOM_ignorable;
static atom_t ATOM_boundclass;
#ifdef HAVE_UTF8PROC_CHARWIDTH
static atom_t ATOM_width;
#endif
#ifdef HAVE_UTF8PROC_AMBIGUOUS_WIDTH
static atom_t ATOM_ambiguous_width;
#endif
#ifdef HAVE_UTF8PROC_TOUPPER
static atom_t ATOM_uppercase;
#endif
#ifdef HAVE_UTF8PROC_TOLOWER
static atom_t ATOM_lowercase;
#endif
#ifdef HAVE_UTF8PROC_TOTITLE
static atom_t ATOM_titlecase;
#endif
#ifdef HAVE_UTF8PROC_INDIC_CONJUNCT_BREAK
static atom_t ATOM_indic_conjunct_break;
#endif


/** unicode_property(?Code, ?Property)
*/

typedef struct pmap
{ int		code;
  const char   *name;
  atom_t	atom;
} pmap;

#define UNIFY_SYMBOL(t, c, map) \
	unify_symbol(t, c, map ## _map, sizeof(map ## _map)/sizeof(pmap))

#define CAT(n) { UTF8PROC_CATEGORY_ ## n, #n, 0 }
static pmap category_map[] =
{ CAT(LU),
  CAT(LL),
  CAT(LT),
  CAT(LM),
  CAT(LO),
  CAT(MN),
  CAT(MC),
  CAT(ME),
  CAT(ND),
  CAT(NL),
  CAT(NO),
  CAT(PC),
  CAT(PD),
  CAT(PS),
  CAT(PE),
  CAT(PI),
  CAT(PF),
  CAT(PO),
  CAT(SM),
  CAT(SC),
  CAT(SK),
  CAT(SO),
  CAT(ZS),
  CAT(ZL),
  CAT(ZP),
  CAT(CC),
  CAT(CF),
  CAT(CS),
  CAT(CO),
  CAT(CN),
  { 0 }
};

#define BIDI(n) { UTF8PROC_BIDI_CLASS_ ## n, #n, 0 }
static pmap bidi_map[] =
{ BIDI(L),
  BIDI(LRE),
  BIDI(LRO),
  BIDI(R),
  BIDI(AL),
  BIDI(RLE),
  BIDI(RLO),
  BIDI(PDF),
  BIDI(EN),
  BIDI(ES),
  BIDI(ET),
  BIDI(AN),
  BIDI(CS),
  BIDI(NSM),
  BIDI(BN),
  BIDI(B),
  BIDI(S),
  BIDI(WS),
  BIDI(ON),
  { 0 }
};

#define DECOMP(n) { UTF8PROC_DECOMP_TYPE_ ## n, #n, 0 }
static pmap decomp_map[] =
{ DECOMP(FONT),
  DECOMP(NOBREAK),
  DECOMP(INITIAL),
  DECOMP(MEDIAL),
  DECOMP(FINAL),
  DECOMP(ISOLATED),
  DECOMP(CIRCLE),
  DECOMP(SUPER),
  DECOMP(SUB),
  DECOMP(VERTICAL),
  DECOMP(WIDE),
  DECOMP(NARROW),
  DECOMP(SMALL),
  DECOMP(SQUARE),
  DECOMP(FRACTION),
  DECOMP(COMPAT),
  { 0 }
};


#define BOUNDCLASS(n) { UTF8PROC_BOUNDCLASS_ ## n, #n, 0 }
static pmap boundclass_map[] =
{ BOUNDCLASS(START),
  BOUNDCLASS(OTHER),
  BOUNDCLASS(CR),
  BOUNDCLASS(LF),
  BOUNDCLASS(CONTROL),
  BOUNDCLASS(EXTEND),
  BOUNDCLASS(L),
  BOUNDCLASS(V),
  BOUNDCLASS(T),
  BOUNDCLASS(LV),
  BOUNDCLASS(LVT),
  BOUNDCLASS(REGIONAL_INDICATOR),
  BOUNDCLASS(SPACINGMARK),
  BOUNDCLASS(PREPEND),
  BOUNDCLASS(ZWJ),
#ifdef HAVE_UTF8PROC_BOUNDCLASS_E_BASE
  BOUNDCLASS(E_BASE),
  BOUNDCLASS(E_MODIFIER),
  BOUNDCLASS(GLUE_AFTER_ZWJ),
  BOUNDCLASS(E_BASE_GAZ),
#endif
#ifdef HAVE_UTF8PROC_BOUNDCLASS_EXTENDED_PICTOGRAPHIC
  BOUNDCLASS(EXTENDED_PICTOGRAPHIC),
  BOUNDCLASS(E_ZWG),
#endif
  { 0 }
};


#ifdef HAVE_UTF8PROC_INDIC_CONJUNCT_BREAK
#define ICB(n) { UTF8PROC_INDIC_CONJUNCT_BREAK_ ## n, #n, 0 }
static pmap indic_conjunct_break_map[] =
{ ICB(NONE),
  ICB(LINKER),
  ICB(CONSONANT),
  ICB(EXTEND),
  { 0 }
};
#endif


/* Convert an integer property value (from the utf8proc property struct)
 * into its Prolog atom (e.g. UTF8PROC_CATEGORY_LU → 'lu').  For the
 * category map the atom is constructed by lower-casing only the second
 * letter (so "Lu", "Ll", ...).  We also accept a one-letter "L", "N",
 * ... shorthand that matches every subcategory starting with that
 * capital initial.
 */
static bool
unify_symbol(term_t arg, int code, pmap *map, size_t size)
{ pmap *m;

  /* `indic_conjunct_break_map` maps zero to `none`, so do not reject
   * it as an unset property. */
  if ( code == 0
#ifdef HAVE_UTF8PROC_INDIC_CONJUNCT_BREAK
       && map != indic_conjunct_break_map
#endif
     )
    return false;

  if ( code < size && map[code].code == code )
  { m = &map[code];
  } else
  { m = map;
    pmap *end = &m[size];
    for(; map < end && m->code != code; m++)
      ;
    if ( map == end || !m->name )
      return false;
  }

  if ( !m->atom )
  { char buf[32];
    const char *s;
    char *o;

    if ( map == category_map )
    { buf[0] = m->name[0];
      buf[1] = (char) tolower((unsigned char)m->name[1]);
      buf[2] = 0;
    } else
    { for(s=m->name, o=buf; *s; s++)
	*o++ = (char) tolower((unsigned char)*s);
      *o = 0;
    }

    m->atom = PL_new_atom(buf);
  }

  if ( PL_unify_atom(arg, m->atom) )
    return true;

  if ( map == category_map )
  { char *s;
    size_t len;

    if ( PL_get_atom_nchars(arg, &len, &s) && len == 1 &&
	 m->name[0] == s[0] )
      return true;
  }

  return false;
}


static foreign_t
unicode_property(term_t code, term_t property, term_t silent)
{ int32_t uc;
  const utf8proc_property_t *p;
  atom_t pname;
  size_t parity;
  term_t arg = PL_new_term_ref();
  int quiet = false;

  if ( !PL_get_bool_ex(silent, &quiet) )
    return false;

  if ( !PL_get_integer(code, &uc) )
  { wchar_t *ws;
    size_t len;

    if ( PL_get_wchars(code, &len, &ws, CVT_ATOM) &&
	 len == 1 )
    { uc = ws[0];
    } else
      return PL_type_error("code", code);
  }

  if ( uc < 0 || uc > 0x10FFFF )
    return PL_domain_error("code", code);

  p = utf8proc_get_property(uc);
  if ( !p->category )
    return false;

  if ( !PL_get_name_arity(property, &pname, &parity) ||
       parity != 1 )
    return PL_type_error("compound", property);
  _PL_get_arg(1, property, arg);

  if ( pname == ATOM_category )
    return UNIFY_SYMBOL(arg, p->category, category);
  else if ( pname == ATOM_combining_class )
    return PL_unify_integer(arg, p->combining_class);
  else if ( pname == ATOM_bidi_class )
    return UNIFY_SYMBOL(arg, p->bidi_class, bidi);
#ifdef HAVE_UTF8PROC_BIDI_MIRRORED
  else if ( pname == ATOM_bidi_mirrored )
    return PL_unify_bool(arg, p->bidi_mirrored);
#endif
  else if ( pname == ATOM_decomp_type )
    return UNIFY_SYMBOL(arg, p->decomp_type, decomp);
  else if ( pname == ATOM_ignorable )
    return PL_unify_bool(arg, p->ignorable);
  else if ( pname == ATOM_boundclass )
    return UNIFY_SYMBOL(arg, p->boundclass, boundclass);
#ifdef HAVE_UTF8PROC_CHARWIDTH
  else if ( pname == ATOM_width )
    return PL_unify_integer(arg, p->charwidth);
#endif
#ifdef HAVE_UTF8PROC_AMBIGUOUS_WIDTH
  else if ( pname == ATOM_ambiguous_width )
    return PL_unify_bool(arg, p->ambiguous_width);
#endif
#ifdef HAVE_UTF8PROC_INDIC_CONJUNCT_BREAK
  else if ( pname == ATOM_indic_conjunct_break )
    return UNIFY_SYMBOL(arg, p->indic_conjunct_break,
			indic_conjunct_break);
#endif
#ifdef HAVE_UTF8PROC_TOUPPER
  else if ( pname == ATOM_uppercase )
  { int32_t m = utf8proc_toupper(uc);
    return m != uc && PL_unify_integer(arg, m);
  }
#endif
#ifdef HAVE_UTF8PROC_TOLOWER
  else if ( pname == ATOM_lowercase )
  { int32_t m = utf8proc_tolower(uc);
    return m != uc && PL_unify_integer(arg, m);
  }
#endif
#ifdef HAVE_UTF8PROC_TOTITLE
  else if ( pname == ATOM_titlecase )
  { int32_t m = utf8proc_totitle(uc);
    return m != uc && PL_unify_integer(arg, m);
  }
#endif
  else if ( quiet )
    return false;
  else
    return PL_domain_error("unicode_property", property);
}



		 /*******************************
		 *	      MAPPING		*
		 *******************************/

static bool
get_map_mask(term_t t, int *mask)
{ int m = 0;
  term_t tail = PL_copy_term_ref(t);
  term_t head = PL_new_term_ref();

  if ( PL_get_integer(t, mask) )
    return true;

  while ( PL_get_list(tail, head, tail) )
  { atom_t a;

    if ( !PL_get_atom_ex(head, &a) )
      return false;
    if ( a == ATOM_stable )
      m |= UTF8PROC_STABLE;
    else if ( a == ATOM_compat )
      m |= UTF8PROC_COMPAT;
    else if ( a == ATOM_compose )
      m |= UTF8PROC_COMPOSE;
    else if ( a == ATOM_decompose )
      m |= UTF8PROC_DECOMPOSE;
    else if ( a == ATOM_ignore )
      m |= UTF8PROC_IGNORE;
    else if ( a == ATOM_rejectna )
      m |= UTF8PROC_REJECTNA;
    else if ( a == ATOM_nlf2ls )
      m |= UTF8PROC_NLF2LS;
    else if ( a == ATOM_nlf2ps )
      m |= UTF8PROC_NLF2PS;
    else if ( a == ATOM_nlf2lf )
      m |= UTF8PROC_NLF2LF;
    else if ( a == ATOM_stripcc )
      m |= UTF8PROC_STRIPCC;
    else if ( a == ATOM_casefold )
      m |= UTF8PROC_CASEFOLD;
    else if ( a == ATOM_charbound )
      m |= UTF8PROC_CHARBOUND;
    else if ( a == ATOM_lump )
      m |= UTF8PROC_LUMP;
    else if ( a == ATOM_stripmark )
      m |= UTF8PROC_STRIPMARK;
    else
      return PL_domain_error("unicode_mapping", head);
  }

  if ( !PL_get_nil_ex(tail) )
    return false;

  *mask = m;

  return true;
}


/** unicode_map(+In, -Out, +Options)
*/

static foreign_t
unicode_map(term_t in, term_t out, term_t options)
{ int mask;
  size_t len_in;
  utf8proc_ssize_t len_out;
  char *utf8_in;
  uint8_t *utf8_out;

  if ( !get_map_mask(options, &mask) )
    return false;
  if ( !PL_get_nchars(in, &len_in, &utf8_in,
		      CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION|
		      REP_UTF8) )
    return false;
  if ( len_in == 0 )			/* empty atom */
    return PL_unify_chars(out, PL_ATOM, 0, utf8_in);
  len_out = utf8proc_map((uint8_t*)utf8_in, len_in, &utf8_out, mask);
  if ( len_out > 0 )
  { bool rc;

    rc = PL_unify_chars(out, PL_ATOM|REP_UTF8, len_out, (char*)utf8_out);
    free(utf8_out);
    return rc;
  } else
  { switch( len_out )
    { case UTF8PROC_ERROR_NOMEM:
	return PL_resource_error("memory");
      case UTF8PROC_ERROR_OVERFLOW:
	return PL_resource_error("string_length");
      case UTF8PROC_ERROR_INVALIDUTF8:
      case UTF8PROC_ERROR_NOTASSIGNED:
	return PL_domain_error("unicode_string", in);
      case UTF8PROC_ERROR_INVALIDOPTS:
	return PL_domain_error("unicode_map_options", options);
      default:
	assert(0);
	return false;
    }
  }
}


/** unicode_option_mask(+Options, -Mask)
*/

static foreign_t
unicode_option_mask(term_t options, term_t mask_t)
{ int mask;

  if ( !get_map_mask(options, &mask) )
    return false;

  return PL_unify_integer(mask_t, mask);
}


		 /*******************************
		 *	     GRAPHEMES		*
		 *******************************/

#ifdef HAVE_UTF8PROC_GRAPHEME_BREAK_STATEFUL

/* Forward mode: decompose Text into grapheme clusters, emit each
 * cluster as an atom/string (driven by out_type) in Graphemes.
 */
static bool
text_to_graphemes(const char *utf8, size_t len,
		  term_t graphemes, int out_type)
{ const utf8proc_uint8_t *s   = (const utf8proc_uint8_t*)utf8;
  const utf8proc_uint8_t *end = s + len;
  term_t tail = PL_copy_term_ref(graphemes);
  term_t head = PL_new_term_ref();
  const utf8proc_uint8_t *cluster_start = s;
  utf8proc_int32_t prev_cp = 0;
  utf8proc_int32_t state   = 0;
  bool first = true;

  while (s < end)
  { utf8proc_int32_t cp;
    utf8proc_ssize_t r = utf8proc_iterate(s, end - s, &cp);

    if (r < 0)
      return PL_domain_error("utf8_string", graphemes);

    if (!first &&
	utf8proc_grapheme_break_stateful(prev_cp, cp, &state))
    { size_t clen = (size_t)(s - cluster_start);
      if ( !PL_unify_list(tail, head, tail) ||
	   !PL_unify_chars(head, out_type|REP_UTF8, clen,
			   (const char*)cluster_start) )
	return false;
      cluster_start = s;
    }

    prev_cp = cp;
    s += r;
    first = false;
  }

  if (cluster_start < end)
  { size_t clen = (size_t)(end - cluster_start);
    if ( !PL_unify_list(tail, head, tail) ||
	 !PL_unify_chars(head, out_type|REP_UTF8, clen,
			 (const char*)cluster_start) )
      return false;
  }

  return PL_unify_nil(tail);
}


/* Reverse mode: walk Graphemes list, concatenate each element's UTF-8,
 * unify the result with Text (as atom or string based on out_type).
 */
static bool
graphemes_to_text(term_t text, term_t graphemes, int out_type)
{ term_t tail = PL_copy_term_ref(graphemes);
  term_t head = PL_new_term_ref();
  char *buf = NULL;
  size_t buflen = 0;
  size_t bufcap = 0;
  bool rc = true;

  while ( rc && PL_get_list(tail, head, tail) )
  { char *u;
    size_t ulen;

    if ( !PL_get_nchars(head, &ulen, &u,
			CVT_ATOM|CVT_STRING|CVT_LIST|
			REP_UTF8|CVT_EXCEPTION) )
    { rc = false;
      break;
    }
    if ( buflen + ulen > bufcap )
    { size_t ncap = (bufcap ? bufcap*2 : 64);
      while ( ncap < buflen + ulen ) ncap *= 2;
      char *nb = realloc(buf, ncap);
      if ( !nb ) { free(buf); return PL_resource_error("memory"); }
      buf = nb;
      bufcap = ncap;
    }
    memcpy(buf+buflen, u, ulen);
    buflen += ulen;
  }

  if ( rc && !PL_get_nil_ex(tail) )
    rc = false;

  if ( rc )
    rc = PL_unify_chars(text, out_type|REP_UTF8, buflen,
			buf ? buf : "");

  free(buf);
  return rc;
}


static foreign_t
text_graphemes(term_t text, term_t graphemes, int out_type)
{ char *utf8;
  size_t len;

  if ( PL_get_nchars(text, &len, &utf8,
		     CVT_ATOM|CVT_STRING|REP_UTF8|BUF_STACK) )
    return text_to_graphemes(utf8, len, graphemes, out_type);

  return graphemes_to_text(text, graphemes, out_type);
}


/** atom_graphemes(?Atom, ?Graphemes)
*/
static foreign_t
pl_atom_graphemes(term_t text, term_t graphemes)
{ return text_graphemes(text, graphemes, PL_ATOM);
}

/** string_graphemes(?String, ?Graphemes)
*/
static foreign_t
pl_string_graphemes(term_t text, term_t graphemes)
{ return text_graphemes(text, graphemes, PL_STRING);
}

#endif /* HAVE_UTF8PROC_GRAPHEME_BREAK_STATEFUL */


		 /*******************************
		 *    VERSION AND VALIDITY	*
		 *******************************/

#ifdef HAVE_UTF8PROC_UNICODE_VERSION
/** unicode_version(-Version) is det.
 *
 * Version is an atom like '15.1.0' describing the Unicode version the
 * bound utf8proc library implements.
 */
static foreign_t
unicode_version(term_t version)
{ return PL_unify_atom_chars(version, utf8proc_unicode_version());
}
#endif


#ifdef HAVE_UTF8PROC_CODEPOINT_VALID
/** unicode_codepoint_valid(+Code) is semidet.
 *
 * True when Code is a non-negative integer in the Unicode range and
 * has an assigned category (not `Cn`).
 */
static foreign_t
unicode_codepoint_valid(term_t code)
{ int32_t uc;

  if ( !PL_get_integer(code, &uc) )
    return PL_type_error("integer", code);

  if ( !utf8proc_codepoint_valid(uc) )
    return false;

  const utf8proc_property_t *p = utf8proc_get_property(uc);
  return p->category != UTF8PROC_CATEGORY_CN;
}
#endif


		 /*******************************
		 *	    INSTALLATION	*
		 *******************************/

#define MKATOM(n) \
	ATOM_ ## n = PL_new_atom(#n)

/* Kernel atom-normalisation callback.  Registered with
 * PL_atom_normalize_hook in install_unicode4pl().  Performs NFC
 * normalisation on UTF-8 in place: the NFC result is always shorter
 * than or equal to the input, so the result fits in the original
 * buffer.  On return *len holds the new byte length.  Returns 0 on
 * success, -1 on error.
 */
static int
utf8proc_normalize_cb(unsigned char *in, size_t *len)
{ utf8proc_uint8_t *result;
  utf8proc_ssize_t out_len;

  out_len = utf8proc_map((uint8_t*)in, *len, &result,
			 UTF8PROC_STABLE | UTF8PROC_COMPOSE);
  if ( out_len < 0 )
    return -1;

  if ( (size_t)out_len != *len ||
       memcmp(result, in, (size_t)out_len) != 0 )
  { assert((size_t)out_len <= *len);
    memcpy(in, result, (size_t)out_len);
    *len = (size_t)out_len;
  }
  free(result);
  return 0;
}


static PL_atom_normalize_t old_hook = NULL;

install_t
install_unicode4pl(void)
{ MKATOM(category);
  MKATOM(combining_class);
  MKATOM(bidi_class);
#ifdef HAVE_UTF8PROC_BIDI_MIRRORED
  MKATOM(bidi_mirrored);
#endif
  MKATOM(decomp_type);
  MKATOM(ignorable);
  MKATOM(boundclass);
#ifdef HAVE_UTF8PROC_CHARWIDTH
  MKATOM(width);
#endif
#ifdef HAVE_UTF8PROC_AMBIGUOUS_WIDTH
  MKATOM(ambiguous_width);
#endif
#ifdef HAVE_UTF8PROC_TOUPPER
  MKATOM(uppercase);
#endif
#ifdef HAVE_UTF8PROC_TOLOWER
  MKATOM(lowercase);
#endif
#ifdef HAVE_UTF8PROC_TOTITLE
  MKATOM(titlecase);
#endif
#ifdef HAVE_UTF8PROC_INDIC_CONJUNCT_BREAK
  MKATOM(indic_conjunct_break);
#endif

  MKATOM(stable);
  MKATOM(compat);
  MKATOM(compose);
  MKATOM(decompose);
  MKATOM(ignore);
  MKATOM(rejectna);
  MKATOM(nlf2ls);
  MKATOM(nlf2ps);
  MKATOM(nlf2lf);
  MKATOM(stripcc);
  MKATOM(casefold);
  MKATOM(charbound);
  MKATOM(lump);
  MKATOM(stripmark);

  assert(sizeof(char) == sizeof(uint8_t));

  PL_register_foreign("$unicode_property",       3, unicode_property,	      0);
  PL_register_foreign("unicode_map",		 3, unicode_map,	      0);
  PL_register_foreign("unicode_option_mask",	 2, unicode_option_mask,      0);
#ifdef HAVE_UTF8PROC_GRAPHEME_BREAK_STATEFUL
  PL_register_foreign("atom_graphemes",		 2, pl_atom_graphemes,	      0);
  PL_register_foreign("string_graphemes",	 2, pl_string_graphemes,      0);
#endif
#ifdef HAVE_UTF8PROC_UNICODE_VERSION
  PL_register_foreign("unicode_version",	 1, unicode_version,	      0);
#endif
#ifdef HAVE_UTF8PROC_CODEPOINT_VALID
  PL_register_foreign("unicode_codepoint_valid", 1, unicode_codepoint_valid,  0);
#endif

  old_hook = PL_atom_normalize_hook(utf8proc_normalize_cb);
}

install_t
uninstall_unicode4pl(void)
{ PL_atom_normalize_hook(old_hook);
}
