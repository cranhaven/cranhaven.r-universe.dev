/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2025, VU University, Amsterdam
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
#include "utf8proc.h"
#include <assert.h>
#include <ctype.h>

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

static functor_t FUNCTOR_error2;
static functor_t FUNCTOR_type_error2;
static functor_t FUNCTOR_domain_error2;
static functor_t FUNCTOR_resource_error1;

static atom_t ATOM_category;
static atom_t ATOM_combining_class;
static atom_t ATOM_bidi_class;
static atom_t ATOM_decomp_type;
static atom_t ATOM_decomp_mapping;
static atom_t ATOM_bidi_mirrored;
static atom_t ATOM_uppercase_mapping;
static atom_t ATOM_lowercase_mapping;
static atom_t ATOM_titlecase_mapping;
static atom_t ATOM_comb1st_index;
static atom_t ATOM_comb2nd_index;
static atom_t ATOM_comp_exclusion;
static atom_t ATOM_ignorable;
static atom_t ATOM_control_boundary;
static atom_t ATOM_extend;
static atom_t ATOM_casefold_mapping;


static int
type_error(const char *expected, term_t actual)
{ term_t ex;

  if ( (ex = PL_new_term_ref()) &&
       PL_unify_term(ex,
                     PL_FUNCTOR, FUNCTOR_error2,
                       PL_FUNCTOR, FUNCTOR_type_error2,
                         PL_CHARS, expected,
                         PL_TERM, actual,
                       PL_VARIABLE) )
    return PL_raise_exception(ex);

  return FALSE;
}


static int
domain_error(const char *domain, term_t actual)
{ term_t ex;

  if ( (ex = PL_new_term_ref()) &&
       PL_unify_term(ex,
                     PL_FUNCTOR, FUNCTOR_error2,
                       PL_FUNCTOR, FUNCTOR_domain_error2,
                         PL_CHARS, domain,
                         PL_TERM, actual,
                       PL_VARIABLE) )
  return PL_raise_exception(ex);

  return FALSE;
}


static int
resource_error(const char *what)
{ term_t ex;

  if ( (ex = PL_new_term_ref()) &&
       PL_unify_term(ex,
		     PL_FUNCTOR, FUNCTOR_error2,
		       PL_FUNCTOR, FUNCTOR_resource_error1,
		         PL_CHARS, what,
		       PL_VARIABLE) )
    return PL_raise_exception(ex);

  return FALSE;
}


/** unicode_property(?Code, ?property)

*/

typedef struct pmap
{ char		code;
  char	       *name;
  atom_t	atom;
} pmap;

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


static int
unify_symbol(term_t arg, int code, pmap *map)
{ pmap *m;

  if ( code == 0 )
    return FALSE;

  m = &map[code-1];
  if ( m->code != code )
  { for(m=map; m->code && m->code != code; m++)
      ;
    assert(m->code == code);
  }

  if ( !m->atom )
  { char buf[10];
    char *s, *o;

    if ( map == category_map )
    { buf[0] = m->name[0];
      buf[1] = tolower(m->name[1]);
      buf[2] = 0;
    } else
    { for(s=m->name,o=buf; *s; s++)
      { if ( isupper(*s) )
	  *o++ = tolower(*s);
	else
	  *o++ = *s;
      }
      *o = 0;
    }

    m->atom = PL_new_atom(buf);
  }

  if ( PL_unify_atom(arg, m->atom) )
    return TRUE;

  if ( map == category_map )
  { char *s;
    size_t len;

    if ( PL_get_atom_nchars(arg, &len, &s) && len == 1 &&
	 m->name[0] == s[0] )
      return TRUE;
  }

  return FALSE;
}


static int
unify_wstring(term_t arg, const int32_t *ws)
{ term_t tail = PL_copy_term_ref(arg);
  term_t head = PL_new_term_ref();

  for(; *ws; ws++)
  { if ( !PL_unify_list(tail, head, tail) ||
	 !PL_unify_integer(head, *ws) )
      return FALSE;
  }

  return PL_unify_nil(tail);
}


static foreign_t
unicode_property(term_t code, term_t property)
{ int32_t uc;
  const utf8proc_property_t *p;
  atom_t pname;
  size_t parity;
  term_t arg = PL_new_term_ref();

  if ( !PL_get_integer(code, &uc) )
  { pl_wchar_t *ws;
    size_t len;

    if ( PL_get_wchars(code, &len, &ws, CVT_ATOM) &&
	 len == 1 )
    { uc = ws[0];
    } else
      return type_error("code", code);
  }

  if ( uc < 0 || uc > 0x10FFFF )
    return domain_error("code", code);

  p = utf8proc_get_property(uc);
  if ( !p->category )
    return FALSE;

  if ( !PL_get_name_arity(property, &pname, &parity) ||
       parity != 1 )
    return type_error("compound", property);
  _PL_get_arg(1, property, arg);

  if ( pname == ATOM_category )
    return unify_symbol(arg, p->category, category_map);
  else if ( pname == ATOM_combining_class )
    return PL_unify_integer(arg, p->combining_class);
  else if ( pname == ATOM_bidi_class )
    return unify_symbol(arg, p->bidi_class, bidi_map);
  else if ( pname == ATOM_decomp_type )
    return unify_symbol(arg, p->decomp_type, decomp_map);
  else if ( pname == ATOM_decomp_mapping )
    return p->decomp_mapping ? unify_wstring(arg, p->decomp_mapping) : FALSE;
  else if ( pname == ATOM_bidi_mirrored )
    return PL_unify_bool(arg, p->bidi_mirrored);
  else if ( pname == ATOM_uppercase_mapping )
    return p->uppercase_mapping >= 0 ?
		PL_unify_integer(arg, p->uppercase_mapping) : FALSE;
  else if ( pname == ATOM_lowercase_mapping )
    return p->lowercase_mapping >= 0 ?
		PL_unify_integer(arg, p->lowercase_mapping) : FALSE;
  else if ( pname == ATOM_titlecase_mapping )
    return p->titlecase_mapping >= 0 ?
		PL_unify_integer(arg, p->titlecase_mapping) : FALSE;
  else if ( pname == ATOM_comb1st_index )
    return p->comb1st_index >= 0 ?
		PL_unify_integer(arg, p->comb1st_index) : FALSE ;
  else if ( pname == ATOM_comb2nd_index )
    return p->comb2nd_index >= 0 ?
		PL_unify_integer(arg, p->comb2nd_index) : FALSE;
  else if ( pname == ATOM_comp_exclusion )
    return PL_unify_bool(arg, p->comp_exclusion);
  else if ( pname == ATOM_ignorable )
    return PL_unify_bool(arg, p->ignorable);
  else if ( pname == ATOM_control_boundary )
    return PL_unify_bool(arg, p->control_boundary);
  else if ( pname == ATOM_extend )
    return PL_unify_bool(arg, p->extend);
  else if ( pname == ATOM_casefold_mapping )
  { if ( p->casefold_mapping )
    { term_t tail = PL_copy_term_ref(arg);
      term_t head = PL_new_term_ref();
      const int32_t *ws = p->casefold_mapping;

      for(; *ws >= 0; ws++)
      { if ( !PL_unify_list(tail, head, tail) ||
	     !PL_unify_integer(head, *ws) )
	  return FALSE;
      }

      return PL_unify_nil(tail);
    }
    return FALSE;
  } else
    return domain_error("unicode_property", property);
}



		 /*******************************
		 *	      MAPPING		*
		 *******************************/

static int
get_map_mask(term_t t, int *mask)
{ int m = 0;
  term_t tail = PL_copy_term_ref(t);
  term_t head = PL_new_term_ref();

  if ( PL_get_integer(t, mask) )
    return TRUE;

  while ( PL_get_list(tail, head, tail) )
  { atom_t a;

    if ( !PL_get_atom(head, &a) )
      return type_error("atom", head);
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
      return domain_error("unicode_mapping", head);
  }

  if ( !PL_get_nil(tail) )
    return type_error("list", tail);

  *mask = m;

  return TRUE;
}


/** unicode_map(+In, -Out, +Options)
*/

static foreign_t
unicode_map(term_t in, term_t out, term_t options)
{ int mask;
  size_t len_in;
  ssize_t len_out;
  char *utf8_in;
  uint8_t *utf8_out;

  if ( !get_map_mask(options, &mask) )
    return FALSE;
  if ( !PL_get_nchars(in, &len_in, &utf8_in,
		      CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION|
		      REP_UTF8) )
    return FALSE;
  if ( len_in == 0 )			/* empty atom */
    return PL_unify_chars(out, PL_ATOM, 0, utf8_in);
  len_out = utf8proc_map((uint8_t*)utf8_in, len_in, &utf8_out, mask);
  if ( len_out > 0 )
  { int rc;

    rc = PL_unify_chars(out, PL_ATOM|REP_UTF8, len_out, (char*)utf8_out);
    free(utf8_out);
    return rc;
  } else
  { switch( len_out )
    { case UTF8PROC_ERROR_NOMEM:
	return resource_error("memory");
      case UTF8PROC_ERROR_OVERFLOW:
	return resource_error("string_length");
      case UTF8PROC_ERROR_INVALIDUTF8:
      case UTF8PROC_ERROR_NOTASSIGNED:
	return domain_error("unicode_string", in);
      case UTF8PROC_ERROR_INVALIDOPTS:
	return domain_error("unicode_map_options", options);
      default:
	assert(0);
        return FALSE;
    }
  }
}


/** unicode_option_mask(+Options, -Mask)
*/

static foreign_t
unicode_option_mask(term_t options, term_t mask_t)
{ int mask;

  if ( !get_map_mask(options, &mask) )
    return FALSE;

  return PL_unify_integer(mask_t, mask);
}


#define MKATOM(n) \
        ATOM_ ## n = PL_new_atom(#n)
#define MKFUNCTOR(n,a) \
        FUNCTOR_ ## n ## a = PL_new_functor(PL_new_atom(#n), a)

install_t
install_unicode4pl()
{ MKFUNCTOR(error, 2);
  MKFUNCTOR(type_error, 2);
  MKFUNCTOR(domain_error, 2);
  MKFUNCTOR(resource_error, 1);

  MKATOM(category);
  MKATOM(combining_class);
  MKATOM(bidi_class);
  MKATOM(decomp_type);
  MKATOM(decomp_mapping);
  MKATOM(bidi_mirrored);
  MKATOM(uppercase_mapping);
  MKATOM(lowercase_mapping);
  MKATOM(titlecase_mapping);
  MKATOM(comb1st_index);
  MKATOM(comb2nd_index);
  MKATOM(comp_exclusion);
  MKATOM(ignorable);
  MKATOM(control_boundary);
  MKATOM(extend);
  MKATOM(casefold_mapping);

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

  PL_register_foreign("$unicode_property",   2, unicode_property,    0);
  PL_register_foreign("unicode_map",	     3, unicode_map,	     0);
  PL_register_foreign("unicode_option_mask", 2, unicode_option_mask, 0);
}
