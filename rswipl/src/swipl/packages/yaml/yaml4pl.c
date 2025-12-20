/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018-2023, VU University Amsterdam
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

#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <yaml.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>

#ifdef _MSC_VER
#define strdup(s) _strdup(s)
#endif

atom_t ATOM_null;
atom_t ATOM_true;
atom_t ATOM_false;
atom_t ATOM_canonical;
atom_t ATOM_unicode;
atom_t ATOM_stream_start;
atom_t ATOM_stream_end;
atom_t ATOM_document_start;
atom_t ATOM_document_end;
atom_t ATOM_mapping_start;
atom_t ATOM_mapping_end;
atom_t ATOM_sequence_start;
atom_t ATOM_sequence_end;
atom_t ATOM_scalar;
atom_t ATOM_alias;
atom_t ATOM_plain;
atom_t ATOM_single_quoted;
atom_t ATOM_double_quoted;
atom_t ATOM_literal;
atom_t ATOM_folded;

functor_t FUNCTOR_mapping3;
functor_t FUNCTOR_sequence3;
functor_t FUNCTOR_eq2;
functor_t FUNCTOR_tag2;
functor_t FUNCTOR_error2;
functor_t FUNCTOR_yaml_error2;

#define N_TOP      0
#define N_MAPPING  1
#define N_SEQUENCE 2

typedef struct node
{ struct node *parent;
  int    type;					/* N_* */
  term_t key;					/* Created key */
  term_t doc;					/* saved document tail */
} node;

static int yaml_read_handler(void *closure,
			     unsigned char *buffer, size_t size,
			     size_t *size_read);


static node *
push(node **stack, node *old)
{ node *n;

  if ( (n=malloc(sizeof(*n))) )
  { *n = *old;
    n->parent = *stack;
    *stack    = n;
    return n;
  }

  PL_resource_error("memory");
  return NULL;
}

static int
pop(node **stack, node *old)
{ node *n;

  if ( (n=*stack) )
  { *old = *n;
    *stack = n->parent;
    free(n);
    return TRUE;
  }

  assert(0);
  return FALSE;
}

static int
extend(node *state, term_t tmp, term_t v)
{ switch(state->type)
  { case N_TOP:
      return PL_unify(state->doc, v);
    case N_SEQUENCE:
      return PL_unify_list(state->doc, tmp, state->doc) &&
	     PL_unify(tmp, v);
    case N_MAPPING:
      if ( state->key )
      { term_t key = state->key;
	state->key = 0;
	return PL_unify_list(state->doc, tmp, state->doc) &&
	       PL_unify_term(tmp, PL_FUNCTOR, FUNCTOR_eq2,
				    PL_TERM, key, PL_TERM, v);
      } else
      { state->key = v;
	return TRUE;
      }
    default:
      assert(0);
      return FALSE;
  }
}

static inline int
is_digit(int c)
{ return ( c >= '0' && c <= '9' );
}

static inline int
is_nonzero_digit(int c)
{ return ( c >= '1' && c <= '9' );
}

static inline int
is_odigit(int c)
{ return ( c >= '0' && c <= '7' );
}

static inline int
is_xdigit(int c)
{ return ( (c >= '0' && c <= '9') ||
	   (c >= 'a' && c <= 'f') ||
	   (c >= 'A' && c <= 'F') );
}

enum ntype
{ N_NONE = 0,
  N_INT,
  N_FLOAT,
};

/* See https://yaml.org/spec/1.2.2/#10214-floating-point */

static enum ntype
is_number(const char *s, size_t len)
{ if ( len > 0 )
  { const char *e = s+len;
    int isfloat = FALSE;

    if ( e>s && *s == '-' )		/* -? */
      s++;

    if ( e-s >= 3 && *s == '0' )	/* 0oN+$ or 0xN+$ */
    { s++;
      if ( *s == 'o' )
      { s++;
	if ( s < e )
	{ while(is_odigit(*s))
	    s++;
	}
	return s == e ? N_INT : N_NONE;
      } else if ( *s == 'x' )
      { s++;
	if ( s < e )
	{ while(is_xdigit(*s))
	    s++;
	}
	return s == e ? N_INT : N_NONE;
      }

      s--;
    }

    if ( e == s )
      return N_NONE;

    if ( *s == '0' )		/* ( 0 | ([1-9]+ [0-9]*) ) */
    { s++;
    } else if ( is_nonzero_digit(*s) )
    { s++;
      while(is_digit(*s))
	s++;
    } else
      return N_NONE;

    if ( e > s && *s == '.' )	/* ( \. [0-9]* ) */
    { s++;
      if ( e > s && is_digit(*s) ) /* officical YAML says "0." __is__ allowed  */
      { while(e > s && is_digit(*s))	/* we do not allow it. */
	  s++;
	isfloat = TRUE;
      } else
	return N_NONE;
    }

    if ( e > s && (*s == 'e' || *s == 'E') ) /* ( \. [0-9]* )? ( [eE] [-+]? [0-9]+ )? */
    { s++;
      if ( e > s && (*s == '+' || *s == '-') )
	s++;
      if ( e > s && is_digit(*s) )
      { s++;
	while(e > s && is_digit(*s))
	  s++;
      } else
	return N_NONE;
      isfloat = TRUE;
    }

    if ( s == e )
      return isfloat ? N_FLOAT : N_INT;
  }

  return N_NONE;
}

static int
is_null(const char *s, size_t len)
{ if ( len == 4 &&
       ( strcmp(s, "null") == 0 ||
         strcmp(s, "Null") == 0 ||
	 strcmp(s, "NULL") == 0 ) )
    return TRUE;
  if ( len == 1 && *s == '~' )
    return TRUE;
  return FALSE;
}

static int
is_true(const char *s, size_t len)
{ if ( len == 4 &&
       ( strcmp(s, "true") == 0 ||
         strcmp(s, "True") == 0 ||
	 strcmp(s, "TRUE") == 0 ) )
    return TRUE;
  return FALSE;
}

static int
is_false(const char *s, size_t len)
{ if ( len == 5 &&
       ( strcmp(s, "false") == 0 ||
         strcmp(s, "False") == 0 ||
	 strcmp(s, "FALSE") == 0 ) )
    return TRUE;
  return FALSE;
}

static int
is_special_float(const char *s, size_t len, double *d)
{ int neg = FALSE;

  if ( len == 4 &&
       ( strcmp(s, ".nan") == 0 ||
         strcmp(s, ".NaN") == 0 ||
	 strcmp(s, ".NAN") == 0 ) )
  { *d = NAN;
    return TRUE;
  }
  if ( *s == '-' || *s == '+' )
  { neg = (*s == '-');
    s++;
    len--;
  }
  if ( len == 4 &&
       ( strcmp(s, ".inf") == 0 ||
         strcmp(s, ".Inf") == 0 ||
	 strcmp(s, ".INF") == 0 ) )
  { if ( neg )
      *d = -HUGE_VAL;
    else
      *d = HUGE_VAL;
    return TRUE;
  }

  return FALSE;
}


static const char *
implicit_tag(const char *s)
{ enum ntype rc;
  double d;
  size_t len = strlen(s);

  if ( (rc=is_number(s, len)) != N_NONE )
    return rc == N_FLOAT ? "float" : "int";
  else if ( is_null(s, len) )
    return "null";
  else if ( is_true(s, len) || is_false(s, len) )
    return "bool";
  else if ( is_special_float(s, len, &d) )
    return "float";
  else
    return NULL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Out a scalar. If it is tagged we   put a term tag(Tag, String) and leave
the rest to the Prolog library. If it is not tagged we need to implement

"10.3.2. Tag Resolution" from http://yaml.org/spec/1.2/spec.html
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
put_scalar(term_t t, int iskey, const yaml_event_t *event)
{ char *s = (char*)event->data.scalar.value;
  size_t len = event->data.scalar.length;
  int flags = iskey ? PL_ATOM|REP_UTF8 : PL_STRING|REP_UTF8;
  double d;

  if ( event->data.scalar.tag )
  { term_t tmp = PL_new_term_ref();

    return ( PL_put_functor(t, FUNCTOR_tag2) &&
	     PL_get_arg(1, t, tmp) &&
	     PL_unify_atom_chars(tmp, (char*)event->data.scalar.tag) &&
	     PL_get_arg(2, t, tmp) &&
	     PL_unify_chars(tmp, flags, len, s) );
  }

  if ( is_number(s, len) != N_NONE )
  { return PL_chars_to_term(s, t);
  } else if ( is_null(s, len) )
  { return PL_unify_atom(t, ATOM_null);
  } else if ( is_true(s, len) )
  { return PL_unify_atom(t, ATOM_true);
  } else if ( is_false(s, len) )
  { return PL_unify_atom(t, ATOM_false);
  } else if ( is_special_float(s, len, &d) )
  { return PL_unify_float(t, d);
  } else
  { return PL_unify_chars(t, flags, len, s);
  }
}

#define ANCHOR_TABLE_SIZE 16

typedef struct hcell
{ struct hcell *next;
  char	       *name;
  term_t        value;
} hcell;

typedef struct htable
{ size_t count;
  size_t size;
  hcell **entries;
} htable;

static htable *
new_anchor_table(void)
{ htable *t;
  size_t ebytes = sizeof(*t->entries) * ANCHOR_TABLE_SIZE;

  if ( (t = malloc(sizeof(*t))) &&
       (t->entries = malloc(ebytes)) )
  { t->count = 0;
    t->size = ANCHOR_TABLE_SIZE;
    memset(t->entries, 0, ebytes);
    return t;
  }

  if ( t )
    free(t);

  PL_resource_error("memory");
  return NULL;
}

static void
free_anchor_table(htable *t)
{ if ( t )
  { size_t i;

    for(i=0; i<t->size; i++)
    { hcell *c = t->entries[i];
      hcell *n;

      for(; c; c=n)
      { n = c->next;
	free(c->name);
	free(c);
      }
    }

    free(t->entries);
    free(t);
  }
}

#define SEED 0x6263533a
#define MIX(h,k,m) { k *= m; k ^= k >> r; k *= m; h *= m; h ^= k; }

static unsigned int
MurmurHashAligned2(const void * key, size_t len, unsigned int seed)
{ const unsigned int m = 0x5bd1e995;
  const int r = 24;
  unsigned int h = seed ^ (unsigned int)len;
  const unsigned char * data = (const unsigned char *)key;

  while( len >= 4 )
  { unsigned int k;

    k  = data[0];
    k |= data[1] << 8;
    k |= data[2] << 16;
    k |= data[3] << 24;

    MIX(h,k,m);

    data += 4;
    len -= 4;
  }

  switch( len )
  { case 3: h ^= data[2] << 16;
    case 2: h ^= data[1] << 8;
    case 1: h ^= data[0];
      h *= m;
  };

  h ^= h >> 13;
  h *= m;
  h ^= h >> 15;

  return h;
}

static int
rehash(htable *t)
{ size_t newsize = t->size*2;
  size_t ebytes = sizeof(*t->entries) * newsize;
  hcell **new;

  if ( (new=malloc(ebytes)) )
  { size_t i;

    memset(new, 0, ebytes);
    for(i=0; i<t->size; i++)
    { hcell *c = t->entries[i];
      hcell *n;

      for(; c; c=n)
      { int k = MurmurHashAligned2(c->name, strlen(c->name), SEED) % newsize;

	n = c->next;
	c->next = new[k];
	new[k] = c;
      }
    }

    free(t->entries);
    t->size = newsize;
    t->entries = new;
    return FALSE;
  }

  return PL_resource_error("memory");
}


static int
add_anchor(htable *t, const char *name, term_t value)
{ const char *s = name;
  unsigned int k;
  hcell *c;

  if ( t->count > t->size )
  { if ( !rehash(t) )
      return FALSE;
  }

  k = MurmurHashAligned2(s, strlen(s), SEED) % t->size;
  if ( (c=malloc(sizeof(*c))) )
  { c->name       = strdup(s);
    c->value      = PL_copy_term_ref(value);
    c->next       = t->entries[k];
    t->entries[k] = c;
    t->count++;

    return TRUE;
  }

  return PL_resource_error("memory");
}

term_t
find_anchor(htable *t, const char *name)
{ unsigned int k = MurmurHashAligned2(name, strlen(name), SEED) % t->size;
  hcell *c;

  for(c=t->entries[k]; c; c=c->next)
  { if ( strcmp(c->name, name) == 0 )
      return c->value;
  }

  return 0;
}

static int
store_anchor(htable **anchors, term_t node, const yaml_event_t *event)
{ if ( event->data.alias.anchor )
  { if ( !*anchors )
    { if ( !(*anchors = new_anchor_table()) )
	return FALSE;
    }

    return add_anchor(*anchors, (const char*)event->data.alias.anchor, node);
  }

  return TRUE;
}

static int
create_mapping(term_t t, const yaml_event_t *event)
{ if ( !PL_put_functor(t, FUNCTOR_mapping3) )
    return FALSE;

  if ( event->data.mapping_start.tag )
  { term_t tag = PL_new_term_ref();

    if ( !PL_put_atom_chars(tag, (char*)event->data.mapping_start.tag) ||
	 !PL_cons_functor(t, FUNCTOR_tag2, tag, t) )
      return FALSE;
  }

  return TRUE;
}

static int
get_mapping_mapping(term_t t)
{ if ( PL_is_functor(t, FUNCTOR_tag2) )
    _PL_get_arg(2, t, t);
  _PL_get_arg(1, t, t);				/* mapping */

  return TRUE;
}

static int
create_sequence(term_t t, const yaml_event_t *event)
{ if ( !PL_put_functor(t, FUNCTOR_sequence3) )
    return FALSE;

  if ( event->data.sequence_start.tag )
  { term_t tag = PL_new_term_ref();

    if ( !PL_put_atom_chars(tag, (char*)event->data.sequence_start.tag) ||
	 !PL_cons_functor(t, FUNCTOR_tag2, tag, t) )
      return FALSE;
  }

  return TRUE;
}

static int
get_sequence_list(term_t t)
{ if ( PL_is_functor(t, FUNCTOR_tag2) )
    _PL_get_arg(2, t, t);
  _PL_get_arg(1, t, t);

  return TRUE;
}

static int
parse_document(yaml_parser_t *parser, term_t doc)
{ yaml_event_t event;
  node *stack = NULL;
  node state;
  term_t tmp = PL_new_term_ref();
  htable *anchor_table = NULL;

  state.type = N_TOP;
  state.key  = 0;
  state.doc  = doc;

  do
  { if ( !yaml_parser_parse(parser, &event) )
    { term_t ex;

      if ( (ex=PL_new_term_ref()) &&
	   PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
			       PL_FUNCTOR, FUNCTOR_yaml_error2,
				 PL_INT, (int)parser->error,
				 PL_STRING, parser->problem,
			       PL_VARIABLE) )
	PL_raise_exception(ex);
      goto error;
    }

    switch(event.type)
    { case YAML_NO_EVENT:
        break;
      case YAML_STREAM_START_EVENT:
        break;
      case YAML_STREAM_END_EVENT:
        break;
      case YAML_DOCUMENT_START_EVENT:
        break;
      case YAML_DOCUMENT_END_EVENT:
        break;
      case YAML_SEQUENCE_START_EVENT:
      { term_t list;
        if ( !(list = PL_new_term_ref()) ||
	     !create_sequence(list, &event) ||
	     !store_anchor(&anchor_table, list, &event) ||
	     !extend(&state, tmp, list) ||
	     !push(&stack, &state) ||
	     !get_sequence_list(list) )
	  goto error;
	state.doc  = list;
	state.type = N_SEQUENCE;
        break;
      }
      case YAML_SEQUENCE_END_EVENT:
        if ( !PL_unify_nil(state.doc) ||
	     !pop(&stack, &state) )
	  goto error;
        break;
      case YAML_MAPPING_START_EVENT:
      { term_t mapping;

	if ( !(mapping = PL_new_term_ref()) ||
	     !create_mapping(mapping, &event) ||
	     !store_anchor(&anchor_table, mapping, &event) ||
	     !extend(&state, tmp, mapping) ||
	     !push(&stack, &state) ||
	     !get_mapping_mapping(mapping) )
	  goto error;
	state.doc  = mapping;
	state.type = N_MAPPING;
	state.key  = 0;
        break;
      }
      case YAML_MAPPING_END_EVENT:
	if ( !PL_unify_nil(state.doc) ||
	     !pop(&stack, &state) )
	  goto error;
        break;
      case YAML_ALIAS_EVENT:
      { term_t node;
	const char *anchor = (const char*)event.data.alias.anchor;

        if ( anchor_table &&
	     (node = find_anchor(anchor_table, anchor)) )
	{ if ( !extend(&state, tmp, node) )
	    goto error;
	} else
	{ term_t ex;

	  return ( (ex=PL_new_term_ref()) &&
		   PL_put_atom_chars(ex, anchor) &&
		   PL_existence_error("anchor", ex) );
	}
        break;
      }
      case YAML_SCALAR_EVENT:
      { term_t val;
	int iskey = (state.type == N_MAPPING && !state.key);
        if ( !(val = PL_new_term_ref()) ||
	     !put_scalar(val, iskey, &event) ||
	     !extend(&state, tmp, val) )
	  goto error;
        break;
      }
    }
    if ( event.type != YAML_STREAM_END_EVENT )
      yaml_event_delete(&event);
  } while(event.type != YAML_STREAM_END_EVENT);
  yaml_event_delete(&event);

  free_anchor_table(anchor_table);

  return TRUE;

error:
  free_anchor_table(anchor_table);
  while(stack)
    pop(&stack, &state);

  return FALSE;
}

static foreign_t
yaml_parse_stream(term_t stream, term_t doc)
{ IOSTREAM *fd;
  yaml_parser_t parser;
  int rc;

  if ( !PL_get_stream(stream, &fd, SIO_INPUT) )
    return FALSE;
  if ( !yaml_parser_initialize(&parser) )
  { PL_release_stream(fd);
    return PL_resource_error("memory");
  }

  yaml_parser_set_input(&parser, yaml_read_handler, fd);
  rc = parse_document(&parser, PL_copy_term_ref(doc));
  yaml_parser_delete(&parser);
  rc = rc && PL_release_stream(fd);

  return rc;
}

		 /*******************************
		 *	       INPUT		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
UTF-8 Decoding, based on http://www.cl.cam.ac.uk/~mgk25/unicode.html
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define ISUTF8_CB(c)  (((c)&0xc0) == 0x80) /* Is continuation byte */
#define CONT(i)   ISUTF8_CB(in[i])
#define VAL(i, s) ((in[i]&0x3f) << s)

#define IS_UTF8_2BYTE(in) \
	((in[0]&0xe0) == 0xc0 && CONT(1))
#define IS_UTF8_3BYTE(in) \
	((in[0]&0xf0) == 0xe0 && CONT(1)&&CONT(2))
#define IS_UTF8_4BYTE(in) \
	((in[0]&0xf8) == 0xf0 && CONT(1)&&CONT(2)&&CONT(3))
#define IS_UTF8_5BYTE(in) \
	((in[0]&0xfc) == 0xf8 && CONT(1)&&CONT(2)&&CONT(3)&&CONT(4))
#define IS_UTF8_6BYTE(in) \
	((in[0]&0xfe) == 0xfc && CONT(1)&&CONT(2)&&CONT(3)&&CONT(4)&&CONT(5))

#define utf8_get_char(in, chr) \
	(*(in) & 0x80 ? __utf8_get_char(in, chr) \
		      : (*(chr) = *(in), (char *)(in)+1))

static char *
__utf8_get_char(const char *in, int *chr)
{ if ( IS_UTF8_2BYTE(in) )		/* 2-byte, 0x80-0x7ff */
  { *chr = ((in[0]&0x1f) << 6)|VAL(1,0);
    return (char *)in+2;
  }

  if ( IS_UTF8_3BYTE(in) )		/* 3-byte, 0x800-0xffff */
  { *chr = ((in[0]&0xf) << 12)|VAL(1,6)|VAL(2,0);
    return (char *)in+3;
  }

  if ( IS_UTF8_4BYTE(in) )		/* 4-byte, 0x10000-0x1FFFFF */
  { *chr = ((in[0]&0x7) << 18)|VAL(1,12)|VAL(2,6)|VAL(3,0);
    return (char *)in+4;
  }

  if ( IS_UTF8_5BYTE(in) )		/* 5-byte, 0x200000-0x3FFFFFF */
  { *chr = ((in[0]&0x3) << 24)|VAL(1,18)|VAL(2,12)|VAL(3,6)|VAL(4,0);
    return (char *)in+5;
  }

  if ( IS_UTF8_6BYTE(in) )		/* 6-byte, 0x400000-0x7FFFFFF */
  { *chr = ((in[0]&0x1) << 30)|VAL(1,24)|VAL(2,18)|VAL(3,12)|VAL(4,6)|VAL(5,0);
    return (char *)in+6;
  }

  *chr = (*in)&0xff;			/* Error */

  return (char *)in+1;
}


static int
yaml_read_handler(void *closure,
		  unsigned char *buffer, size_t size, size_t *size_read)
{ IOSTREAM *in = closure;

  *size_read = Sfread(buffer, 1, size, in);
  return !Sferror(in);
}

static int
yaml_write_handler(void *closure,
		   unsigned char *buffer, size_t size)
{ IOSTREAM *out = closure;

  if ( out->encoding == ENC_OCTET )
  { Sfwrite(buffer, 1, size, out);
  } else
  { int c;
    const char *s = (const char *)buffer;
    const char *end = &s[size];

    while ( s < end )
    { s = utf8_get_char(s, &c);
      if ( Sputcode(c, out) < 0 )
	break;
    }
  }

  return !Sferror(out);
}


		 /*******************************
		 *	      EMITTER		*
		 *******************************/

static int
write_emitter(IOSTREAM *s, atom_t aref, int flags)
{ yaml_emitter_t *ref = PL_blob_data(aref, NULL, NULL);

  Sfprintf(s, "<yaml_emitter>(%p)", ref);
  return TRUE;
}


static void
acquire_emitter(atom_t aref)
{ yaml_emitter_t *ref = PL_blob_data(aref, NULL, NULL);
  (void)ref;
}


static int
release_emitter(atom_t aref)
{ yaml_emitter_t *ref = PL_blob_data(aref, NULL, NULL);

  yaml_emitter_delete(ref);
  free(ref);

  return TRUE;
}


static int
save_emitter(atom_t aref, IOSTREAM *fd)
{ yaml_emitter_t *ref = PL_blob_data(aref, NULL, NULL);
  (void)fd;

  return PL_warning("Cannot save reference to <yaml_emitter>(%p)",
		    ref);
}


static atom_t
load_emitter(IOSTREAM *fd)
{ (void)fd;

  return PL_new_atom("<yaml_emitter>");
}


static PL_blob_t emitter_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_NOCOPY,
  "yaml_emitter",
  release_emitter,
  NULL,
  write_emitter,
  acquire_emitter,
  save_emitter,
  load_emitter
};


static int
unify_emitter(term_t t, yaml_emitter_t *emitter)
{ return PL_unify_blob(t, emitter, sizeof(*emitter), &emitter_blob);
}

static int
get_emitter(term_t t, yaml_emitter_t **emitter)
{ void *p;
  size_t len;
  PL_blob_t *type;

  if ( PL_get_blob(t, &p, &len, &type) && type == &emitter_blob )
  { *emitter = p;
    return TRUE;
  }

  PL_type_error("yaml_emitter", t);
  return FALSE;
}


static foreign_t
yaml_emitter_create(term_t t, term_t stream, term_t options)
{ yaml_emitter_t *emitter = NULL;
  IOSTREAM *fd;
  int rc = FALSE;

  if ( !PL_get_stream(stream, &fd, SIO_OUTPUT) )
    return FALSE;

  if ( (emitter=malloc(sizeof(*emitter))) )
  { term_t tail = PL_copy_term_ref(options);
    term_t head = PL_new_term_ref();
    term_t arg  = PL_new_term_ref();
    int uset = FALSE;

    if ( !yaml_emitter_initialize(emitter) )
    { rc = PL_resource_error("memory");
      goto out;
    }

    yaml_emitter_set_output(emitter, yaml_write_handler, fd);
    while(PL_get_list_ex(tail, head, tail))
    { atom_t name;
      size_t arity;

      if ( PL_get_name_arity(head, &name, &arity) && arity == 1 )
      { _PL_get_arg(1, head, arg);

	if ( name == ATOM_canonical )
	{ int v;

	  if ( PL_get_bool_ex(arg, &v) )
	    yaml_emitter_set_canonical(emitter, v);
	  else
	    goto out;
	} else if ( name == ATOM_unicode )
	{ int v;

	  if ( PL_get_bool_ex(arg, &v) )
	    yaml_emitter_set_unicode(emitter, v);
	  else
	    goto out;
	  uset = TRUE;
	}
      } else
      { rc = PL_type_error("option", head);
	goto out;
      }
    }
    if ( !PL_get_nil_ex(tail) )
      goto out;

    if ( !uset )
      yaml_emitter_set_unicode(emitter, TRUE);

    return unify_emitter(t, emitter);
  } else
  { rc = PL_resource_error("memory");
    goto out;
  }

out:
  PL_release_stream(fd);
  if ( emitter )
    free(emitter);
  return rc;
}


static int
get_string_arg(int i, term_t ev, term_t tmp, yaml_char_t **s)
{ char *pls;

  if ( PL_get_arg(i, ev, tmp) )
  { int flags = CVT_ATOMIC|BUF_RING|REP_UTF8|CVT_EXCEPTION;

    if ( PL_is_variable(tmp) )
    { *s = NULL;

      return TRUE;
    } else if ( PL_get_chars(tmp, &pls, flags) )
    { *s = (yaml_char_t*)pls;

      return TRUE;
    }
  }

  return FALSE;
}

static int
get_value(term_t t, yaml_char_t **value, yaml_char_t **tag)
{ char *s;
  size_t len;
  int rc;
  double d;

  if ( PL_is_integer(t) )
  { rc = PL_get_nchars(t, &len, &s, CVT_INTEGER|BUF_RING|CVT_EXCEPTION);
  } else if ( PL_get_float(t, &d) )
  { if ( isnan(d) )
    { *value = (yaml_char_t*)".nan";
      return TRUE;
    } else if ( isinf(d) )
    { *value = (yaml_char_t*)( d<0 ? "-.inf" : ".inf" );
      return TRUE;
    } else
      rc = PL_get_nchars(t, &len, &s, CVT_FLOAT|BUF_RING|CVT_EXCEPTION);
  } else if ( PL_is_string(t) )
  { const char *tg;

    rc = PL_get_nchars(t, &len, &s, CVT_STRING|BUF_RING|REP_UTF8|CVT_EXCEPTION);
    if ( rc && *tag == NULL && (tg=implicit_tag(s)) )
      *tag = (yaml_char_t*)"tag:yaml.org,2002:str";
  } else if ( PL_is_atom(t) )
  { rc = PL_get_nchars(t, &len, &s, CVT_ATOM|BUF_RING|REP_UTF8|CVT_EXCEPTION);
  } else
  { rc = PL_type_error("yaml_scalar", t);
  }

  if ( !rc )
    return FALSE;
  if ( strlen(s) < len )
    return PL_domain_error("string_without_nul", t);

  *value = (yaml_char_t*)s;

  return TRUE;
}


static int
get_bool_arg(int i, term_t ev, term_t tmp, int *val)
{ return ( PL_get_arg(i, ev, tmp) &&
	   PL_get_bool_ex(tmp, val) );
}


static foreign_t
yaml_emit_event(term_t t, term_t Event)
{ yaml_emitter_t *emitter;
  atom_t name;
  size_t arity;

  if ( !get_emitter(t, &emitter) )
    return FALSE;

  if ( PL_get_name_arity(Event, &name, &arity) )
  { yaml_event_t event;
    int ok;
    term_t tmp = PL_new_term_ref();

#define GET_STRING_ARG(n, ev, s) \
	get_string_arg(n, ev, tmp, s)

    if ( name == ATOM_stream_start && arity == 0 )
    { ok = yaml_stream_start_event_initialize(&event, YAML_UTF8_ENCODING);
    } else if ( name == ATOM_stream_end && arity == 0 )
    { ok = yaml_stream_end_event_initialize(&event);
    } else if ( name == ATOM_document_start && arity <=	1 )
    { int implicit = FALSE;

      if ( arity == 1 && !get_bool_arg(1, Event, tmp, &implicit) )
	return FALSE;

      ok = yaml_document_start_event_initialize(
	       &event, NULL, NULL, NULL, implicit);
    } else if ( name == ATOM_document_end && arity <= 1 )
    { int implicit = FALSE;

      if ( arity == 1 && !get_bool_arg(1, Event, tmp, &implicit) )
	return FALSE;
      ok = yaml_document_end_event_initialize(&event, implicit);
    } else if ( name == ATOM_mapping_start && arity == 2 )
    { yaml_char_t *anchor, *tag;
      if ( !GET_STRING_ARG(1, Event, &anchor) ||
	   !GET_STRING_ARG(2, Event, &tag) )
	return FALSE;
      ok = yaml_mapping_start_event_initialize(
	       &event, anchor, tag, 0, YAML_BLOCK_MAPPING_STYLE);
    } else if ( name == ATOM_mapping_end && arity == 0 )
    { ok = yaml_mapping_end_event_initialize(&event);
    } else if ( name == ATOM_sequence_start && arity == 2 )
    { yaml_char_t *anchor, *tag;
      if ( !GET_STRING_ARG(1, Event, &anchor) ||
	   !GET_STRING_ARG(2, Event, &tag) )
	return FALSE;
      ok = yaml_sequence_start_event_initialize(
	       &event, anchor, tag, 0, YAML_BLOCK_SEQUENCE_STYLE);
    } else if ( name == ATOM_sequence_end && arity == 0 )
    { ok = yaml_sequence_end_event_initialize(&event);
    } else if ( name == ATOM_scalar && arity == 4 )
    { yaml_char_t *anchor, *tag, *value = NULL;
      int implicit, style;
      atom_t style_a;

      if ( !GET_STRING_ARG(2, Event, &tag) ||
	   !PL_get_arg(1, Event, tmp) ||
	   !get_value(tmp, &value, &tag) ||
	   !GET_STRING_ARG(3, Event, &anchor) )
	return FALSE;

      if ( !PL_get_arg(4, Event, tmp) ||
	   !PL_get_atom_ex(tmp, &style_a) )
	return FALSE;

      if ( style_a == ATOM_plain )
	style = YAML_PLAIN_SCALAR_STYLE;
      else if ( style_a == ATOM_single_quoted )
	style = YAML_SINGLE_QUOTED_SCALAR_STYLE;
      else if ( style_a == ATOM_double_quoted )
	style = YAML_DOUBLE_QUOTED_SCALAR_STYLE;
      else if ( style_a == ATOM_literal )
	style = YAML_LITERAL_SCALAR_STYLE;
      else if ( style_a == ATOM_folded )
	style = YAML_FOLDED_SCALAR_STYLE;
      else
	return PL_domain_error("yaml_scalar_style", tmp);

      implicit = (tag == NULL);
      ok = yaml_scalar_event_initialize(
	       &event, anchor, tag, value, -1, implicit, implicit, style);
    } else if ( name == ATOM_alias && arity == 1 )
    { yaml_char_t *alias;

      if ( !GET_STRING_ARG(1, Event, &alias) )
	return FALSE;
      ok = yaml_alias_event_initialize(&event, alias);
    } else
      return PL_domain_error("yaml_event", Event);

    if ( ok )
    { if ( yaml_emitter_emit(emitter, &event) )
      { return TRUE;
      } else
      { term_t ex;

	if ( (ex=PL_new_term_ref()) &&
	     PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
				 PL_FUNCTOR, FUNCTOR_yaml_error2,
				   PL_INT, (int)emitter->error,
				   PL_STRING, emitter->problem,
				 PL_VARIABLE) )
	  PL_raise_exception(ex);
	return FALSE;
      }
    } else
    { return PL_resource_error("memory");
    }

    return TRUE;
  } else
    return PL_domain_error("yaml_event", Event);
}



		 /*******************************
		 *	      REGISTER		*
		 *******************************/

#define MKATOM(n) \
        ATOM_ ## n = PL_new_atom(#n)
#define MKFUNCTOR(n,a) \
        FUNCTOR_ ## n ## a = PL_new_functor(PL_new_atom(#n), a)

install_t
install_yaml4pl(void)
{ MKATOM(null);
  MKATOM(true);
  MKATOM(false);
  MKATOM(canonical);
  MKATOM(unicode);
  MKATOM(stream_start);
  MKATOM(stream_end);
  MKATOM(document_start);
  MKATOM(document_end);
  MKATOM(mapping_start);
  MKATOM(mapping_end);
  MKATOM(sequence_start);
  MKATOM(sequence_end);
  MKATOM(scalar);
  MKATOM(alias);
  MKATOM(plain);
  MKATOM(single_quoted);
  MKATOM(double_quoted);
  MKATOM(literal);
  MKATOM(folded);

  MKFUNCTOR(mapping, 3);
  MKFUNCTOR(sequence, 3);
  MKFUNCTOR(tag, 2);
  MKFUNCTOR(error, 2);
  MKFUNCTOR(yaml_error, 2);
  FUNCTOR_eq2 = PL_new_functor(PL_new_atom("="), 2);

  PL_register_foreign("yaml_parse_stream",   2,	yaml_parse_stream,   0);
  PL_register_foreign("yaml_emitter_create", 3,	yaml_emitter_create, 0);
  PL_register_foreign("yaml_emit_event",     2,	yaml_emit_event,     0);
}
