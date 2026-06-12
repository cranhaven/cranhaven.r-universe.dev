/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2015, University of Amsterdam
                              VU University Amsterdam
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

#define O_DEBUG 1
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include <zlib.h>

install_t install_zlib4pl(void);

static atom_t ATOM_format;		/* format(Format) */
static atom_t ATOM_level;		/* level(Int) */
static atom_t ATOM_close_parent;	/* close_parent(Bool) */
static atom_t ATOM_multi_part;		/* multi_part(Bool) */
static atom_t ATOM_gzip;
static atom_t ATOM_deflate;
static atom_t ATOM_raw_deflate;
static int debuglevel = 0;

#ifdef O_DEBUG
#define DEBUG(n, g) if ( debuglevel >= n ) g
#else
#define DEBUG(n, g) (void)0
#endif

		 /*******************************
		 *	       TYPES		*
		 *******************************/

#define BUFSIZE SIO_BUFSIZE		/* raw I/O buffer */

typedef enum
{ F_UNKNOWN = 0,
  F_GZIP,				/* gzip output */
  F_DEFLATE,				/* zlib data */
  F_RAW_DEFLATE
} zformat;

typedef struct z_context
{ IOSTREAM	   *stream;		/* Original stream */
  IOSTREAM	   *zstream;		/* Compressed stream (I'm handle of) */
  int		    close_parent;	/* close parent on close */
  int		    initialized;	/* did inflateInit()? */
  int		    multi_part;		/* Multipart gzip file */
  int		    z_stream_end;	/* Seen Z_STREAM_END */
  zformat	    format;		/* current format */
  z_stream	    zstate;		/* Zlib state */
  gz_header	    zhead;		/* Header */
} z_context;


static z_context*
alloc_zcontext(IOSTREAM *s)
{ z_context *ctx = PL_malloc(sizeof(*ctx));

  memset(ctx, 0, sizeof(*ctx));
  ctx->stream       = s;
  ctx->close_parent = TRUE;

  return ctx;
}


static void
free_zcontext(z_context *ctx)
{ if ( ctx->stream->upstream )
    Sset_filter(ctx->stream, NULL);
  else
    PL_release_stream(ctx->stream);

  PL_free(ctx);
}


		 /*******************************
		 *	       GZ I/O		*
		 *******************************/

static void
sync_stream(z_context *ctx)
{ IOPOS *pos;

  if ( (pos = ctx->stream->position) )
  { size_t bytes = (char*)ctx->zstate.next_in - ctx->stream->bufp;
    pos->byteno += bytes;
  }

  ctx->stream->bufp = (char*)ctx->zstate.next_in;
}


static ssize_t				/* inflate */
zread(void *handle, char *buf, size_t size)
{ z_context *ctx = handle;
  int rc;
  const char *msg = NULL;

  ctx->zstate.next_out  = (Bytef*)buf;
  ctx->zstate.avail_out = (uInt)size;

  if ( ctx->zstate.avail_in == 0 )
  { if ( ctx->z_stream_end )
      goto end_seen;

    if ( !Sfeof(ctx->stream) )
    { ctx->zstate.next_in  = (Bytef*)ctx->stream->bufp;
      ctx->zstate.avail_in = (uInt)(ctx->stream->limitp - ctx->stream->bufp);
      DEBUG(1, Sdprintf("Set avail_in to %d\n", (int)ctx->zstate.avail_in));
      DEBUG(2, { int i;
		 Sdprintf("Received:");
		 for(i=0; i<(int)ctx->zstate.avail_in; i++)
		   Sdprintf(" 0x%02x", ctx->zstate.next_in[i]&0xff);
		 Sdprintf("\n");
	       });
    }
  }

  DEBUG(1, Sdprintf("Processing %d bytes\n", (int)ctx->zstate.avail_in));

  if ( ctx->initialized == FALSE )
  { if ( ctx->format == F_GZIP )
    { rc = inflateInit2(&ctx->zstate, MAX_WBITS+16);
    } else if ( ctx->format == F_DEFLATE )
    { rc = inflateInit(&ctx->zstate);
    } else if ( ctx->format == F_RAW_DEFLATE )
    { rc = inflateInit2(&ctx->zstate, -MAX_WBITS);
    } else
    { memset(&ctx->zhead, 0, sizeof(ctx->zhead));
      rc = inflateInit2(&ctx->zstate, MAX_WBITS+32);
      rc = inflateGetHeader(&ctx->zstate, &ctx->zhead);
      if ( rc != Z_OK )
	Sdprintf("inflateGetHeader() failed\n");
    }
    ctx->initialized = TRUE;
    sync_stream(ctx);
  }

  rc = inflate(&ctx->zstate, Z_NO_FLUSH);
  sync_stream(ctx);

  switch( rc )
  { case Z_OK:
    { size_t n = (size_t)(size - ctx->zstate.avail_out);

      DEBUG(1, Sdprintf("inflate(): Z_OK: %zd bytes\n", n));

      if ( n == 0 )
      { /* If we get here then there was not enough data in the in buffer
           to decode a single character, but we are not at the end of the
           stream, so we must read
	   more from the parent
	*/
	DEBUG(1, Sdprintf("Not enough data to decode.  Retrying\n"));

	return zread(handle, buf, size);
      }

      return n;
    }
    case Z_STREAM_END:
    { size_t n;

      ctx->z_stream_end = TRUE;

    end_seen:
      n = (size_t)(size - ctx->zstate.avail_out);

      DEBUG(1, Sdprintf("Z_STREAM_END: %zd bytes\n", n));

      if ( n == 0 && ctx->multi_part )
      { if ( ctx->multi_part == -1 &&
	     ctx->zhead.done < 0 )
	  return n;			/* default multi_part and deflate */

	if ( Sfeof(ctx->stream) )
	  return n;

	DEBUG(1, Sdprintf("Multi-part gzip stream; restarting\n"));
	ctx->z_stream_end = FALSE;
	ctx->initialized = FALSE;		/* multiple zips */
	inflateEnd(&ctx->zstate);
	return zread(handle, buf, size);
      }

      return n;
    }
    case Z_NEED_DICT:
      DEBUG(1, Sdprintf("Z_NEED_DICT\n"));
      msg = "zlib: need dictionary";
      break;
    case Z_DATA_ERROR:
      DEBUG(1, Sdprintf("Z_DATA_ERROR\n"));
      msg = "zlib: corrupt input data";
      break;
    case Z_STREAM_ERROR:
      DEBUG(1, Sdprintf("Z_STREAM_ERROR\n"));
      msg = "zlib: inconsistent state";
      break;
    case Z_MEM_ERROR:
      DEBUG(1, Sdprintf("Z_MEM_ERROR\n"));
      msg = "zlib: not enough memory";
      break;
    case Z_BUF_ERROR:
      DEBUG(1, Sdprintf("Z_BUF_ERROR\n"));
      msg = "zlib: unexpected end-of-file";
      break;
    default:
      DEBUG(1, Sdprintf("Inflate error: %d\n", rc));
  }
  if ( ctx->zstate.msg )
    msg = ctx->zstate.msg;
  if ( msg )
    Sseterr(ctx->zstream, SIO_FERR, msg);
  return -1;
}


static ssize_t				/* deflate */
zwrite4(void *handle, char *buf, size_t size, int flush)
{ z_context *ctx = handle;
  Bytef buffer[SIO_BUFSIZE];
  int rc;
  int loops = 0;

  ctx->zstate.next_in = (Bytef*)buf;
  ctx->zstate.avail_in = (uInt)size;

  DEBUG(1, Sdprintf("Compressing %d bytes\n", (int)ctx->zstate.avail_in));
  do
  { loops++;
    ctx->zstate.next_out  = buffer;
    ctx->zstate.avail_out = sizeof(buffer);

    switch( (rc = deflate(&ctx->zstate, flush)) )
    { case Z_OK:
      case Z_STREAM_END:
      { size_t n = sizeof(buffer) - ctx->zstate.avail_out;

	DEBUG(1, Sdprintf("[%d] Compressed (%s) to %zd bytes; left %d\n",
			  loops, rc == Z_OK ? "Z_OK" : "Z_STREAM_END",
			  n, ctx->zstate.avail_in));
	if ( n > 0 )
	{ DEBUG(2, { int i;
		     Sdprintf("Sending:");
		     for(i=0; i<n; i++)
		       Sdprintf(" 0x%02x", buffer[i]&0xff);
		     Sdprintf("\n");
		   });

	  if ( Sfwrite(buffer, 1, n, ctx->stream) != n )
	    return -1;
	}

	break;
      }
      case Z_BUF_ERROR:
	DEBUG(1, Sdprintf("zwrite4(): Z_BUF_ERROR\n"));
        break;
      case Z_STREAM_ERROR:
      default:
	Sdprintf("ERROR: zwrite(): %s\n", ctx->zstate.msg);
	return -1;
    }
  } while ( ctx->zstate.avail_in > 0 ||
	    (flush != Z_NO_FLUSH && rc == Z_OK) );

  if ( flush != Z_NO_FLUSH && Sflush(ctx->stream) < 0 )
    return -1;

  return size;
}


static ssize_t				/* deflate */
zwrite(void *handle, char *buf, size_t size)
{ return zwrite4(handle, buf, size, Z_NO_FLUSH);
}


static int
zcontrol(void *handle, int op, void *data)
{ z_context *ctx = handle;

  switch(op)
  { case SIO_FLUSHOUTPUT:
      DEBUG(1, Sdprintf("Flushing output\n"));
      return (int)zwrite4(handle, NULL, 0, Z_SYNC_FLUSH);
    case SIO_SETENCODING:
      return 0;				/* allow switching encoding */
    default:
    { IOSTREAM *parent = ctx->stream;
      if ( parent && parent->magic == SIO_MAGIC )
      { IOFUNCTIONS *funcs;
	Scontrol_function ctrl;

	if ( (funcs=parent->functions) &&
	     (ctrl=funcs->control) )
	  return (*ctrl)(parent->handle, op, data);
      }
      return -1;
    }
  }
}


static int
zclose(void *handle)
{ z_context *ctx = handle;
  ssize_t rc;

  DEBUG(1, Sdprintf("zclose() ...\n"));

  if ( (ctx->stream->flags & SIO_INPUT) )
  { if ( ctx->initialized == TRUE )
      rc = inflateEnd(&ctx->zstate);
    else
      rc = Z_OK;
  } else
  { rc = zwrite4(handle, NULL, 0, Z_FINISH);	/* flush */
    if ( rc == 0 )
      rc = deflateEnd(&ctx->zstate);
    else
      deflateEnd(&ctx->zstate);
  }

  switch(rc)
  { case Z_OK:
      DEBUG(1, Sdprintf("%s(): Z_OK\n",
			(ctx->stream->flags & SIO_INPUT) ? "inflateEnd"
							 : "deflateEnd"));
      if ( ctx->close_parent )
      { IOSTREAM *parent = ctx->stream;
	free_zcontext(ctx);
	return Sclose(parent);
      } else
      { free_zcontext(ctx);
	return 0;
      }
    case Z_STREAM_ERROR:		/* inconsistent state */
    case Z_DATA_ERROR:			/* premature end */
    default:
      if ( ctx->close_parent )
      { IOSTREAM *parent = ctx->stream;
	free_zcontext(ctx);
	Sclose(parent);
	return -1;
      }

      free_zcontext(ctx);
      return -1;
  }
}


static IOFUNCTIONS zfunctions =
{ zread,
  zwrite,
  NULL,					/* seek */
  zclose,
  zcontrol,				/* zcontrol */
  NULL,					/* seek64 */
};


		 /*******************************
		 *	 PROLOG CONNECTION	*
		 *******************************/

#define COPY_FLAGS (SIO_INPUT|SIO_OUTPUT| \
		    SIO_TEXT| \
		    SIO_REPXML|SIO_REPPL|\
		    SIO_RECORDPOS)

static foreign_t
pl_zopen(term_t org, term_t new, term_t options)
{ term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  z_context *ctx;
  zformat fmt = F_UNKNOWN;
  int level = Z_DEFAULT_COMPRESSION;
  IOSTREAM *s, *s2;
  int close_parent = TRUE;
  int multi_part = -1;			/* default */

  while(PL_get_list(tail, head, tail))
  { atom_t name;
    size_t arity;
    term_t arg = PL_new_term_ref();

    if ( !PL_get_name_arity(head, &name, &arity) || arity != 1 )
      return PL_type_error("option", head);
    _PL_get_arg(1, head, arg);

    if ( name == ATOM_format )
    { atom_t a;

      if ( !PL_get_atom_ex(arg, &a) )
	return FALSE;
      if ( a == ATOM_gzip )
	fmt = F_GZIP;
      else if ( a == ATOM_deflate )
	fmt = F_DEFLATE;
      else if ( a == ATOM_raw_deflate )
        fmt = F_RAW_DEFLATE;
      else
	return PL_domain_error("compression_format", arg);
    } else if ( name == ATOM_level )
    { if ( !PL_get_integer_ex(arg, &level) )
	return FALSE;
      if ( level < 0 || level > 9 )
	return PL_domain_error("compression_level", arg);
    } else if ( name == ATOM_close_parent )
    { if ( !PL_get_bool_ex(arg, &close_parent) )
	return FALSE;
    } else if ( name == ATOM_multi_part )
    { if ( !PL_get_bool_ex(arg, &multi_part) )
	return FALSE;
    }
  }
  if ( !PL_get_nil_ex(tail) )
    return FALSE;

  if ( !PL_get_stream_handle(org, &s) )
    return FALSE;			/* Error */
  ctx = alloc_zcontext(s);
  ctx->close_parent = close_parent;
  ctx->multi_part = multi_part;
  ctx->format = fmt;
  if ( (s->flags & SIO_OUTPUT) )
  { int rc;

    if ( fmt == F_GZIP )
    { rc = deflateInit2(&ctx->zstate, level, Z_DEFLATED, MAX_WBITS+16, MAX_MEM_LEVEL, 0);
    } else if ( fmt == F_RAW_DEFLATE )
    { rc = deflateInit2(&ctx->zstate, level, Z_DEFLATED, -MAX_WBITS, MAX_MEM_LEVEL, 0);
    } else
    { rc = deflateInit(&ctx->zstate, level);
    }

    if ( rc != Z_OK )
    { free_zcontext(ctx);
      return FALSE;			/* TBD: Error */
    }
  }

  if ( !(s2 = Snew(ctx,
		   (s->flags&COPY_FLAGS)|SIO_FBUF,
		   &zfunctions))	)
  { free_zcontext(ctx);			/* no memory */

    return FALSE;
  }

  s2->encoding = s->encoding;
  ctx->zstream = s2;
  Sset_filter(s, s2);
  PL_release_stream(s);
  if ( PL_unify_stream(new, s2) )
  { return TRUE;
  } else
  { ctx->close_parent = FALSE;
    Sclose(s2);
    return PL_instantiation_error(new);	/* actually, over instantiation */
  }
}


#ifdef O_DEBUG
static foreign_t
zdebug(term_t level)
{ return PL_get_integer(level, &debuglevel);
}
#endif

		 /*******************************
		 *	       INSTALL		*
		 *******************************/

#define MKFUNCTOR(name, arity) PL_new_functor(PL_new_atom(name), arity)

install_t
install_zlib4pl(void)
{ ATOM_format       = PL_new_atom("format");
  ATOM_level        = PL_new_atom("level");
  ATOM_close_parent = PL_new_atom("close_parent");
  ATOM_gzip	    = PL_new_atom("gzip");
  ATOM_deflate	    = PL_new_atom("deflate");
  ATOM_raw_deflate  = PL_new_atom("raw_deflate");
  ATOM_multi_part   = PL_new_atom("multi_part");

  PL_register_foreign("zopen",  3, pl_zopen,  0);
#ifdef O_DEBUG
  PL_register_foreign("zdebug", 1, zdebug, 0);
#endif
}
