/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2012-2025, VU University Amsterdam
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
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#ifdef __WINDOWS__
#define LIBARCHIVE_STATIC 1
#endif
#include <stdbool.h>
#include <archive.h>
#include <archive_entry.h>
#include <assert.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>

#if ARCHIVE_VERSION_NUMBER < 3000000
#error "Requires libarchive 3.0.0 or later"
#endif


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		Design of the libarchive interface

An archive is represented by a   symbol (blob). The C-structure contains
the archive, the current header and some state information.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define ARCHIVE_MAGIC 348184378

/* The normal way of calling the predicates (similar for both read and
   write).  [`ar` is the archive_wrapper blob]

   For reading (ar->entry is allocated by archive_read_next_header):
     archive_open_stream(Stream, read, Archive, Options),  % AR_VIRGIN -> AR_OPENED_ARCHIVE
     repeat:
       archive_next_header(Stream, Name),                  % AR_OPENED_ARCHIVE -> AR_NEW_ENTRY
       archive_header_prop_(Archive, Property),
       archive_open_entry(Archive, EntryStream),           % AR_NEW_ENTRY -> AR_OPENED_ENTRY
       read(EntryStream, ...), get_code(EntryStream,Code), etc.
       close(EntryStream),           % ar_entry_close_cb() % AR_OPENED_ENTRY _> AR_OPENED_ARCHIVE
     archive_close(Archive),                               % ... -> AR_VIRGIN
     close(Stream)

   For writing (ar->entry is allocated by archive_entry_new()):
     archive_open_stream(Stream, write, Archive, Options), % AR_VIRGIN -> AR_OPENED_ARCHIVE
     repeat:
       archive_next_header(Stream, Name),                  % AR_OPENED_ARCHIVE -> AR_NEW_ENTRY
       archive_set_header_property(Archive, Property),
       archive_open_entry(Archive, EntryStream),           % AR_NEW_ENTRY -> AR_OPENED_ENTRY
       write(EntryStream, ...), format(EntryStream, ...), etc.
       close(EntryStream),           % ar_entry_close_cb() % AR_OPENED_ENTRY _> AR_OPENED_ARCHIVE
     archive_close(Archive),                               % ... -> AR_VIRGIN
     close(Stream)

   When the `ar` blob is created, it stores the Stream in ar->data by
   calling PL_get_stream(). This increases the reference count for
   the stream; it's decremented by PL_release_stream(ar->data).

   When an EntryStream is created (by archive_open_entry/2) and a
   write is done on the stream, the following calls and callbacks
   happen for write (similar for read):
      prolog:write(EntryStream)
      archive4pl:ar_entry_write_cb()
      libarchive:archive_write_data()
      archive4pl:libarchive_write_cb()
      Sfwrite(ar->data)

   EntryStream needs `ar`, so archive_open_entry/2 ends up calling
   PL_register_atom(ar->symbol) to ensure it doesn't get gc-ed.

   The close for EntryStream - ar_entry_close_cb() - calls
   archive_write_finish_entry() [for write]. It also calls
   PL_unregister_atom(ar->symbol).

   [The documentation for archive_write_finish_entry() says that it is
   implicitly called by archive_write_header() or
   archive_write_close(). However, it's simpler to explicitly call it
   when closing the EntryStream.]

   archive_close(Archive) calls archive_{read,write}_free(), which
   calls archive_{read,write}_close(), which calls
   libarchive_close_cb().

   When the `ar` blob is released, it needs to free the
   archive_entry struct, close the entry stream (EntryStream),
   and close/release the archive Stream (ar->data).
 */
typedef enum ar_status
{ AR_VIRGIN = 0,
  AR_OPENED_ARCHIVE,
  AR_NEW_ENTRY,
  AR_OPENED_ENTRY,
  AR_CLOSED_ENTRY,
  AR_ERROR
} ar_status;

typedef struct archive_wrapper
{ atom_t		symbol;		/* Associated symbol */
  IOSTREAM *		data;		/* Underlying stream */
  unsigned int		type;		/* Types of formats/filters supported */
  int			magic;		/* magic code */
  ar_status		status;		/* Current status */
  int			close_parent;	/* Close the parent handle */
  int			closed_archive;	/* Archive was closed with open entry */
  struct archive *	archive;	/* Actual archive handle */
  struct archive_entry *entry;		/* Current entry */
  int			how;		/* r/w mode ('r' or 'w') */
  int			agc;		/* subject to AGC */
} archive_wrapper;

/* Convenience function - sets ar->status to AR_ERROR and returns false.
   It is intended to be wrapped around PL_xxx_error() calls.  If
   ar->status is set to AR_ERROR, that all further use of ar will
   throw an error - this may be overkill for some errors. */

static bool
ar_set_status_error(archive_wrapper *ar, bool rc)
{ if ( ar )
    ar->status = AR_ERROR;
  return false;
}

static archive_wrapper archive_wrapper_init_value =
{
  0,			/* symbol		*/
  NULL,			/* data			*/
  AR_VIRGIN,		/* type			*/
  ARCHIVE_MAGIC,	/* magic		*/
  AR_VIRGIN,		/* status		*/
  FALSE,		/* close_parent		*/
  FALSE,		/* closed_archive	*/
  NULL,			/* archive		*/
  NULL,			/* entry		*/
  ' ',			/* how			*/
  FALSE			/* agc			*/
};

#if 0
/* For debugging: */
static const char*
ar_status_str(ar_status status)
{ switch ( status )
  { case AR_VIRGIN:         return "AR_VIRGIN";
    case AR_OPENED_ARCHIVE: return "AR_OPENED";
    case AR_NEW_ENTRY:      return "AR_NEW_ENTRY";
    case AR_OPENED_ENTRY:   return "AR_OPENED_ENTRY";
    case AR_CLOSED_ENTRY:   return "AR_CLOSED_ENTRY";
    case AR_ERROR:          return "AR_ERROR";
    default:                return "AR_???";
  }
}
#endif



		 /*******************************
		 *	      CONSTANTS		*
		 *******************************/

static atom_t ATOM_close_parent;
static atom_t ATOM_compression;
static atom_t ATOM_filter;
static atom_t ATOM_format;
static atom_t ATOM_all;
static atom_t ATOM_bzip2;
static atom_t ATOM_compress;
static atom_t ATOM_gzip;
static atom_t ATOM_grzip;
static atom_t ATOM_lrzip;
static atom_t ATOM_lzip;
static atom_t ATOM_lzma;
static atom_t ATOM_lzop;
static atom_t ATOM_none;
static atom_t ATOM_rpm;
static atom_t ATOM_uu;
static atom_t ATOM_xz;
static atom_t ATOM_7zip;
static atom_t ATOM_ar;
static atom_t ATOM_cab;
static atom_t ATOM_cpio;
static atom_t ATOM_empty;
static atom_t ATOM_gnutar;
static atom_t ATOM_iso9660;
static atom_t ATOM_lha;
static atom_t ATOM_mtree;
static atom_t ATOM_rar;
static atom_t ATOM_raw;
static atom_t ATOM_tar;
static atom_t ATOM_xar;
static atom_t ATOM_zip;
static atom_t ATOM_file;
static atom_t ATOM_link;
static atom_t ATOM_socket;
static atom_t ATOM_character_device;
static atom_t ATOM_block_device;
static atom_t ATOM_directory;
static atom_t ATOM_fifo;
static atom_t ATOM_read;
static atom_t ATOM_write;

static functor_t FUNCTOR_error2;
static functor_t FUNCTOR_archive_error2;
static functor_t FUNCTOR_existence_error3;
static functor_t FUNCTOR_filetype1;
static functor_t FUNCTOR_format1;
static functor_t FUNCTOR_mtime1;
static functor_t FUNCTOR_size1;
static functor_t FUNCTOR_link_target1;
static functor_t FUNCTOR_permissions1;

static
int PL_existence_error3(const char* type, const char* object, term_t in)
{ term_t ex = PL_new_term_ref();

  if ( PL_unify_term(ex,
                     PL_FUNCTOR, FUNCTOR_error2,
                     PL_FUNCTOR, FUNCTOR_existence_error3,
                     PL_CHARS, type,
                     PL_CHARS, object,
                     PL_TERM, in,
                     PL_VARIABLE))
    return PL_raise_exception(ex);
  return FALSE;
}

		 /*******************************
		 *	  SYMBOL WRAPPER	*
		 *******************************/

static int archive_free_handle(archive_wrapper *ar)
{ int rc;
  /* It's safe to pass NULL to archive_read_free(), archive_write_free() */
  if ( ar->how == 'r' )
    rc = archive_read_free(ar->archive);
  else
    rc = archive_write_free(ar->archive);
  ar->archive = NULL;
  return rc;
}

static void
ar_w_acquire_cb(atom_t symbol)
{ archive_wrapper *ar = PL_blob_data(symbol, NULL, NULL);
  ar->symbol = symbol;
}

/* Callback from atom gc */
static int
ar_w_release_cb(atom_t symbol)
{ archive_wrapper *ar = PL_blob_data(symbol, NULL, NULL);

  assert(ar->magic == ARCHIVE_MAGIC);
  ar->agc = TRUE;

  /* TODO: The following assert isn't always true */
  /* assert(ar->status != AR_OPENED_ENTRY); */

  /* Only writeable archives have ar->entry allocated by archive_entry_new() */
  if (ar->how == 'w')
    archive_entry_free(ar->entry); /* Safe even if !ar->entry */
  archive_free_handle(ar);

  return TRUE;
}

static int
ar_w_compare_cb(atom_t a, atom_t b)
{ const archive_wrapper *ara = PL_blob_data(a, NULL, NULL);
  const archive_wrapper *arb = PL_blob_data(b, NULL, NULL);

  return ( ara > arb ?  1 :
	   ara < arb ? -1 : 0
	 );
}

static int
ar_w_write_cb(IOSTREAM *s, atom_t symbol, int flags)
{ const archive_wrapper *ar = PL_blob_data(symbol, NULL, NULL);

  Sfprintf(s, "<archive>(%p)", ar);

  return TRUE;
}

static PL_blob_t archive_blob =
{ PL_BLOB_MAGIC,
  0,
  "archive",
  ar_w_release_cb,
  ar_w_compare_cb,
  ar_w_write_cb,
  ar_w_acquire_cb
};


static bool
get_archive(term_t t, archive_wrapper **arp)
{ PL_blob_t *type;
  void *data;

  if ( PL_get_blob(t, &data, NULL, &type) && type == &archive_blob)
  { archive_wrapper *ar = data;

    assert(ar->magic == ARCHIVE_MAGIC);

    if ( ar->symbol )
    { *arp = ar;

      return true;
    }

    return ar_set_status_error(ar, PL_permission_error("access", "closed_archive", t));
  }

  return PL_type_error("archive", t),false;
}



		 /*******************************
		 *	      CALLBACKS		*
		 *******************************/

/* Callback from archive_XXX_close() or archive_XXX_free() */
static int
libarchive_open_cb(struct archive *a, void *cdata)
{ return ARCHIVE_OK;
}

static int
libarchive_close_cb(struct archive *a, void *cdata)
{ archive_wrapper *ar = cdata;
  PL_release_stream(ar->data);
  if ( ar->close_parent && ar->archive )
  { if ( Sgcclose(ar->data, ar->agc ? SIO_CLOSE_FORCE : 0) != 0 )
    { archive_set_error(ar->archive, errno, "Close failed");
      ar->data = NULL;
      return ARCHIVE_FATAL;
    }
    ar->data = NULL;
  }

  return ARCHIVE_OK;				/* TBD: close_parent */
}

static ssize_t
libarchive_read_cb(struct archive *a, void *cdata, const void **buffer)
{ const archive_wrapper *ar = cdata;
  /* In the folowing code, Sfeof() call S__fillbuff() if the buffer is empty.
     TODO: Why is the code written this way instead of using Sfread()?
           One reason could be that apparently libarchive doesn't
           provide the buffer to dump the data in so need to maintain
           such a buffer ourselves and copy the data or use the
           stream's buffer.  Looks a little hacky.  On the other hand,
           there is little wrong with it and if something changes the
           tests will tell us. */
  if ( Sfeof(ar->data) )
  { if ( Sferror(ar->data) )
      return -1;
    return 0;
  } else
  { ssize_t bytes = ar->data->limitp - ar->data->bufp;

    *buffer = ar->data->bufp;
    ar->data->bufp = ar->data->limitp;
    ar->data->position->byteno += bytes;

    return bytes;
  }
}

static ssize_t
libarchive_write_cb(struct archive *a, void *cdata, const void *buffer, size_t n)
{ const archive_wrapper *ar = cdata;
  return Sfwrite(buffer, 1, n, ar->data);
}

static int64_t
libarchive_skip_cb(struct archive *a, void *cdata, int64_t request)
{ archive_wrapper *ar = cdata;

  if ( Sseek64(ar->data, request, SIO_SEEK_CUR) == 0 )
    return request;
  Sclearerr(ar->data);			/* TODO: why is this? */

  return 0;				/* cannot skip; library will read */
}

#ifdef HAVE_ARCHIVE_READ_SET_SEEK_CALLBACK
static int64_t
libarchive_seek_cb(struct archive *a, void *cdata, int64_t request, int whence)
{ archive_wrapper *ar = cdata;
  int s_whence;

  switch (whence) {
    case SEEK_SET: s_whence=SIO_SEEK_SET; break;
    case SEEK_CUR: s_whence=SIO_SEEK_CUR; break;
    case SEEK_END: s_whence=SIO_SEEK_END; break;
    default:	   assert(0); return -1;
  }

  if ( Sseek64(ar->data, request, s_whence) == 0 ) {
    return Stell64(ar->data);
  }
  Sclearerr(ar->data);				/* JW: TODO: why is this? */

  return ARCHIVE_FATAL;
}
#endif


		 /*******************************
		 *	      PROLOG		*
		 *******************************/

static int
archive_error(const archive_wrapper *ar, int rc)
{ int eno = archive_errno(ar->archive);
  const char *s = archive_error_string(ar->archive);
  term_t ex;

  if ( eno == 0 )
    eno = rc;
  if ( !s )
  { switch( rc )
    { case ARCHIVE_EOF:    s = "eof";    break;
      case ARCHIVE_OK:     s = "ok";     break;
      case ARCHIVE_RETRY:  s = "retry";  break;
      case ARCHIVE_WARN:   s = "warn";   break;
      case ARCHIVE_FAILED: s = "failed"; break;
      case ARCHIVE_FATAL:  s = "fatal";  break;
      default:		   s = "unknown";
    }
  }

  if ( ( (ex = PL_new_term_ref()) &&
	 PL_unify_term(ex,
		       PL_FUNCTOR, FUNCTOR_error2,
			 PL_FUNCTOR, FUNCTOR_archive_error2,
			   PL_INT, eno,
			   PL_CHARS, s,
			 PL_VARIABLE) ) )
    return PL_raise_exception(ex);

  return FALSE;
}

#define	FILTER_ALL	  0x0000ffff
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FILTER_BZIP2
#define	FILTER_BZIP2	  0x00000001
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FILTER_COMPRESS
#define	FILTER_COMPRESS 0x00000002
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FILTER_GZIP
#define	FILTER_GZIP	  0x00000004
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FILTER_GRZIP
#define	FILTER_GRZIP	  0x00000008
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FILTER_LRZIP
#define	FILTER_LRZIP	  0x00000010
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FILTER_LZIP
#define	FILTER_LZIP	  0x00000020
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FILTER_LZMA
#define	FILTER_LZMA	  0x00000040
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FILTER_LZOP
#define	FILTER_LZOP	  0x00000080
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FILTER_NONE
#define	FILTER_NONE	  0x00000100
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FILTER_RPM
#define	FILTER_RPM	  0x00000200
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FILTER_UU
#define	FILTER_UU	  0x00000400
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FILTER_XZ
#define	FILTER_XZ	  0x00000800
#endif

#define FILTER_MASK       0x0000ffff

#ifdef HAVE_ARCHIVE_READ_SUPPORT_FORMAT_7ZIP
#define FORMAT_7ZIP	  0x00010000
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FORMAT_AR
#define FORMAT_AR	  0x00020000
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FORMAT_CAB
#define FORMAT_CAB	  0x00040000
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FORMAT_CPIO
#define FORMAT_CPIO	  0x00080000
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FORMAT_EMPTY
#define FORMAT_EMPTY	  0x00100000
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FORMAT_GNUTAR
#define FORMAT_GNUTAR	  0x00200000
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FORMAT_ISO9660
#define FORMAT_ISO9660	  0x00400000
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FORMAT_LHA
#define FORMAT_LHA	  0x00800000
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FORMAT_MTREE
#define FORMAT_MTREE	  0x01000000
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FORMAT_RAR
#define FORMAT_RAR	  0x02000000
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FORMAT_RAW
#define FORMAT_RAW	  0x04000000
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FORMAT_TAR
#define FORMAT_TAR	  0x08000000
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FORMAT_XAR
#define FORMAT_XAR	  0x10000000
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FORMAT_ZIP
#define FORMAT_ZIP	  0x20000000
#endif

#define FORMAT_MASK	  0xffff0000
#if defined(FORMAT_RAW) && defined(FORMAT_MTREE)
#define FORMAT_ALL	  (FORMAT_MASK&~(FORMAT_RAW|FORMAT_MTREE))
#else
/* Compile without these.  Make sure the constants are as above */
#define FORMAT_ALL	  (FORMAT_MASK&~(0x04000000|0x01000000))
#endif

static void
enable_type(archive_wrapper *ar, int type,
	    int (*f)(struct archive *ar))
{ if ( (ar->type & type) )
  { if ( (*f)(ar->archive) != ARCHIVE_OK )
      ar->type &= ~type;
  }
}

static foreign_t
archive_open_stream(term_t data, term_t mode, term_t handle, term_t options)
{ archive_wrapper *ar;
  term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  term_t arg  = PL_new_term_ref();
  int rc = ARCHIVE_OK;				/* silence compiler */

  /* If you make any changes here, also change archive_close() */

  { archive_wrapper ar_local;
    atom_t a;
    ar_local = archive_wrapper_init_value;

    if ( !PL_unify_blob(handle, &ar_local, sizeof ar_local, &archive_blob) ||
         !PL_get_atom_ex(handle, &a) )
      return FALSE;
    ar = PL_blob_data(a, NULL, NULL);
  }


  { atom_t mname;
    int flags;

    if ( PL_get_atom_ex(mode, &mname) )
    { if ( mname == ATOM_write )
      { ar->how = 'w';
        flags = SIO_OUTPUT;
      } else if ( mname == ATOM_read )
      { ar->how = 'r';
        flags = SIO_INPUT;
      } else
      { return ar_set_status_error(ar, PL_domain_error("io_mode", mode));
      }
    } else
    { return FALSE;
    }

    if ( !PL_get_stream(data, &ar->data, flags) )
      return FALSE;
  }

  while( PL_get_list_ex(tail, head, tail) )
  { atom_t name;
    size_t arity;

    if ( !PL_get_name_arity(head, &name, &arity) ||
	 !PL_get_arg(1, head, arg) )
      return ar_set_status_error(ar, PL_type_error("option", head));
    if ( name == ATOM_compression || name == ATOM_filter )
    { atom_t c;

      if ( !PL_get_atom_ex(arg, &c) )
	return FALSE;
      if ( ar->how == 'w' && ((ar->type & FILTER_MASK) != 0) )
        return ar_set_status_error(ar, PL_permission_error("set", "filter", arg));
      if ( c == ATOM_all )
      { if (ar->how == 'w')
          return ar_set_status_error(ar, PL_domain_error("write_filter", arg));
	ar->type |= FILTER_ALL;
      }
#ifdef FILTER_BZIP2
      else if ( c == ATOM_bzip2 )
	ar->type |= FILTER_BZIP2;
#endif
#ifdef FILTER_COMPRESS
      else if ( c == ATOM_compress )
	ar->type |= FILTER_COMPRESS;
#endif
#ifdef FILTER_GZIP
      else if ( c == ATOM_gzip )
	ar->type |= FILTER_GZIP;
#endif
#ifdef FILTER_GRZIP
      else if ( c == ATOM_grzip )
	ar->type |= FILTER_GRZIP;
#endif
#ifdef FILTER_LRZIP
      else if ( c == ATOM_lrzip )
	ar->type |= FILTER_LRZIP;
#endif
#ifdef FILTER_LZIP
      else if ( c == ATOM_lzip )
	ar->type |= FILTER_LZIP;
#endif
#ifdef FILTER_LZMA
      else if ( c == ATOM_lzma )
	ar->type |= FILTER_LZMA;
#endif
#ifdef FILTER_LZOP
      else if ( c == ATOM_lzop )
	ar->type |= FILTER_LZOP;
#endif
#ifdef FILTER_NONE
      else if ( c == ATOM_none )
	ar->type |= FILTER_NONE;
#endif
#ifdef FILTER_RPM
      else if ( c == ATOM_rpm )
	ar->type |= FILTER_RPM;
#endif
#ifdef FILTER_UU
      else if ( c == ATOM_uu )
	ar->type |= FILTER_UU;
#endif
#ifdef FILTER_XZ
      else if ( c == ATOM_xz )
	ar->type |= FILTER_XZ;
#endif
      else
	return ar_set_status_error(ar, PL_domain_error("filter", arg));
    } else if ( name == ATOM_format )
    { atom_t f;

      if ( !PL_get_atom_ex(arg, &f) )
	return FALSE;
      if ( ar->how == 'w' && (( ar->type & FORMAT_MASK ) != 0 ) )
        return ar_set_status_error(ar, PL_permission_error("set", "format", arg));
      if ( f == ATOM_all )
      { if ( ar->how == 'w' )
          return ar_set_status_error(ar, PL_domain_error("write_format", arg));
	ar->type |= FORMAT_ALL;
      }
#ifdef FORMAT_7ZIP
      else if ( f == ATOM_7zip )
	ar->type |= FORMAT_7ZIP;
#endif
#ifdef FORMAT_AR
      else if ( f == ATOM_ar )
	ar->type |= FORMAT_AR;
#endif
#ifdef FORMAT_CAB
      else if ( f == ATOM_cab )
	ar->type |= FORMAT_CAB;
#endif
#ifdef FORMAT_CPIO
      else if ( f == ATOM_cpio )
	ar->type |= FORMAT_CPIO;
#endif
#ifdef FORMAT_EMPTY
      else if ( f == ATOM_empty )
	ar->type |= FORMAT_EMPTY;
#endif
#ifdef FORMAT_GNUTAR
      else if ( f == ATOM_gnutar )
	ar->type |= FORMAT_GNUTAR;
#endif
#ifdef FORMAT_ISO9660
      else if ( f == ATOM_iso9660 )
	ar->type |= FORMAT_ISO9660;
#endif
#ifdef FORMAT_LHA
      else if ( f == ATOM_lha )
	ar->type |= FORMAT_LHA;
#endif
#ifdef FORMAT_MTREE
      else if ( f == ATOM_mtree )
	ar->type |= FORMAT_MTREE;
#endif
#ifdef FORMAT_RAR
      else if ( f == ATOM_rar )
	ar->type |= FORMAT_RAR;
#endif
#ifdef FORMAT_RAW
      else if ( f == ATOM_raw )
	ar->type |= FORMAT_RAW;
#endif
#ifdef FORMAT_TAR
      else if ( f == ATOM_tar )
	ar->type |= FORMAT_TAR;
#endif
#ifdef FORMAT_XAR
      else if ( f == ATOM_xar )
	ar->type |= FORMAT_XAR;
#endif
#ifdef FORMAT_ZIP
      else if ( f == ATOM_zip )
	ar->type |= FORMAT_ZIP;
#endif
      else
	return ar_set_status_error(ar, PL_domain_error("format", arg));
    } else if ( name == ATOM_close_parent )
    { if ( !PL_get_bool_ex(arg, &ar->close_parent) )
	return FALSE;
    }
  }
  if ( !PL_get_nil_ex(tail) )
    return FALSE;

  if ( ar->how == 'r' )
  { if ( !(ar->type & FILTER_ALL) )
      ar->type |= FILTER_ALL;
    if ( !(ar->type & FORMAT_MASK) )
      ar->type |= FORMAT_ALL;
  }
  if ( ar->how == 'r' )
  { if ( !(ar->archive = archive_read_new()) )
      return ar_set_status_error(ar, PL_resource_error("memory"));

     if ( (ar->type & FILTER_ALL) == FILTER_ALL )
     { archive_read_support_filter_all(ar->archive);
     } else
     {
#ifdef FILTER_BZIP2
       enable_type(ar, FILTER_BZIP2,    archive_read_support_filter_bzip2);
#endif
#ifdef FILTER_COMPRESS
       enable_type(ar, FILTER_COMPRESS, archive_read_support_filter_compress);
#endif
#ifdef FILTER_GZIP
       enable_type(ar, FILTER_GZIP,     archive_read_support_filter_gzip);
#endif
#ifdef FILTER_GRZIP
       enable_type(ar, FILTER_GRZIP,     archive_read_support_filter_grzip);
#endif
#ifdef FILTER_LRZIP
       enable_type(ar, FILTER_LRZIP,     archive_read_support_filter_lrzip);
#endif
#ifdef FILTER_LZIP
       enable_type(ar, FILTER_LZIP,     archive_read_support_filter_lzip);
#endif
#ifdef FILTER_LZMA
       enable_type(ar, FILTER_LZMA,     archive_read_support_filter_lzma);
#endif
#ifdef FILTER_LZOP
       enable_type(ar, FILTER_LZOP,     archive_read_support_filter_lzop);
#endif
#ifdef FILTER_NONE
       enable_type(ar, FILTER_NONE,     archive_read_support_filter_none);
#endif
#ifdef FILTER_RPM
       enable_type(ar, FILTER_RPM,      archive_read_support_filter_rpm);
#endif
#ifdef FILTER_UU
       enable_type(ar, FILTER_UU,       archive_read_support_filter_uu);
#endif
#ifdef FILTER_XZ
       enable_type(ar, FILTER_XZ,       archive_read_support_filter_xz);
#endif
     }

     if ( (ar->type & FORMAT_ALL) == FORMAT_ALL )
     { archive_read_support_format_all(ar->archive);
#ifdef FORMAT_RAW
       enable_type(ar, FORMAT_RAW,     archive_read_support_format_raw);
#endif
     } else
     {
#ifdef FORMAT_7ZIP
       enable_type(ar, FORMAT_7ZIP,    archive_read_support_format_7zip);
#endif
#ifdef FORMAT_AR
       enable_type(ar, FORMAT_AR,      archive_read_support_format_ar);
#endif
#ifdef FORMAT_CAB
       enable_type(ar, FORMAT_CAB,     archive_read_support_format_cab);
#endif
#ifdef FORMAT_CPIO
       enable_type(ar, FORMAT_CPIO,    archive_read_support_format_cpio);
#endif
#ifdef FORMAT_EMPTY
       enable_type(ar, FORMAT_EMPTY,   archive_read_support_format_empty);
#endif
#ifdef FORMAT_GNUTAR
       enable_type(ar, FORMAT_GNUTAR,  archive_read_support_format_gnutar);
#endif
#ifdef FORMAT_ISO9660
       enable_type(ar, FORMAT_ISO9660, archive_read_support_format_iso9660);
#endif
#ifdef FORMAT_LHA
       enable_type(ar, FORMAT_LHA,     archive_read_support_format_lha);
#endif
#ifdef FORMAT_MTREE
       enable_type(ar, FORMAT_MTREE,   archive_read_support_format_mtree);
#endif
#ifdef FORMAT_RAR
       enable_type(ar, FORMAT_RAR,     archive_read_support_format_rar);
#endif
#ifdef FORMAT_RAW
       enable_type(ar, FORMAT_RAW,     archive_read_support_format_raw);
#endif
#ifdef FORMAT_TAR
       enable_type(ar, FORMAT_TAR,     archive_read_support_format_tar);
#endif
#ifdef FORMAT_XAR
       enable_type(ar, FORMAT_XAR,     archive_read_support_format_xar);
#endif
#ifdef FORMAT_ZIP
       enable_type(ar, FORMAT_ZIP,     archive_read_support_format_zip);
#endif
     }
     archive_read_set_callback_data(ar->archive, ar);
     archive_read_set_open_callback(ar->archive, libarchive_open_cb);
     archive_read_set_read_callback(ar->archive, libarchive_read_cb);
     archive_read_set_skip_callback(ar->archive, libarchive_skip_cb);
#ifdef HAVE_ARCHIVE_READ_SET_SEEK_CALLBACK
     archive_read_set_seek_callback(ar->archive, libarchive_seek_cb);
#endif
     archive_read_set_close_callback(ar->archive, libarchive_close_cb);

     if ( (rc=archive_read_open1(ar->archive)) == ARCHIVE_OK )
     { ar->status = AR_OPENED_ARCHIVE;
       return TRUE;
     }
  } else if ( ar->how == 'w' )
  { if ( !(ar->archive = archive_write_new()) )
      return ar_set_status_error(ar, PL_resource_error("memory"));
     /* Prevent libarchive from padding the last block to 10240 bytes. Some decompressors,
       notably Oracle's jar decompressor, fail when presented with this */
     archive_write_set_bytes_in_last_block(ar->archive, 1);
     if (0) {}
#ifdef FORMAT_7ZIP
     else if ( ar->type & FORMAT_7ZIP )    archive_write_set_format_7zip(ar->archive);
#endif
#ifdef FORMAT_CPIO
     else if ( ar->type & FORMAT_CPIO )    archive_write_set_format_cpio(ar->archive);
#endif
#ifdef FORMAT_GNUTAR
     else if ( ar->type & FORMAT_GNUTAR )  archive_write_set_format_gnutar(ar->archive);
#endif
#ifdef FORMAT_ISO9660
     else if ( ar->type & FORMAT_ISO9660 ) archive_write_set_format_iso9660(ar->archive);
#endif
#ifdef FORMAT_XAR
     else if ( ar->type & FORMAT_XAR )     archive_write_set_format_xar(ar->archive);
#endif
#ifdef FORMAT_ZIP
    else if ( ar->type & FORMAT_ZIP )      archive_write_set_format_zip(ar->archive);
#endif
    else
    { return ar_set_status_error(ar, PL_existence_error3("option", "format", options));
    }


     if (0)  {}
#ifdef FILTER_BZIP2
    else if ( ar->type & FILTER_BZIP2 )    archive_write_add_filter_bzip2(ar->archive);
#endif
#ifdef FILTER_COMPRESS
    else if ( ar->type & FILTER_COMPRESS ) archive_write_add_filter_none(ar->archive);
#endif
#ifdef FILTER_GZIP
    else if ( ar->type & FILTER_GZIP )     archive_write_add_filter_gzip(ar->archive);
#endif
#ifdef FILTER_GRZIP
    else if ( ar->type & FILTER_GRZIP )    archive_write_add_filter_grzip(ar->archive);
#endif
#ifdef FILTER_LRZIP
    else if ( ar->type & FILTER_LRZIP )    archive_write_add_filter_lrzip(ar->archive);
#endif
#ifdef FILTER_LZMA
    else if ( ar->type & FILTER_LZMA )     archive_write_add_filter_lzma(ar->archive);
#endif
#ifdef FILTER_LZOP
    else if ( ar->type & FILTER_LZMA )     archive_write_add_filter_lzop(ar->archive);
#endif
#ifdef FILTER_XZ
    else if ( ar->type & FILTER_XZ )       archive_write_add_filter_xz(ar->archive);
#endif
#ifdef FILTER_NONE
    else                                   archive_write_add_filter_none(ar->archive);
#else
    else
    { return ar_set_status_error(ar, PL_existence_error3("option", "filter", options));
    }
#endif
    if ( (rc=archive_write_open(ar->archive, ar,
				libarchive_open_cb, libarchive_write_cb, libarchive_close_cb)) == ARCHIVE_OK )
    { ar->status = AR_OPENED_ARCHIVE;
      return TRUE;
    }
  } else {
    assert(0);
  }

  return archive_error(ar, rc);
}


static foreign_t
archive_property(term_t archive, term_t prop, term_t value)
{ archive_wrapper *ar;
  atom_t pn;
  const char *s;

  if ( !get_archive(archive, &ar) ||
       !PL_get_atom_ex(prop, &pn) )
    return FALSE;

  if ( pn == ATOM_filter )
  { int i, fcount = archive_filter_count(ar->archive);
    term_t tail = PL_copy_term_ref(value);
    term_t head = PL_new_term_ref();

    for(i=0; i<fcount; i++)
    { s = archive_filter_name(ar->archive, i);

      if ( !s || strcmp(s, "none") == 0 )
	continue;

      if ( !PL_unify_list(tail, head, tail) ||
	   !PL_unify_atom_chars(head, s) )
	return FALSE;
    }
    return PL_unify_nil(tail);
  }

  return FALSE;
}


static foreign_t
archive_next_header(term_t archive, term_t name)
{ archive_wrapper *ar;
  int rc;

  if ( !get_archive(archive, &ar) )
    return FALSE;
  if ( ar->how == 'w' )
  { char* pathname = NULL;
    if ( ar->status == AR_OPENED_ENTRY )
      return ar_set_status_error(ar, PL_permission_error("next_header", "archive", archive));
    if ( !PL_get_atom_chars(name, &pathname) )
      return ar_set_status_error(ar, PL_type_error("atom", name));
    if ( ar->entry )
      archive_entry_clear(ar->entry);
    else
      ar->entry = archive_entry_new();
    if ( !ar->entry )
      return ar_set_status_error(ar, PL_resource_error("memory"));
    archive_entry_set_pathname(ar->entry, pathname);
    /* libarchive-3.1.2 does not tolerate an empty size with zip. Later versions may though - it is fixed in git as of Dec 2013.
     *    For now, set the other entries to a sensible default
     */
    archive_entry_unset_size(ar->entry);
    archive_entry_set_filetype(ar->entry, AE_IFREG);
    archive_entry_set_perm(ar->entry, 0644);
    ar->status = AR_NEW_ENTRY;
    return TRUE;
  }
  if ( ar->status == AR_NEW_ENTRY )
  { if ( (rc=archive_read_data_skip(ar->archive)) != ARCHIVE_OK )
      return archive_error(ar, rc);
  } else if ( ar->status == AR_OPENED_ENTRY )
  { return ar_set_status_error(ar, PL_permission_error("next_header", "archive", archive));
  }

  while ( (rc=archive_read_next_header(ar->archive, &ar->entry)) == ARCHIVE_OK )
  { if ( PL_unify_wchars(name, PL_ATOM, -1,
			 archive_entry_pathname_w(ar->entry)) )
    { ar->status = AR_NEW_ENTRY;
      return TRUE;
    }
    if ( PL_exception(0) )
      return FALSE;
  }

  if ( rc == ARCHIVE_EOF )
    return FALSE;			/* simply at the end */

  return archive_error(ar, rc);
}


static foreign_t
archive_close(term_t archive)
{ archive_wrapper *ar;
  int rc;

  if ( !get_archive(archive, &ar) )
    return FALSE;

  if ( ar->status == AR_OPENED_ENTRY )
  { ar->closed_archive = TRUE;

    return TRUE;
  } else if ( (rc=archive_free_handle(ar)) == ARCHIVE_OK )
  { ar->entry = NULL;
    ar->archive = NULL;
    ar->symbol = 0;

    return TRUE;
  }

  return archive_error(ar, rc);
}

		 /*******************************
		 *	       HEADERS		*
		 *******************************/

static foreign_t
archive_header_prop_(term_t archive, term_t field)
{ archive_wrapper *ar;
  functor_t prop;

  if ( !get_archive(archive, &ar) )
    return FALSE;

  if ( !PL_get_functor(field, &prop) )
    return ar_set_status_error(ar, PL_type_error("compound", field));
  if ( ar->status != AR_NEW_ENTRY )
    return ar_set_status_error(ar, PL_permission_error("access", "archive_entry", archive));

  if ( prop == FUNCTOR_filetype1 )
  { __LA_MODE_T type = archive_entry_filetype(ar->entry);
    atom_t name;
    term_t arg = PL_new_term_ref();
    _PL_get_arg(1, field, arg);

    switch(type&AE_IFMT)
    { case AE_IFREG:  name = ATOM_file;             break;
      case AE_IFLNK:  name = ATOM_link;             break;
      case AE_IFSOCK: name = ATOM_socket;           break;
      case AE_IFCHR:  name = ATOM_character_device; break;
      case AE_IFBLK:  name = ATOM_block_device;     break;
      case AE_IFDIR:  name = ATOM_directory;        break;
      case AE_IFIFO:  name = ATOM_fifo;             break;
      default:
	return PL_unify_integer(arg, (type&AE_IFMT));
    }
    return PL_unify_atom(arg, name);
  } else if ( prop == FUNCTOR_mtime1 )
  { time_t stamp = archive_entry_mtime(ar->entry);
    term_t arg = PL_new_term_ref();
    _PL_get_arg(1, field, arg);

    return PL_unify_float(arg, (double)stamp);
  } else if ( prop == FUNCTOR_size1 )
  { int64_t size = archive_entry_size(ar->entry);
    term_t arg = PL_new_term_ref();
    _PL_get_arg(1, field, arg);

    return PL_unify_int64(arg, size);
  } else if ( prop == FUNCTOR_link_target1 )
  { __LA_MODE_T type = archive_entry_filetype(ar->entry);
    const wchar_t *target = NULL;

    switch(type&AE_IFMT)
    { case AE_IFLNK:
	target = archive_entry_symlink_w(ar->entry);
        break;
    }

    if ( target )
    { term_t arg = PL_new_term_ref();
      _PL_get_arg(1, field, arg);

      return PL_unify_wchars(arg, PL_ATOM, (size_t)-1, target);
    }

    return FALSE;
  } else if ( prop == FUNCTOR_permissions1 )
  { __LA_MODE_T perm = archive_entry_perm(ar->entry);
    term_t arg = PL_new_term_ref();
    _PL_get_arg(1, field, arg);

    return PL_unify_integer(arg, perm);
  } else if ( prop == FUNCTOR_format1 )
  { const char *s = archive_format_name(ar->archive);

    if ( s )
    { char lwr[50];
      char *o;
      term_t arg = PL_new_term_ref();
      _PL_get_arg(1, field, arg);

      for(o=lwr; *s && o < lwr+sizeof(lwr); )
	*o++ = tolower(*s++);

      *o = '\0';

      return PL_unify_atom_chars(arg, lwr);
    }
  }

  return ar_set_status_error(ar, PL_domain_error("archive_header_property", field));
}

static foreign_t
archive_set_header_property(term_t archive, term_t field)
{ archive_wrapper *ar;
  functor_t prop;

  if ( !get_archive(archive, &ar) )
    return FALSE;

  if ( !PL_get_functor(field, &prop) )
    return ar_set_status_error(ar, PL_type_error("compound", field));
  if ( ar->status != AR_NEW_ENTRY )
    return ar_set_status_error(ar, PL_permission_error("access", "archive_entry", archive));
  if ( ar->how != 'w' )
    return ar_set_status_error(ar, PL_permission_error("write", "archive_entry", archive));

  if ( prop == FUNCTOR_filetype1 )
  { atom_t name;
    term_t arg = PL_new_term_ref();
    __LA_MODE_T type;
    _PL_get_arg(1, field, arg);
    if ( !PL_get_atom(arg, &name) )
      return ar_set_status_error(ar, PL_type_error("atom", arg));
    if (name == ATOM_file)                  type = AE_IFREG;
    else if (name == ATOM_link)             type = AE_IFLNK;
    else if (name == ATOM_socket)           type = AE_IFSOCK;
    else if (name == ATOM_character_device) type = AE_IFCHR;
    else if (name == ATOM_block_device)     type = AE_IFBLK;
    else if (name == ATOM_directory)        type = AE_IFDIR;
    else if (name == ATOM_fifo)             type = AE_IFIFO;
    else
      return ar_set_status_error(ar, PL_domain_error("filetype", arg));
    archive_entry_set_filetype(ar->entry, type);
    PL_succeed;
  } else if (prop == FUNCTOR_mtime1)
  { double mtime;
    term_t arg = PL_new_term_ref();
    _PL_get_arg(1, field, arg);
    if ( !PL_get_float(arg, &mtime) )
      return ar_set_status_error(ar, PL_type_error("float", arg));
    archive_entry_set_mtime(ar->entry, (time_t)mtime, 0);
    PL_succeed;
  } else if (prop == FUNCTOR_size1)
  { int64_t size;
    term_t arg = PL_new_term_ref();
    _PL_get_arg(1, field, arg);
    if ( !PL_get_int64(arg, &size) )
      return ar_set_status_error(ar, PL_type_error("size", arg));
    archive_entry_set_size(ar->entry, size);
    PL_succeed;
  } else if (prop == FUNCTOR_link_target1)
  { const wchar_t* link;
    atom_t atom;
    size_t len;
    term_t arg = PL_new_term_ref();
    _PL_get_arg(1, field, arg);
    if ( !PL_get_atom(arg, &atom) )
      return ar_set_status_error(ar, PL_type_error("atom", arg));
    link = PL_atom_wchars(atom, &len);
    archive_entry_copy_symlink_w(ar->entry, link);
    archive_entry_set_filetype(ar->entry, AE_IFLNK);
    PL_succeed;
  } else
    return ar_set_status_error(ar, PL_domain_error("archive_header_property", field));
}


		 /*******************************
		 *	    READ MEMBERS	*
		 *******************************/

static ssize_t
ar_entry_read_cb(void *handle, char *buf, size_t size)
{ const archive_wrapper *ar = handle;

  return archive_read_data(ar->archive, buf, size);
}

static ssize_t
ar_entry_write_cb(void *handle, char *buf, size_t size)
{ const archive_wrapper *ar = handle;

  size_t written = archive_write_data(ar->archive, buf, size);
  /* In older version of libarchive (at least until 3.1.12), archive_write_data returns 0 for
     some formats if the file size is not set. It does not set archive_errno(), unfortunately.
     We turn this into an IO error here by returning -1
  */
  if (written == 0 && size > 0)
  { errno = ENOSPC;
    return -1;
  }
  return written;
}


static int
ar_entry_close_cb(void *handle)
{ archive_wrapper *ar = handle;
  if ( ar->closed_archive )
  { if ( ar->archive )
    { int rc;

      if ( (rc=archive_free_handle(ar)) == ARCHIVE_OK )
      { ar->entry = NULL;
	ar->archive = NULL;
	ar->symbol = 0;
      } else
	return -1;
    }
  }
  if ( ar->status == AR_OPENED_ENTRY )
  { PL_unregister_atom(ar->symbol);
    ar->status = AR_CLOSED_ENTRY;
  }
  return 0;
}

static int
ar_entry_control_cb(void *handle, int op, void *data)
{ const archive_wrapper *ar = handle;

  (void)ar;

  switch(op)
  { case SIO_SETENCODING:
      return 0;				/* allow switching encoding */
    case SIO_GETSIZE:
      *((int64_t*)data) = archive_entry_size(ar->entry);
      return 0;
    case SIO_FLUSHOUTPUT:
       return 0;
    default:
      return -1;
  }
}

static IOFUNCTIONS ar_entry_read_functions =
{ ar_entry_read_cb,
  NULL,					/* write */
  NULL,					/* seek */
  ar_entry_close_cb,
  ar_entry_control_cb,			/* control */
  NULL,					/* seek64 */
};

static IOFUNCTIONS ar_entry_write_functions =
{ NULL,					/* read */
  ar_entry_write_cb,
  NULL,					/* seek */
  ar_entry_close_cb,
  ar_entry_control_cb,			/* control */
  NULL,					/* seek64 */
};

static foreign_t
archive_open_entry(term_t archive, term_t stream)
{ archive_wrapper *ar;
  IOSTREAM *entry_data;

  /* If you make changes here, be sure to also change ar_entry_close() */

  if ( !get_archive(archive, &ar) )
    return FALSE;
  if ( ar->how == 'r' )
  { if ( (entry_data=Snew(ar, SIO_INPUT|SIO_RECORDPOS, &ar_entry_read_functions)) )
    { ar->status = AR_OPENED_ENTRY;
      if ( PL_unify_stream(stream, entry_data) )
      { PL_register_atom(ar->symbol);	/* We may no longer reference the */
        return TRUE;			/* archive itself */
      }
      Sclose(entry_data);
      return FALSE;
    }
  } else if ( ar->how == 'w' )
  { /* First we must finalize the header before trying to write the data */
    if ( ar->status == AR_NEW_ENTRY )
    { archive_write_header(ar->archive, ar->entry);
      archive_entry_free(ar->entry);
      ar->entry = NULL;
    } else
    { return ar_set_status_error(ar, PL_permission_error("access", "archive_entry", archive));
    }
    /* Then we can make a handle for the data */
    if ( (entry_data=Snew(ar, SIO_OUTPUT|SIO_RECORDPOS, &ar_entry_write_functions)) )
    { ar->status = AR_OPENED_ENTRY;
      if ( PL_unify_stream(stream, entry_data) )
      { PL_register_atom(ar->symbol);	/* We may no longer reference the */
        return TRUE;			/* archive itself */
      }
      Sclose(entry_data);
      return FALSE;
    }
  }

  return PL_resource_error("memory");
}


		 /*******************************
		 *	      INSTALL		*
		 *******************************/

#define MKFUNCTOR(n,a) \
	FUNCTOR_ ## n ## a = PL_new_functor(PL_new_atom(#n), a)
#define MKATOM(n) \
	ATOM_ ## n = PL_new_atom(#n)

install_t
install_archive4pl(void)
{ MKATOM(close_parent);
  MKATOM(compression);
  MKATOM(filter);
  MKATOM(format);
  MKATOM(all);
  MKATOM(bzip2);
  MKATOM(compress);
  MKATOM(gzip);
  MKATOM(grzip);
  MKATOM(lrzip);
  MKATOM(lzip);
  MKATOM(lzma);
  MKATOM(lzop);
  MKATOM(none);
  MKATOM(rpm);
  MKATOM(uu);
  MKATOM(xz);
  ATOM_7zip = PL_new_atom("7zip");
  MKATOM(ar);
  MKATOM(cab);
  MKATOM(cpio);
  MKATOM(empty);
  MKATOM(gnutar);
  MKATOM(iso9660);
  MKATOM(lha);
  MKATOM(mtree);
  MKATOM(rar);
  MKATOM(raw);
  MKATOM(tar);
  MKATOM(xar);
  MKATOM(zip);
  MKATOM(file);
  MKATOM(link);
  MKATOM(socket);
  MKATOM(character_device);
  MKATOM(block_device);
  MKATOM(directory);
  MKATOM(fifo);
  MKATOM(write);
  MKATOM(read);

  MKFUNCTOR(error,           2);
  MKFUNCTOR(archive_error,   2);
  MKFUNCTOR(existence_error, 3);
  MKFUNCTOR(filetype,        1);
  MKFUNCTOR(mtime,           1);
  MKFUNCTOR(size,            1);
  MKFUNCTOR(link_target,     1);
  MKFUNCTOR(format,          1);
  MKFUNCTOR(permissions,     1);

  PL_register_foreign("archive_open_stream",  4, archive_open_stream, 0);
  PL_register_foreign("archive_property",     3, archive_property,    0);
  PL_register_foreign("archive_close",        1, archive_close,       0);
  PL_register_foreign("archive_next_header",  2, archive_next_header, 0);
  PL_register_foreign("archive_header_prop_", 2, archive_header_prop_, 0);
  PL_register_foreign("archive_set_header_property", 2, archive_set_header_property, 0);
  PL_register_foreign("archive_open_entry",   2, archive_open_entry,  0);
}
