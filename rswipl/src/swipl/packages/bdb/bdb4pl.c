/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015-2025, University of Amsterdam
                              VU University Amsterdam
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
#include <SWI-Stream.h>
#include <pthread.h>
#include "bdb4pl.h"
#include <sys/types.h>
#include <limits.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <signal.h>

#ifdef O_DEBUG
#define DEBUG(g) g
#else
#define DEBUG(g) (void)0
#endif

static atom_t ATOM_atom;
static atom_t ATOM_btree;
static atom_t ATOM_c_blob;
static atom_t ATOM_c_long;
static atom_t ATOM_c_string;
static atom_t ATOM_client_timeout;
static atom_t ATOM_config;
static atom_t ATOM_database;
static atom_t ATOM_default;
static atom_t ATOM_environment;
static atom_t ATOM_false;
static atom_t ATOM_hash;
static atom_t ATOM_home;
static atom_t ATOM_key;
static atom_t ATOM_mp_mmapsize;
static atom_t ATOM_mp_size;
static atom_t ATOM_read;
static atom_t ATOM_recno;
static atom_t ATOM_server;
static atom_t ATOM_server_timeout;
static atom_t ATOM_term;
static atom_t ATOM_true;
static atom_t ATOM_type;
static atom_t ATOM_type;
static atom_t ATOM_unknown;
static atom_t ATOM_update;
static atom_t ATOM_value;
static atom_t ATOM_thread_count;

static functor_t FUNCTOR_error2;
static functor_t FUNCTOR_bdb3;

#define F_ERROR       ((u_int32_t)-1)
#define F_UNPROCESSED ((u_int32_t)-2)

typedef struct db_flag
{ char	   *name;
  u_int32_t flag;			/* flag for name */
  u_int32_t flags;			/* implied flags */
  atom_t    aname;
} db_flag;

static u_int32_t lookup_flag(db_flag *flags, atom_t name, term_t arg);

static dbenvh   default_env = {0};	/* default environment */

#define mkfunctor(n, a) PL_new_functor(PL_new_atom(n), a)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Old versions blocked signals during the  DB primitives. Current versions
of SWI-Prolog only use synchronous signals, so this is no longer needed.
I leave it here for future reference.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if 1
#define NOSIG(code) { code; }
#else
#define NOSIG(code) \
	{ sigset_t new, old; \
	  sigemptyset(&new); \
	  sigaddset(&new, SIGINT); \
	  sigprocmask(SIG_BLOCK, &new, &old); \
	  code; \
	  sigprocmask(SIG_SETMASK, &old, NULL); \
	}
#endif

static DB_TXN *current_transaction(void);
#define TheTXN current_transaction()

static void
initConstants(void)
{ ATOM_atom	      =	PL_new_atom("atom");
  ATOM_btree	      =	PL_new_atom("btree");
  ATOM_c_blob	      =	PL_new_atom("c_blob");
  ATOM_c_long	      =	PL_new_atom("c_long");
  ATOM_c_string	      =	PL_new_atom("c_string");
  ATOM_client_timeout =	PL_new_atom("client_timeout");
  ATOM_config	      =	PL_new_atom("config");
  ATOM_database	      =	PL_new_atom("database");
  ATOM_default	      = PL_new_atom("default");
  ATOM_environment    = PL_new_atom("environment");
  ATOM_false	      =	PL_new_atom("false");
  ATOM_hash	      =	PL_new_atom("hash");
  ATOM_home	      =	PL_new_atom("home");
  ATOM_key	      =	PL_new_atom("key");
  ATOM_mp_mmapsize    =	PL_new_atom("mp_mmapsize");
  ATOM_mp_size	      =	PL_new_atom("mp_size");
  ATOM_read	      =	PL_new_atom("read");
  ATOM_recno	      =	PL_new_atom("recno");
  ATOM_server	      =	PL_new_atom("server");
  ATOM_server_timeout =	PL_new_atom("server_timeout");
  ATOM_term	      =	PL_new_atom("term");
  ATOM_true	      =	PL_new_atom("true");
  ATOM_type	      =	PL_new_atom("type");
  ATOM_type	      = PL_new_atom("type");
  ATOM_unknown	      =	PL_new_atom("unknown");
  ATOM_update	      =	PL_new_atom("update");
  ATOM_value	      =	PL_new_atom("value");
  ATOM_thread_count   = PL_new_atom("thread_count");

  FUNCTOR_error2      = PL_new_functor(PL_new_atom("error"), 2);
  FUNCTOR_bdb3        = PL_new_functor(PL_new_atom("bdb"),   3);
}

static int bdb_close_env(dbenvh *env, int silent);
static int bdb_close(dbh *db);

		 /*******************************
		 *     DB_ENV SYMBOL WRAPPER	*
		 *******************************/

static void
acquire_dbenv(atom_t symbol)
{ dbenvh *db_env = PL_blob_data(symbol, NULL, NULL);
  db_env->symbol = symbol;
}


static int
release_dbenv(atom_t symbol)
{ dbenvh *db_env = PL_blob_data(symbol, NULL, NULL);
  DB_ENV *env;

  if ( (env=db_env->env) )
  { int rc;

    db_env->env = NULL;
    if ( (rc=env->close(env, 0)) )
      Sdprintf("Warning: BDB: DB_ENV close failed: %s\n", db_strerror(rc));
  }

  PL_free(db_env);

  return TRUE;
}

static int
compare_dbenvs(atom_t a, atom_t b)
{ dbenvh *ara = PL_blob_data(a, NULL, NULL);
  dbenvh *arb = PL_blob_data(b, NULL, NULL);

  return ( ara > arb ?  1 :
	   ara < arb ? -1 : 0
	 );
}

static int
write_dbenv(IOSTREAM *s, atom_t symbol, int flags)
{ dbenvh *db_env = PL_blob_data(symbol, NULL, NULL);

  Sfprintf(s, "<bdb_env>(%p)", db_env);

  return TRUE;
}

static PL_blob_t dbenv_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_NOCOPY,
  "bdb_env",
  release_dbenv,
  compare_dbenvs,
  write_dbenv,
  acquire_dbenv
};


static int
get_dbenv(term_t t, dbenvh **db_env)
{ PL_blob_t *type;
  atom_t a;
  void *data;

  if ( PL_get_blob(t, &data, NULL, &type) && type == &dbenv_blob)
  { dbenvh *p = data;

    if ( p->symbol )
    { *db_env = p;

      return TRUE;
    }

    PL_permission_error("access", "closed_bdb_env", t);
    return FALSE;
  } else if ( PL_get_atom(t, &a) && a == ATOM_default )
  { *db_env = &default_env;
    return TRUE;
  }

  PL_type_error("bdb_env", t);
  return FALSE;
}


static int
unify_dbenv(term_t t, dbenvh *db_env)
{ if ( db_env == &default_env )
    return PL_unify_atom(t, ATOM_default);
  else
    return PL_unify_blob(t, db_env, sizeof(*db_env), &dbenv_blob);
}


		 /*******************************
		 *	 DB SYMBOL WRAPPER	*
		 *******************************/

static void
acquire_db(atom_t symbol)
{ dbh *db = PL_blob_data(symbol, NULL, NULL);
  db->symbol = symbol;
}


static int
release_db(atom_t symbol)
{ dbh *db = PL_blob_data(symbol, NULL, NULL);
  DB *d;

  if ( (d=db->db) )
  { db->db = NULL;
    d->close(d, 0);
  }

  PL_free(db);

  return TRUE;
}

static int
compare_dbs(atom_t a, atom_t b)
{ dbh *ara = PL_blob_data(a, NULL, NULL);
  dbh *arb = PL_blob_data(b, NULL, NULL);

  return ( ara > arb ?  1 :
	   ara < arb ? -1 : 0
	 );
}

static int
write_db(IOSTREAM *s, atom_t symbol, int flags)
{ dbh *db = PL_blob_data(symbol, NULL, NULL);

  Sfprintf(s, "<bdb>(%p)", db);

  return TRUE;
}

static PL_blob_t db_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_NOCOPY,
  "bdb",
  release_db,
  compare_dbs,
  write_db,
  acquire_db
};


static bool
get_db(term_t t, dbh **db)
{ PL_blob_t *type;
  void *data;

  if ( PL_get_blob(t, &data, NULL, &type) && type == &db_blob)
  { dbh *p = data;

    if ( p->symbol )
    { *db = p;

      return true;
    }

    return PL_permission_error("access", "closed_bdb", t),false;
  }

  return PL_type_error("db", t),false;
}


static bool
unify_db(term_t t, dbh *db)
{ return PL_unify_blob(t, db, sizeof(*db), &db_blob);
}

		 /*******************************
		 *	   DATA EXCHANGE	*
		 *******************************/

static int
unify_dbt(term_t t, dtype type, DBT *dbt)
{ switch( type )
  { case D_TERM:
    { term_t r = PL_new_term_ref();

      PL_recorded_external(dbt->data, r);
      return PL_unify(t, r);
    }
    case D_ATOM:
      return PL_unify_chars(t, PL_ATOM|REP_UTF8, dbt->size, dbt->data);
    case D_CBLOB:
      return PL_unify_chars(t, PL_STRING|REP_ISO_LATIN_1, dbt->size, dbt->data);
    case D_CSTRING:
      return PL_unify_chars(t, PL_ATOM|REP_UTF8, (size_t)-1, dbt->data);
    case D_CLONG:
    { long *v = dbt->data;
      return PL_unify_integer(t, *v);
    }
  }
  assert(0);
  return FALSE;
}


static int
get_dbt(term_t t, dtype type, DBT *dbt)
{ memset(dbt, 0, sizeof(*dbt));

  switch(type)
  { case D_TERM:
    { size_t len;

      dbt->data = PL_record_external(t, &len);
      dbt->size = len;
      return TRUE;
    }
    case D_ATOM:
    { size_t len;
      char *s;

      if ( PL_get_nchars(t, &len, &s,
			 CVT_ATOM|CVT_EXCEPTION|REP_UTF8|BUF_MALLOC) )
      { dbt->data = s;
	dbt->size = len;

	return TRUE;
      } else
	return FALSE;
    }
    case D_CBLOB:
    { size_t len;
      char *s;

      if ( PL_get_nchars(t, &len, &s,
			 CVT_ATOM|CVT_STRING|CVT_EXCEPTION|
			 REP_ISO_LATIN_1|BUF_MALLOC) )
      { dbt->data = s;
	dbt->size = len;

	return TRUE;
      } else
	return FALSE;
    }
    case D_CSTRING:
    { size_t len;
      char *s;

      if ( PL_get_nchars(t, &len, &s,
			 CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_UTF8|BUF_MALLOC) )
      { dbt->data = s;
	dbt->size = len+1;		/* account for terminator */

	return TRUE;
      } else
	return FALSE;
    }
    case D_CLONG:
    { long v;

      if ( PL_get_long_ex(t, &v) )
      {	long *d = malloc(sizeof(long));

	*d = v;
	dbt->data = d;
	dbt->size = sizeof(long);

	return TRUE;
      } else
	return FALSE;
    }
  }
  assert(0);
  return FALSE;
}


static void
free_dbt(DBT *dbt, dtype type)
{ switch ( type )
  { case D_TERM:
      PL_erase_external(dbt->data);
      break;
    case D_ATOM:
    case D_CBLOB:
    case D_CSTRING:
      PL_free(dbt->data);
      break;
    case D_CLONG:
      free(dbt->data);
  }
}


static void
free_result_dbt(DBT *dbt)
{ if ( dbt->flags & DB_DBT_MALLOC )
    free(dbt->data);
}


typedef struct
{ int id;
  const char *str;
} err_def;

static const err_def errors[] =
{ { DB_LOCK_DEADLOCK,	"lock_deadlock" },
  { DB_RUNRECOVERY,	"runrecovery" },
  { DB_NOTFOUND,	"notfound" },
  { DB_KEYEMPTY,	"keyempty" },
  { DB_KEYEXIST,	"keyexist" },
  { DB_LOCK_NOTGRANTED,	"lock_notgranted" },
  { DB_SECONDARY_BAD,	"secondary_bad" },
  { 0,			NULL }
};


static int
db_status(int rval, term_t obj)
{ if ( rval == 0 )
    return TRUE;

  if ( rval < 0 )
  { DEBUG(Sdprintf("DB error: %s\n", db_strerror(rval)));
    return FALSE;			/* normal failure */
  } else
  { const char *id_str = NULL;
    const err_def *ed;
    term_t ex, id=0;

    for(ed=errors; ed->id; ed++)
    { if ( ed->id == rval )
      { id_str = ed->str;
	break;
      }
    }

    if ( (ex = PL_new_term_ref()) &&
	 (id = PL_new_term_ref()) &&
	 (id_str ? PL_unify_atom_chars(id, id_str)
		 : PL_unify_integer(id, rval)),
	 PL_unify_term(ex,
		       PL_FUNCTOR, FUNCTOR_error2,
			 PL_FUNCTOR, FUNCTOR_bdb3,
			   PL_TERM, id,
			   PL_CHARS, db_strerror(rval),
			   PL_TERM, obj,
		         PL_VARIABLE) )
      return PL_raise_exception(ex);

    return FALSE;
  }
}


static int
db_status_db(int rval, dbh *dbh)
{ term_t db;

  if ( (db = PL_new_term_ref()) &&
       unify_db(db, dbh) )
    return db_status(rval, db);

  return FALSE;
}

static int
db_status_env(int rval, dbenvh *db_env)
{ term_t env;

  if ( (env = PL_new_term_ref()) &&
       unify_dbenv(env, db_env) )
    return db_status(rval, env);

  return FALSE;
}

static int
db_preoptions(term_t t, dbenvh **db_env, int *type)
{ term_t tail = PL_copy_term_ref(t);
  term_t head = PL_new_term_ref();

  while( PL_get_list(tail, head, tail) )
  { atom_t name;
    size_t arity;

    if ( PL_get_name_arity(head, &name, &arity) )
    { if ( name == ATOM_type )
      { term_t a0 = PL_new_term_ref();
	atom_t tp;

	_PL_get_arg(1, head, a0);

	if ( !PL_get_atom_ex(a0, &tp) )
	  return FALSE;
	if ( tp == ATOM_btree )
	  *type = DB_BTREE;
	else if ( tp == ATOM_hash )
	  *type = DB_HASH;
	else if ( tp == ATOM_recno )
	  *type = DB_RECNO;
	else if ( tp == ATOM_unknown )
	  *type = DB_UNKNOWN;
	else
	  return PL_domain_error("db_type", a0);

	return TRUE;
      } else if ( name == ATOM_environment )
      { term_t a0 = PL_new_term_ref();
	_PL_get_arg(1, head, a0);

	if ( !get_dbenv(a0, db_env) )
	  return FALSE;
      }
    }
  }

  return TRUE;
}


static int
get_dtype(term_t t, dtype *type)
{ atom_t a;

  if ( !PL_get_atom_ex(t, &a) )
    return FALSE;
  if ( a == ATOM_term )
    *type = D_TERM;
  else if ( a == ATOM_atom )
    *type = D_ATOM;
  else if ( a == ATOM_c_blob )
    *type = D_CBLOB;
  else if ( a == ATOM_c_string )
    *type = D_CSTRING;
  else if ( a == ATOM_c_long )
    *type = D_CLONG;
  else
    return PL_domain_error("type", t);

  return TRUE;
}


static db_flag db_flags[] =
{ { "auto_commit",	DB_AUTO_COMMIT,	     0 },
  { "create",		DB_CREATE,	     0 },
  { "excl",		DB_EXCL,	     0 },
  { "multiversion",	DB_MULTIVERSION,     0 },
  { "nommap",		DB_NOMMAP,	     0 },
  { "rdonly",		DB_RDONLY,	     0 },
  { "read_uncommitted",	DB_READ_UNCOMMITTED, 0 },
  { "thread",		DB_THREAD,	     0 },
  { "truncate",		DB_TRUNCATE,	     0 },
  { "dup",		DB_DUP,		     0 },
  { "duplicates",	DB_DUP,		     0 }, /* compatibility */
  { NULL,		0,		     0 },
};


static int
db_options(term_t t, dbh *dbh, char **subdb)
{ term_t tail = PL_copy_term_ref(t);
  term_t head = PL_new_term_ref();
  int flags = 0;

  dbh->key_type   = D_TERM;
  dbh->value_type = D_TERM;

  while( PL_get_list(tail, head, tail) )
  { atom_t name;
    size_t arity;

    if ( PL_get_name_arity(head, &name, &arity) )
    { if ( arity == 1 )
      { term_t a0 = PL_new_term_ref();

	_PL_get_arg(1, head, a0);
	if ( name == ATOM_database )
	{ if ( !PL_get_chars(a0, subdb,
			     CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_UTF8) )
	    return FALSE;
	} else if ( name == ATOM_key )
	{ if ( !get_dtype(a0, &dbh->key_type) )
	    return FALSE;
	} else if ( name == ATOM_value )
	{ if ( !get_dtype(a0, &dbh->value_type) )
	    return FALSE;
	} else if ( name == ATOM_type || name == ATOM_environment )
	{  ;  /* type(_) and environment() are handled by db_preoptions */
	} else
	{ u_int32_t fv = lookup_flag(db_flags, name, a0);

	  switch(fv)
	  { case F_ERROR:
	      return FALSE;
	    case F_UNPROCESSED:
	      return PL_domain_error("db_option", head);
	    default:
	      flags |= fv;
	  }
	}
      } else
	return PL_type_error("db_option", head);
    }
  }

  if ( !PL_get_nil_ex(tail) )
    return FALSE;

  if ( flags )
  { int rval;

    if ( (rval=dbh->db->set_flags(dbh->db, flags)) )
      return db_status_db(rval, dbh);
    dbh->flags = flags;
  }


  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
verify that an environment that is not  initialized for threading is not
accessed from multiple threads. Not sure whether or not an uninitialized
default environment can be accessed from multiple threads.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
check_same_thread(dbenvh *env)
{ if ( (env->flags&DB_THREAD) || env->thread == PL_thread_self() )
  { return TRUE;
  } else if ( env == &default_env && !env->thread )
  { env->thread = PL_thread_self();
    return TRUE;
  } else
  { term_t e;

    if ( (e = PL_new_term_ref()) &&
	 unify_dbenv(e, env) )
      return PL_permission_error("access", "bdb_environment", e);
    return FALSE;
  }
}


static foreign_t
pl_bdb_open(term_t file, term_t mode, term_t handle, term_t options)
{ char *fname;
  int flags;
  int m = 0666;
  int type = DB_BTREE;
  dbh *dbh;
  atom_t a;
  int rval;
  char *subdb = NULL;
  dbenvh *env = &default_env;

  if ( !PL_get_file_name(file, &fname, PL_FILE_OSPATH) )
    return FALSE;

  if ( !PL_get_atom_ex(mode, &a) )		/* process mode */
    return FALSE;
  if ( a == ATOM_read )
    flags = DB_RDONLY;
  else if ( a == ATOM_update )
    flags = DB_CREATE;
  else
    return PL_domain_error("io_mode", mode);

  if ( !db_preoptions(options, &env, &type) ||
       !check_same_thread(env) )
    return FALSE;

  dbh = calloc(1, sizeof(*dbh));
  dbh->magic = DBH_MAGIC;
  dbh->env   = env;
  NOSIG(rval=db_create(&dbh->db, env->env, 0));
  if ( rval )
  { dbh->db = NULL;
    dbh->symbol = 0;
    return db_status(rval, file);
  }

  DEBUG(Sdprintf("New DB at %p\n", dbh->db));

  if ( !db_options(options, dbh, &subdb) )
  { bdb_close(dbh);
    return FALSE;
  }

#ifdef DB41
  if ( (env->flags&DB_INIT_TXN) )
    flags |= DB_AUTO_COMMIT;
  NOSIG(rval=dbh->db->open(dbh->db, TheTXN, fname, subdb, type, flags, m));
#else
  NOSIG(rval=dbh->db->open(dbh->db, fname, subdb, type, flags, m));
#endif

  if ( rval )
  { bdb_close(dbh);
    return db_status_db(rval, dbh);
  }

  return unify_db(handle, dbh);
}


static int
bdb_close(dbh *db)
{ int rval;

  DEBUG(Sdprintf("Close DB at %p\n", db->db));
  NOSIG(rval = db->db->close(db->db, 0);
	db->db = NULL;
	db->symbol = 0);

  return rval;
}


static foreign_t
pl_bdb_close(term_t handle)
{ dbh *db;

  if ( get_db(handle, &db) )
  { if ( !db->db || !db->symbol )
      return PL_existence_error("db", handle);
    return db_status(bdb_close(db), handle);
  }

  return FALSE;
}

static foreign_t
pl_bdb_is_open(term_t t)
{ PL_blob_t *type;
  void *data;

  if ( PL_get_blob(t, &data, NULL, &type) && type == &db_blob)
  { dbh *p = data;

    if ( p->db && p->symbol )
      return TRUE;

    return FALSE;
  }

  return PL_type_error("db", t);
}


		 /*******************************
		 *	   TRANSACTIONS		*
		 *******************************/

static pthread_key_t transaction_key;

typedef struct transaction
{ DB_TXN *tid;				/* transaction id */
  struct transaction *parent;		/* parent id */
  dbenvh *env;				/* environment of the transaction */
} transaction;

typedef struct transaction_stack
{ transaction *top;
} transaction_stack;

static transaction_stack *
my_tr_stack(void)
{ transaction_stack *stack;

  if ( (stack=pthread_getspecific(transaction_key)) )
    return stack;

  if ( (stack=calloc(1,sizeof(*stack))) )
  { pthread_setspecific(transaction_key, stack);
    return stack;
  }

  PL_resource_error("memory");
  return NULL;
}

static void
free_transaction_stack(void *ptr)
{ transaction_stack *stack = ptr;

  assert(stack->top == NULL);
  free(stack);
}


static int
begin_transaction(dbenvh *env, transaction *t)
{ if ( env->env && (env->flags&DB_INIT_TXN) )
  { int rval;
    DB_TXN *pid, *tid;
    transaction_stack *stack;

    if ( !(stack=my_tr_stack()) )
      return FALSE;

    if ( stack->top )
      pid = stack->top->tid;
    else
      pid = NULL;

    if ( (rval=env->env->txn_begin(env->env, pid, &tid, 0)) )
      return db_status_env(rval, env);

    t->parent = stack->top;
    t->tid = tid;
    t->env = env;
    stack->top = t;

    return TRUE;
  } else
  { term_t ex;

    if ( (ex=PL_new_term_ref()) &&
	 unify_dbenv(ex, env) )
      return PL_permission_error("start", "transaction", ex);
    return FALSE;
  }
}


static int
commit_transaction(transaction *t)
{ transaction_stack *stack = my_tr_stack();
  int rval;

  assert(stack);
  assert(stack->top == t);

  stack->top = t->parent;

  if ( (rval=t->tid->commit(t->tid, 0)) )
    return db_status_env(rval, t->env);

  return TRUE;
}


static int
abort_transaction(transaction *t)
{ transaction_stack *stack = my_tr_stack();
  int rval;

  assert(stack);
  assert(stack->top == t);

  stack->top = t->parent;

  if ( (rval=t->tid->abort(t->tid)) )
    return db_status_env(rval, t->env);

  return TRUE;
}


static DB_TXN *
current_transaction(void)
{ transaction_stack *stack;

  if ( (stack=pthread_getspecific(transaction_key)) &&
       stack->top )
    return stack->top->tid;

  return NULL;
}


static foreign_t
bdb_transaction(term_t environment, term_t goal)
{ static predicate_t call1;
  qid_t qid;
  int rval;
  struct transaction tr;
  dbenvh *env = &default_env;

  if ( !call1 )
    call1 = PL_predicate("call", 1, "system");

  if ( (environment && !get_dbenv(environment, &env)) ||
       !check_same_thread(env) )
    return FALSE;

  NOSIG(rval=begin_transaction(env, &tr));
  if ( !rval )
    return FALSE;

  qid = PL_open_query(NULL, PL_Q_PASS_EXCEPTION, call1, goal);
  rval = PL_next_solution(qid);
  if ( rval )
  { PL_cut_query(qid);
    NOSIG(rval=commit_transaction(&tr));
    return rval;
  } else
  { PL_cut_query(qid);

    NOSIG(rval=abort_transaction(&tr));
    if ( !rval )
      return FALSE;

    return FALSE;
  }
}


static foreign_t
pl_bdb_transaction2(term_t environment, term_t goal)
{ return bdb_transaction(environment, goal);
}

static foreign_t
pl_bdb_transaction1(term_t goal)
{ return bdb_transaction(0, goal);
}


		 /*******************************
		 *	     DB ACCESS		*
		 *******************************/

static foreign_t
pl_bdb_put(term_t handle, term_t key, term_t value)
{ DBT k, v;
  dbh *db;
  int flags = 0;
  int rval;

  if ( !get_db(handle, &db) )
    return FALSE;

  if ( !get_dbt(key, db->key_type, &k) ||
       !get_dbt(value, db->value_type, &v) )
    return FALSE;

  NOSIG(rval = db_status(db->db->put(db->db, TheTXN, &k, &v, flags), handle));
  free_dbt(&k, db->key_type);
  free_dbt(&v, db->value_type);

  return rval;
}


static foreign_t
pl_bdb_del2(term_t handle, term_t key)
{ DBT k;
  dbh *db;
  int flags = 0;			/* current no flags in DB */
  int rval;

  if ( !get_db(handle, &db) )
    return FALSE;

  if ( !get_dbt(key, db->key_type, &k) )
    return FALSE;

  NOSIG(rval = db_status(db->db->del(db->db, TheTXN, &k, flags), handle));
  free_dbt(&k, db->key_type);

  return rval;
}


static int
equal_dbt(DBT *a, DBT *b)
{ if ( a->size == b->size )
  { if ( a->data == b->data )
      return TRUE;
    if ( memcmp(a->data, b->data, a->size) == 0 )
      return TRUE;
  }

  return FALSE;
}


static foreign_t
pl_bdb_getall(term_t handle, term_t key, term_t value)
{ DBT k, v;
  dbh *db;
  int rval;

  if ( !get_db(handle, &db) )
    return FALSE;

  if ( !get_dbt(key, db->key_type, &k) )
    return FALSE;
  memset(&v, 0, sizeof(v));

  if ( (db->flags&DB_DUP) )			/* must use a cursor */
  { DBC *cursor;
    term_t tail = PL_copy_term_ref(value);
    term_t head = PL_new_term_ref();

    NOSIG(rval=db->db->cursor(db->db, TheTXN, &cursor, 0));
    if ( rval )
      return db_status(rval, handle);

    NOSIG(rval=cursor->c_get(cursor, &k, &v, DB_SET));
    if ( rval == 0 )
    { DBT k2;

      if ( !PL_unify_list(tail, head, tail) ||
	   !unify_dbt(head, db->value_type, &v) )
      { cursor->c_close(cursor);
	return FALSE;
      }

      memset(&k2, 0, sizeof(k2));
      for(;;)
      { NOSIG(rval=cursor->c_get(cursor, &k2, &v, DB_NEXT));

	if ( rval == 0 )
	{ if ( equal_dbt(&k, &k2) )
	  { int ok = ( PL_unify_list(tail, head, tail) &&
		       unify_dbt(head, db->value_type, &v) );
	    free_result_dbt(&v);
	    if ( ok )
	      continue;
	  }
	  free_result_dbt(&v);
	}

	NOSIG(cursor->c_close(cursor);
	      free_dbt(&k, db->key_type));

	if ( rval <= 0 )		/* normal failure */
	{ return PL_unify_nil(tail);
	} else				/* error failure */
	{ return db_status(rval, handle);
	}
      }
    } else if ( rval == DB_NOTFOUND )
    { free_dbt(&k, db->key_type);
      return FALSE;
    } else
    { free_dbt(&k, db->key_type);
      return db_status(rval, handle);
    }
  } else
  { NOSIG(rval=db->db->get(db->db, TheTXN, &k, &v, 0));

    if ( !rval )
    { term_t t = PL_new_term_ref();
      term_t tail = PL_copy_term_ref(value);
      term_t head = PL_new_term_ref();

      free_dbt(&k, db->key_type);
      PL_recorded_external(v.data, t);
      if ( PL_unify_list(tail, head, tail) &&
	   PL_unify(head, t) &&
	   PL_unify_nil(tail) )
	return TRUE;

      return FALSE;
    } else
      return db_status(rval, handle);
  }
}


typedef struct _dbget_ctx
{ dbh *db;				/* the database */
  DBC *cursor;				/* the cursor */
  DBT key;				/* the key */
  DBT k2;				/* secondary key */
  DBT value;				/* the value */
} dbget_ctx;


static foreign_t
pl_bdb_enum(term_t handle, term_t key, term_t value, control_t ctx)
{ DBT k, v;
  dbh *db;
  int rval = 0;
  dbget_ctx *c = NULL;
  fid_t fid = 0;

  memset(&k, 0, sizeof(k));
  memset(&v, 0, sizeof(v));

  switch( PL_foreign_control(ctx) )
  { case PL_FIRST_CALL:
      if ( !get_db(handle, &db) )
	return FALSE;
      c = calloc(1, sizeof(*c));

      c->db = db;
      if ( (rval=db->db->cursor(db->db, TheTXN, &c->cursor, 0)) )
      { free(c);
	return db_status(rval, handle);
      }
      DEBUG(Sdprintf("Created cursor at %p\n", c->cursor));

      rval = c->cursor->c_get(c->cursor, &c->key, &c->value, DB_FIRST);
      if ( rval == 0 )
      { int rc;

	fid = PL_open_foreign_frame();
	rc = ( unify_dbt(key, db->key_type, &c->key) &&
	       unify_dbt(value, db->value_type, &c->value) );
	free_result_dbt(&c->key);
	free_result_dbt(&c->value);
	if ( rc )
	{ PL_close_foreign_frame(fid);
	  PL_retry_address(c);
	}

	PL_rewind_foreign_frame(fid);
	goto retry;
      }
      goto out;
    case PL_REDO:
      c = PL_foreign_context_address(ctx);
      db = c->db;

    retry:
      for(;;)
      { rval = c->cursor->c_get(c->cursor, &c->k2, &c->value, DB_NEXT);

	if ( rval == 0 )
	{ int rc;

	  if ( !fid )
	    fid = PL_open_foreign_frame();

	  rc =  ( unify_dbt(key, db->key_type, &c->k2) &&
		  unify_dbt(value, db->value_type, &c->value) );
	  free_result_dbt(&c->key);
	  free_result_dbt(&c->value);
	  if ( rc )
	  { PL_close_foreign_frame(fid);
	    PL_retry_address(c);
	  }
	  PL_rewind_foreign_frame(fid);
	  continue;
	}
	break;
      }
      break;
    case PL_PRUNED:
      c = PL_foreign_context_address(ctx);
      db = c->db;
      break;
  }

out:
  if ( c )
  { if ( rval == 0 )
      rval = c->cursor->c_close(c->cursor);
    else
      c->cursor->c_close(c->cursor);
    free(c);
  }
  if ( fid )
    PL_close_foreign_frame(fid);

  db_status(rval, handle);
  return FALSE;				/* also on rval = 0! */
}


#define DO_DEL \
	if ( del ) \
	{ do \
	  { if ( (rval=c->cursor->c_del(c->cursor, 0)) != 0 ) \
	      return db_status(rval, handle); \
	  } while(0); \
	}


static foreign_t
pl_bdb_getdel(term_t handle, term_t key, term_t value, control_t ctx, int del)
{ dbh *db;
  int rval = 0;
  dbget_ctx *c = NULL;
  fid_t fid = 0;

  switch( PL_foreign_control(ctx) )
  { case PL_FIRST_CALL:
      if ( !get_db(handle, &db) )
	return FALSE;

      if ( (db->flags&DB_DUP) )		/* DB with duplicates */
      { c = calloc(1, sizeof(*c));

	c->db = db;
	if ( (rval=db->db->cursor(db->db, TheTXN, &c->cursor, 0)) )
	{ free(c);
	  return db_status(rval, handle);
	}
	DEBUG(Sdprintf("Created cursor at %p\n", c->cursor));
	if ( !get_dbt(key, db->key_type, &c->key) )
	  return FALSE;

	rval = c->cursor->c_get(c->cursor, &c->key, &c->value, DB_SET);
	if ( rval == 0 )
	{ int rc;

	  fid = PL_open_foreign_frame();
	  rc = unify_dbt(value, db->value_type, &c->value);
	  free_result_dbt(&c->value);

	  if ( rc )
	  { DO_DEL;

	    PL_close_foreign_frame(fid);
	    PL_retry_address(c);
	  }

	  PL_rewind_foreign_frame(fid);
	  goto retry;
	}
	goto out;
      } else				/* Unique DB */
      { DBT k, v;
	int rc;

	if ( !get_dbt(key, db->key_type, &k) )
	  return FALSE;
	memset(&v, 0, sizeof(v));
	if ( (db->env->flags&DB_THREAD) )
	  v.flags = DB_DBT_MALLOC;

	if ( (rval=db->db->get(db->db, TheTXN, &k, &v, 0)) == 0 )
	{ rc = unify_dbt(value, db->value_type, &v);

	  free_result_dbt(&v);
	  if ( rc && del )
	  { int flags = 0;

	    rc = db_status(db->db->del(db->db, TheTXN, &k, flags), handle);
	  }
	} else
	  rc = db_status(rval, handle);

	free_dbt(&k, db->key_type);

	return rc;
      }
    case PL_REDO:
      c = PL_foreign_context_address(ctx);
      db = c->db;

    retry:
      for(;;)
      { rval = c->cursor->c_get(c->cursor, &c->k2, &c->value, DB_NEXT);

	if ( rval == 0 && equal_dbt(&c->key, &c->k2) )
	{ if ( !fid )
	    fid = PL_open_foreign_frame();
	  if ( unify_dbt(value, db->value_type, &c->value) )
	  { DO_DEL;
	    PL_close_foreign_frame(fid);
	    PL_retry_address(c);
	  }
	  PL_rewind_foreign_frame(fid);
	  continue;
	}
	break;
      }
      break;
    case PL_PRUNED:
      c = PL_foreign_context_address(ctx);
      db = c->db;
      break;
  }

out:
  if ( c )
  { if ( rval == 0 )
      rval = c->cursor->c_close(c->cursor);
    else
      c->cursor->c_close(c->cursor);
    DEBUG(Sdprintf("Destroyed cursor at %p\n", c->cursor));
    free_dbt(&c->key, db->key_type);
    free(c);
  }
  if ( fid )
    PL_close_foreign_frame(fid);

  db_status(rval, handle);
  return FALSE;				/* also on rval = 0! */
}


static foreign_t
pl_bdb_get(term_t handle, term_t key, term_t value, control_t ctx)
{ int rval;

  NOSIG(rval = pl_bdb_getdel(handle, key, value, ctx, FALSE));

  return rval;
}


static foreign_t
pl_bdb_del3(term_t handle, term_t key, term_t value, control_t ctx)
{ int rval;

  NOSIG(rval=pl_bdb_getdel(handle, key, value, ctx, TRUE));

  return rval;
}


static int
bdb_close_env(dbenvh *env, int silent)
{ int rc = TRUE;

  if ( env->env )
  { int rval = env->env->close(env->env, 0);

    if ( silent )			/* do not throw exceptions */
    { if ( rval )
	Sdprintf("DB: ENV close failed: %s\n", db_strerror(rc));
      rc = !rval;
    } else
    { rc = db_status_env(rval, env);
    }

    env->env	= NULL;
    env->flags  = 0;
    env->thread = 0;
    if ( env->home )
    { free(env->home);
      env->home = NULL;
    }
  }

  return rc;
}


		 /*******************************
		 *     DATABASE ENVIRONMENTS    *
		 *******************************/

typedef struct _server_info
{ char *host;
  long cl_timeout;
  long sv_timeout;
  u_int32_t flags;
} server_info;


static void
#ifdef DB43
pl_bdb_error(const DB_ENV *dbenv, const char *prefix, const char *msg)
#else
pl_bdb_error(const char *prefix, char *msg)
#endif
{ Sdprintf("%s%s\n", prefix, msg);
}


#if defined(HAVE_SET_RPC_SERVER) || defined(HAVE_SET_SERVER)

static int
get_server(term_t options, server_info *info)
{ term_t l = PL_copy_term_ref(options);
  term_t h = PL_new_term_ref();

  while( PL_get_list(l, h, l) )
  { atom_t name;
    size_t arity;

    if ( PL_get_name_arity(h, &name, &arity) && name == ATOM_server )
    { info->cl_timeout = 0;
      info->sv_timeout = 0;
      info->flags      = 0;

      if ( arity >= 1 )			/* server(host) */
      { term_t a = PL_new_term_ref();

	_PL_get_arg(1, h, a);
	if ( !PL_get_chars(a, &info->host,
			   CVT_ATOM|CVT_STRING|REP_MB|CVT_EXCEPTION) )
	  return FALSE;
      }
      if ( arity == 2 )			/* server(host, options) */
      { term_t a = PL_new_term_ref();

	_PL_get_arg(2, h, l);
	while( PL_get_list(l, h, l) )
	{ atom_t name;
	  size_t arity;

	  if ( PL_get_name_arity(h, &name, &arity) && arity == 1 )
	  { _PL_get_arg(1, h, a);

	    if ( name == ATOM_server_timeout )
	    { if ( !PL_get_long_ex(a, &info->sv_timeout) )
		return FALSE;
	    } else if ( name == ATOM_client_timeout )
	    { if ( !PL_get_long_ex(a, &info->cl_timeout) )
		return FALSE;
	    } else
	      return PL_domain_error("server_option", a);
	  } else
	    return PL_domain_error("server_option", a);
	}
	if ( !PL_get_nil_ex(l) )
	  return FALSE;
      }

      return TRUE;
    }
  }

  return FALSE;
}


#if defined(DB_CLIENT) && !defined(DB_RPCCLIENT)
#define DB_RPCCLIENT DB_CLIENT
#endif

#endif


#define MAXCONFIG 20

static db_flag dbenv_flags[] =
{ { "init_lock",	DB_INIT_LOCK,	     0 },
  { "init_log",		DB_INIT_LOG,	     0 },
  { "init_mpool",	DB_INIT_MPOOL,	     0 },
  { "init_rep",		DB_INIT_REP,	     DB_INIT_TXN|DB_INIT_LOCK },
  { "init_txn",		DB_INIT_TXN,	     DB_INIT_LOG },
  { "recover",		DB_RECOVER,	     DB_CREATE|DB_INIT_TXN },
  { "recover_fatal",	DB_RECOVER_FATAL,    DB_CREATE|DB_INIT_TXN },
  { "use_environ",	DB_USE_ENVIRON,	     0 },
  { "use_environ_root",	DB_USE_ENVIRON_ROOT, 0 },
  { "create",		DB_CREATE,	     0 },
  { "lockdown",		DB_LOCKDOWN,	     0 },
#ifdef DB_FAILCHK
  { "failchk",		DB_FAILCHK,	     0 },
#endif
  { "private",		DB_PRIVATE,	     0 },
  { "register",		DB_REGISTER,	     0 },
  { "system_mem",	DB_SYSTEM_MEM,	     0 },
  { "thread",		DB_THREAD,	     0 },
  { (char*)NULL,	0,		     0 }
};

#define F_ERROR       ((u_int32_t)-1)
#define F_UNPROCESSED ((u_int32_t)-2)

static u_int32_t
lookup_flag(db_flag *fp, atom_t name, term_t arg)
{ for(; fp->name; fp++)
  { if ( !fp->aname )
      fp->aname = PL_new_atom(fp->name);

    if ( fp->aname == name )
    { int v;

      if ( arg )
      { if ( !PL_get_bool_ex(arg, &v) )
	  return F_ERROR;
	return v ? (fp->flag|fp->flags) : 0;
      } else
      { return fp->flag;
      }
    }
  }

  return F_UNPROCESSED;
}


static foreign_t
bdb_init(term_t newenv, term_t option_list)
{ int rval;
  term_t options = PL_copy_term_ref(option_list);
  u_int32_t flags = 0;
  term_t head = PL_new_term_ref();
  term_t a    = PL_new_term_ref();
  char *home = NULL;
  char *config[MAXCONFIG];
  int nconf = 0;
  dbenvh *env;

  if ( newenv )
  { if ( !(env=malloc(sizeof(*env))) )
      return PL_resource_error("memory");
    memset(env, 0, sizeof(*env));
  } else
    env = &default_env;

  if ( env->env )
  { term_t e;

    if ( (e=PL_new_term_ref()) &&
	 unify_dbenv(e, env) )
      return PL_permission_error("initialize", "environment", e);
    return FALSE;
  }

  config[0] = NULL;

  {
#if defined(HAVE_SET_RPC_SERVER) || defined(HAVE_SET_SERVER)
    server_info si;
    if ( get_server(option_list, &si) )
    { if ( (rval=db_env_create(&env->env, DB_RPCCLIENT)) )
	goto db_error;
#ifdef HAVE_SET_RPC_SERVER		/* >= 4.0; <= 5.0 */
      rval = env->env->set_rpc_server(env->env, 0, si.host,
				      si.cl_timeout, si.sv_timeout, si.flags);
#else
#ifdef HAVE_SET_SERVER
      rval = env->env->set_server(env->env, si.host,
				  si.cl_timeout, si.sv_timeout, si.flags);
#endif
#endif
      if ( rval )
	goto db_error;
    } else
#endif
    { if ( (rval=db_env_create(&env->env, 0)) )
	goto db_error;
    }
  }

  env->env->set_errpfx(env->env, "bdb4pl: ");
  env->env->set_errcall(env->env, pl_bdb_error);

  flags |= DB_INIT_MPOOL;		/* always needed? */

  while(PL_get_list(options, head, options))
  { atom_t name;
    size_t arity;

    if ( !PL_get_name_arity(head, &name, &arity) )
    { PL_type_error("option", head);
      goto pl_error;
    }

    if ( arity == 1 )
    { _PL_get_arg(1, head, a);

      if ( name == ATOM_mp_mmapsize )	/* mp_mmapsize */
      { size_t v;

	if ( !PL_get_size_ex(a, &v) )
	  return FALSE;
	env->env->set_mp_mmapsize(env->env, v);
	flags |= DB_INIT_MPOOL;
      } else if ( name == ATOM_mp_size ) /* mp_size */
      { size_t v;

	if ( !PL_get_size_ex(a, &v) )
	  return FALSE;
	env->env->set_cachesize(env->env, 0, v, 0);
	flags |= DB_INIT_MPOOL;
      } else if ( name == ATOM_thread_count )
      { size_t v;

	if ( !PL_get_size_ex(a, &v) )
	  return FALSE;
	env->env->set_thread_count(env->env, v);
      } else if ( name == ATOM_home )	/* db_home */
      {	if ( !PL_get_file_name(a, &home,
			       PL_FILE_OSPATH|PL_FILE_EXIST|PL_FILE_ABSOLUTE) )
	  goto pl_error;
      } else if ( name == ATOM_config )	/* db_config */
      { term_t h = PL_new_term_ref();
	term_t a2 = PL_new_term_ref();

	while(PL_get_list(a, h, a))
	{ atom_t nm;
	  size_t ar;
	  const char *n;
	  char *v;

	  if ( !PL_get_name_arity(h, &nm, &ar) || ar !=	1 )
	  { PL_domain_error("db_config", h);
	    goto pl_error;
	  }
	  _PL_get_arg(1, h, a2);
	  if ( !PL_get_chars(a2, &v, CVT_ATOM|CVT_STRING|CVT_EXCEPTION) )
	    goto pl_error;
	  n = PL_atom_chars(nm);
	  if ( !(config[nconf] = malloc(strlen(n)+strlen(v)+2)) )
	  { PL_resource_error("memory");
	    goto pl_error;
	  }
	  strcpy(config[nconf], n);
	  strcat(config[nconf], " ");
	  strcat(config[nconf], v);
	  config[++nconf] = NULL;
	}
	if ( !PL_get_nil_ex(a) )
	  goto pl_error;
      } else
      { u_int32_t fv = lookup_flag(dbenv_flags, name, a);

	switch(fv)
	{ case F_ERROR:
	    goto pl_error;
	  case F_UNPROCESSED:
	    PL_domain_error("db_option", head);
	    goto pl_error;
	  default:
	    flags |= fv;
	}
      }
    } else
    { PL_type_error("db_option", head);
      goto pl_error;
    }
  }

  if ( !PL_get_nil_ex(options) )
    goto pl_error;

  if ( (rval=env->env->open(env->env, home, flags, 0666)) != 0 )
    goto db_error;
  if ( newenv && !unify_dbenv(newenv, env) )
    goto pl_error;

  env->flags = flags;
  env->home  = strdup(home);
  if ( !(flags&DB_THREAD) )
    env->thread = PL_thread_self();

  if ( !rval )
    return TRUE;

pl_error:
  bdb_close_env(env, TRUE);
  return FALSE;

db_error:
  db_status_env(rval, env);
  bdb_close_env(env, TRUE);
  return FALSE;
}

static foreign_t
pl_bdb_init1(term_t option_list)
{ return bdb_init(0, option_list);
}

static foreign_t
pl_bdb_init2(term_t env, term_t option_list)
{ return bdb_init(env, option_list);
}

static foreign_t
pl_bdb_close_environment(term_t handle)
{ dbenvh *db_env;

  if ( get_dbenv(handle, &db_env) )
    return bdb_close_env(db_env, FALSE);

  return FALSE;
}

static foreign_t
pl_bdb_is_open_env(term_t t)
{ PL_blob_t *type;
  void *data;
  atom_t a;

  if ( PL_get_blob(t, &data, NULL, &type) && type == &dbenv_blob)
  { dbenvh *p = data;

    if ( p->symbol )
      return TRUE;

    return FALSE;
  } else if ( PL_get_atom(t, &a) && a == ATOM_default )
  { return TRUE;
  }

  return PL_type_error("bdb_env", t);
}


static foreign_t
pl_bdb_env_property(term_t t, term_t prop)
{ dbenvh *env;

  if ( get_dbenv(t, &env ) )
  { atom_t name;
    size_t arity;

    if ( PL_get_name_arity(prop, &name, &arity) && arity == 1 )
    { term_t a = PL_new_term_ref();
      u_int32_t flag;

      _PL_get_arg(1, prop, a);
      if ( name == ATOM_home && env->home )
	return PL_unify_atom_chars(a, env->home);
      else if ( (flag=lookup_flag(dbenv_flags,name,0)) != F_UNPROCESSED )
	return PL_unify_bool(a, env->flags&flag);
    }
  }

  return FALSE;
}


static foreign_t
pl_bdb_version(term_t v)
{ return PL_unify_integer(v,
			  DB_VERSION_MAJOR*10000 +
			  DB_VERSION_MINOR*100   +
			  DB_VERSION_PATCH);
}


install_t
install_bdb4pl(void)
{ initConstants();
  PL_license("agpl", "BerkeleyDB (libdb, used by library(bdb))");

#define NDET PL_FA_NONDETERMINISTIC

  PL_register_foreign("bdb_open",	       4, pl_bdb_open,		    0);
  PL_register_foreign("bdb_close",	       1, pl_bdb_close,		    0);
  PL_register_foreign("bdb_is_open",	       1, pl_bdb_is_open,	    0);
  PL_register_foreign("bdb_put",	       3, pl_bdb_put,		    0);
  PL_register_foreign("bdb_del",	       2, pl_bdb_del2,		    0);
  PL_register_foreign("bdb_del",	       3, pl_bdb_del3,		    NDET);
  PL_register_foreign("bdb_getall",	       3, pl_bdb_getall,	    0);
  PL_register_foreign("bdb_get",	       3, pl_bdb_get,		    NDET);
  PL_register_foreign("bdb_enum",	       3, pl_bdb_enum,		    NDET);
  PL_register_foreign("bdb_init",	       1, pl_bdb_init1,		    0);
  PL_register_foreign("bdb_init",	       2, pl_bdb_init2,		    0);
  PL_register_foreign("bdb_close_environment", 1, pl_bdb_close_environment, 0);
  PL_register_foreign("bdb_is_open_env",       1, pl_bdb_is_open_env,	    0);
  PL_register_foreign("bdb_env_property",      2, pl_bdb_env_property,	    0);
  PL_register_foreign("bdb_transaction",       1, pl_bdb_transaction1,	    0);
  PL_register_foreign("bdb_transaction",       2, pl_bdb_transaction2,	    0);
  PL_register_foreign("bdb_version",           1, pl_bdb_version,	    0);

  pthread_key_create(&transaction_key, free_transaction_stack);
  default_env.symbol = ATOM_default;
}


install_t
uninstall(void)
{ if ( transaction_key )
  { pthread_key_delete(transaction_key);
    transaction_key = 0;
  }
  bdb_close_env(&default_env, TRUE);
}
