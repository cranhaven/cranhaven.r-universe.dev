/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, University of Amsterdam
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

#ifndef DB4PL_H_INCLUDED
#define DB4PL_H_INCLUDED

#include <SWI-Prolog.h>
#if   defined(HAVE_DB6_DB_H)
#include <db6/db.h>
#elif defined(HAVE_DB5_DB_H)
#include <db5/db.h>
#elif defined(HAVE_DB4_DB_H)
#include <db4/db.h>
#elif defined(HAVE_DB3_DB_H)
#include <db3/db.h>
#else
#include <db.h>
#endif

/* Consider anything >= DB4.3 as DB43 */
#if DB_VERSION_MAJOR >= 4
#if DB_VERSION_MAJOR > 4 || DB_VERSION_MINOR >= 3
#define DB43 1
#endif
#endif

/* Consider anything >= DB4.1 as DB41 */
#if DB_VERSION_MAJOR >= 4
#if DB_VERSION_MAJOR > 4 || DB_VERSION_MINOR >= 1
#define DB41 1
#endif
#endif

#define DBH_MAGIC 277484232		/* magic for validation */
#define DBH_ENVMAGIC 6560701		/* magic for validation */

typedef enum
{ D_TERM,				/* a Prolog term */
  D_ATOM,				/* an atom (length+cahsr) */
  D_CBLOB,				/* a C-blob (bytes) */
  D_CSTRING,				/* a C-string (0-terminated) */
  D_CLONG				/* a C-long */
} dtype;

typedef struct
{ DB_ENV       *env;			/* the database environment */

  atom_t	symbol;			/* <bdb_env>(...)  */
  int		magic;			/* DBH_MAGIC */
  u_int32_t	flags;			/* flags used to create the env */
  int		thread;			/* associated thread */
  char	       *home;			/* Directory */
} dbenvh;

typedef struct
{ DB	       *db;			/* the database */

  atom_t	symbol;			/* <bdb>(...)  */
  int		magic;			/* DBH_MAGIC */
  u_int32_t	flags;			/* flags used to open the database */
  dtype		key_type;		/* type of the key */
  dtype		value_type;		/* type of the data */
  dbenvh       *env;			/* associated environment */
} dbh;

#endif /*DB4PL_H_INCLUDED*/
