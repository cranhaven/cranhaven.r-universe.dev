// ### This file is part of 'PGRdup' package for R.

// ### Copyright (C) 2014-2020, ICAR-NBPGR.
// #
// # PGRdup is free software: you can redistribute it and/or modify it
// # under the terms of the GNU General Public License as published by
// # the Free Software Foundation, either version 2 of the License, or
// # (at your option) any later version.
// #
// # PGRdup is distributed in the hope that it will be useful, but
// # WITHOUT ANY WARRANTY; without even the implied warranty of
// # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// # GNU General Public License for more details.
// #
// #  A copy of the GNU General Public License is available at
// #  http://www.r-project.org/Licenses/

#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void fdouble_metaphone(char**, char**, char**, int*);

static const R_CMethodDef CEntries[] = {
  {"fdouble_metaphone", (DL_FUNC) &fdouble_metaphone, 4},
  {NULL, NULL, 0}
};

void R_init_PGRdup(DllInfo *dll)
{
  R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
