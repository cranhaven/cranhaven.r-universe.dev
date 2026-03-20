/* glpkR.h
   R interface to GLPK.
 
   Copyright (C) 2011-2014 Gabriel Gelius-Dietrich, Dpt. for Bioinformatics,
   Institute for Informatics, Heinrich-Heine-University, Duesseldorf, Germany.
   All right reserved.
   Email: geliudie@uni-duesseldorf.de
 
   This file is part of glpkAPI.
 
   GlpkAPI is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
 
   GlpkAPI is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
 
   You should have received a copy of the GNU General Public License
   along with glpkAPI.  If not, see <http://www.gnu.org/licenses/>.
*/


#include <stdlib.h>
#include <glpk.h>


/* avoid remapping of Rf_<function> to <function> in R header files */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif /* R_NO_REMAP */

/* use strict R headers */
#ifndef STRICT_R_HEADERS
#define STRICT_R_HEADERS
#endif /* STRICT_R_HEADERS */

#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */


/* -------------------------------------------------------------------------- */
/* check arguments to GLPK */
#ifdef CHECK_GLPK_ARGS
#define checkRowIndex(p, r) do { \
    if ( (Rf_asInteger(r) > glp_get_num_rows(R_ExternalPtrAddr(p))) || \
         (Rf_asInteger(r) < 1) ) \
        Rf_error("Row index '%i' is out of range!", Rf_asInteger(r)); \
} while (0)
#define checkColIndex(p, c) do { \
    if ( (Rf_asInteger(c) > glp_get_num_cols(R_ExternalPtrAddr(p))) || \
         (Rf_asInteger(c) < 1) ) \
        Rf_error("Column index '%i' is out of range!", Rf_asInteger(c)); \
} while (0)
#define checkVarType(v) do { \
    if ( (Rf_asInteger(v) > GLP_FX) || (Rf_asInteger(v) < GLP_FR) ) \
        Rf_error("Invalid variable type '%i'!", Rf_asInteger(v)); \
} while (0)
#define checkVarKind(v) do { \
    int rv = Rf_asInteger(v); \
    if ( (rv != GLP_CV) && (rv != GLP_IV) && (rv != GLP_BV) ) \
        Rf_error("Invalid variable kind '%i'!", Rf_asInteger(v)); \
} while (0)
#define checkVarStat(v) do { \
    int rv = Rf_asInteger(v); \
    if ( (rv != GLP_BS) && (rv != GLP_NL) && (rv != GLP_NU)  && (rv != GLP_NF) && (rv != GLP_NS) ) \
        Rf_error("Invalid variable status '%i'!", Rf_asInteger(v)); \
} while (0)
#define checkSolType(v) do { \
    int rv = Rf_asInteger(v); \
    if ( (rv != GLP_SOL) && (rv != GLP_IPT) && (rv != GLP_MIP) ) \
        Rf_error("Invalid variable status '%i'!", Rf_asInteger(v)); \
} while (0)
#define checkScaling(v) do { \
    int rv = Rf_asInteger(v); \
    if ( (rv != GLP_SF_GM) && (rv != GLP_SF_EQ) && (rv != GLP_SF_2N) && (rv != GLP_SF_SKIP) && (rv != GLP_SF_AUTO) ) \
        Rf_error("Invalid scaling option '%i'!", Rf_asInteger(v)); \
} while (0)
#define checkVarTypes(v) do { \
    int y = 0; \
    const int *rv; \
    if (TYPEOF(v) == INTSXP) { \
        rv = INTEGER(v); \
        while (y < Rf_length(v)) { \
            if ( ((rv[y]) > GLP_FX) || ((rv[y]) < GLP_FR) ) { \
                Rf_error("Variable type 'type[%i] = %i' is invalid!", (y+1), rv[y]); \
            } \
            y++; \
        } \
    } \
} while (0)
#define checkVarKinds(v) do { \
    int y = 0; \
    const int *rv; \
    if (TYPEOF(v) == INTSXP) { \
        rv = INTEGER(v); \
        while (y < Rf_length(v)) { \
            if ( ((rv[y]) != GLP_CV) && ((rv[y]) != GLP_IV) && ((rv[y]) != GLP_BV) ) { \
                Rf_error("Variable kind 'kind[%i] = %i' is invalid!", (y+1), rv[y]); \
            } \
            y++; \
        } \
    } \
} while (0)
#define checkRowIndices(p, r, s) do { \
    int y = s ? 1 : 0; \
    int nr = glp_get_num_rows(R_ExternalPtrAddr(p)); \
    const int *rr = INTEGER(r); \
    while (y < Rf_length(r)) { \
        if ( ((rr[y]) > nr) || ((rr[y]) < 1) ) { \
            Rf_error("Row index 'i[%i] = %i' is out of range!", s ? y : (y+1), rr[y]); \
        } \
        y++; \
    } \
} while (0)
#define checkColIndices(p, c, s) do { \
    int y = s ? 1 : 0; \
    int nc = glp_get_num_cols(R_ExternalPtrAddr(p)); \
    const int *rc = INTEGER(c); \
    while (y < Rf_length(c)) { \
        if ( ((rc[y]) > nc) || ((rc[y]) < 1) ) { \
            Rf_error("Column index 'j[%i] = %i' is out of range!", s ? y : (y+1), rc[y]); \
        } \
        y++; \
    } \
} while (0)
#define checkVecLen(l, v) do { \
    if ( Rf_length(v) != Rf_asInteger(l) ) { \
        Rf_error("Vector does not have length %i!", Rf_asInteger(l)); \
    } \
} while (0)
#if defined(R_VERSION) && R_VERSION >= R_Version(3, 0, 0)
#define checkDupIndices(m, n, ne) do { \
    int *oind; \
    int dupA = 0; \
    int dupB = 0; \
    int y = 1; \
    const int *rm = INTEGER(m); \
    const int *rn = INTEGER(n); \
    oind = R_Calloc(Rf_asInteger(ne), int); \
    R_orderVector(oind, Rf_asInteger(ne), PROTECT(Rf_lang2(m, n)), TRUE, FALSE); \
    while (y < Rf_asInteger(ne)) { \
        if ( (rm[oind[y-1]] == rm[oind[y]]) && (rn[oind[y-1]] == rn[oind[y]]) ) { \
            dupA = oind[y-1]; \
            dupB = oind[y]; \
            break; \
        } \
        y++; \
    } \
    R_Free(oind); \
    if (dupB) { \
        Rf_error("Duplicate indices 'ia[%i] = ia[%i] = %i' and 'ja[%i] = ja[%i] = %i' not allowed!", dupA+1, dupB+1, rm[dupA], dupA+1, dupB+1, rn[dupB]); \
    } \
    UNPROTECT(1); \
} while (0)
#else
#define checkDupIndices(m, n, ne)
#endif
#else
#define checkRowIndex(p, r)
#define checkColIndex(p, c)
#define checkVarType(v)
#define checkVarKind(v)
#define checkVarStat(v)
#define checkSolType(v)
#define checkScaling(v)
#define checkVarTypes(v)
#define checkVarKinds(v)
#define checkRowIndices(p, r)
#define checkColIndices(p, c)
#define checkVecLen(l, v)
#define checkDupIndices(m, n, ne)
#endif


/* -------------------------------------------------------------------------- */
/* NULL */
#define checkIfNil(cp) do { \
    if (R_ExternalPtrAddr(cp) == NULL) \
        Rf_error("You passed a nil value!"); \
} while (0)


/* -------------------------------------------------------------------------- */
/* problem */
#define checkTypeOfProb(cp) do { \
    if ( (TYPEOF(cp) != EXTPTRSXP) || (R_ExternalPtrTag(cp) != tagGLPKprob) ) \
        Rf_error("You must pass a glpk problem structure!"); \
} while (0)

#define checkProb(p) do { \
    checkIfNil(p); \
    checkTypeOfProb(p); \
} while (0)


/* -------------------------------------------------------------------------- */
/* parameters */
#define checkTypeOfParm(pa) do { \
    if ( (TYPEOF(pa) != EXTPTRSXP) || (R_ExternalPtrTag(pa) != tagGLPKparm) ) \
        Rf_error("You must pass a pointer to an glpk parameter structure!"); \
} while (0)

#define checkParm(p) do { \
    checkIfNil(p); \
    checkTypeOfParm(p); \
} while (0)


/* -------------------------------------------------------------------------- */
/* MathProg */
#define checkTypeOfMathProg(mp) do { \
    if ( (TYPEOF(mp) != EXTPTRSXP) || (R_ExternalPtrTag(mp) != tagMATHprog) ) \
        Rf_error("You must pass a pointer to an MathProg translator workspace!"); \
} while (0)

#define checkMathProg(p) do { \
    checkIfNil(p); \
    checkTypeOfMathProg(p); \
} while (0)
