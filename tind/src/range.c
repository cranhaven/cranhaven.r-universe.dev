/*
 * This file is a part of tind.
 *
 * Copyright (c) Grzegorz Klima 2025
 *
 ******************************************************
 * fast range for integer and double vectors w/o Infs *
 ******************************************************
 */


#include "range.h"
#include "compiler.h"


TIND__ATTRIBUTE_FUNCTION
SEXP
range(SEXP sx, SEXP snarm)
{
    size_t i, n = XLENGTH(sx);
    int real = isReal(sx), nna = 0, narm = *INTEGER(snarm);
    SEXP sy;

    if (real) {
        const double *x = REAL(sx);
        double m = NA_REAL, M = NA_REAL;
        for (i = 0; i < n; ++i) {
            if (ISNA(x[i])) {
                if (narm) continue;
                m = M = NA_REAL;
                break;
            }
            if (!nna) {
                ++nna;
                m = M = x[i];
                continue;
            }
            if (m > x[i]) m = x[i]; else if (M < x[i]) M = x[i];
        }
        sy = PROTECT(allocVector(REALSXP, 2));
        REAL(sy)[0] = m;
        REAL(sy)[1] = M;
    } else {
        const int *x = INTEGER(sx);
        int m = NA_INTEGER, M = NA_INTEGER;
        for (i = 0; i < n; ++i) {
            if (x[i] == NA_INTEGER) {
                if (narm) continue;
                m = M = NA_INTEGER;
                break;
            }
            if (!nna) {
                ++nna;
                m = M = x[i];
                continue;
            }
            if (m > x[i]) m = x[i]; else if (M < x[i]) M = x[i];
        }
        sy = PROTECT(allocVector(INTSXP, 2));
        INTEGER(sy)[0] = m;
        INTEGER(sy)[1] = M;
    }

    UNPROTECT(1);
    return sy;
}

