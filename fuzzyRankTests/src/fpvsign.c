
// if mu is hypothesized value, then ll == sum(x < mu), uu == sum(x > mu)
// and ll == sum(x == mu).  tailsexp == 2 if and only if a two tailed test
// is wanted.  Otherwise does upper-tailed test.  Swap arguments ll and uu
// to do lower-tailed test.

#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include "fuzzyranktests.h"
#include "myutil.h"

SEXP fpvsign(SEXP ll, SEXP tt, SEXP uu, SEXP tailsexp)
{
    int l = getScalarInteger(ll);
    int t = getScalarInteger(tt);
    int u = getScalarInteger(uu);
    int tails = getScalarInteger(tailsexp);

    int n = l + t + u;
    double *knots = (double *) R_alloc(t + 2, sizeof(double));
    double *values = (double *) R_alloc(t + 2, sizeof(double));
    double half = 1.0 / 2.0;
    int k;
    int lenout;
    SEXP result;

    for (k = 0; k <= t + 1; ++k) {
        knots[k] = pbinom(l + k - 1, n, half, TRUE, FALSE);
        values[k] = pbinom(k - 1, t, half, TRUE, FALSE);
    }
    /* pbinom doesn't like t = 0 so */
    if (t == 0) {
        values[0] = 0.0;
        values[1] = 1.0;
    }

    lenout = t + 2;

    if (tails == 2) {

        int wbot = l;
        int wtop = l + t;

        if (u < l) {
            wbot = u;
            wtop = u + t;
            for (k = 0; k <= t + 1; ++k)
                knots[k] = pbinom(u + k - 1, n, half, TRUE, FALSE);
        }

        for (k = 0; k <= t + 1; ++k)
            knots[k] *= 2.0; 

        if (2 * wtop == n)
            knots[t + 1] = 1.0;

        if (2 * wtop > n) {
            int kkmax = 0;
            for (k = 0; k <= t + 1; ++k)
                values[k] = 0.0;
            for (k = 0; k <= t; ++k) {
                int kk = k;
                int w = wbot + k;
                if (n - w < w) {
                    w = n - w;
                    kk = w - wbot;
                }
                if (kk > kkmax)
                    kkmax = kk;
                values[kk + 1] += dbinom(k, t, half, FALSE);
            }
            /* at this point values[0] == 0.0 and for k = 0, ..., kkmax
            *  values[k + 1] is the mixing probability for the interval
            *  with endpoints knots[k] and knots[k + 1], except perhaps
            *  we need to fixup knots[kkmax + 1]
            */
            for (k = 0; k <= kkmax; ++k)
                values[k + 1] += values[k];
            lenout = kkmax + 2;
            knots[kkmax + 1] = 1.0;
        }
    }

    PROTECT(result = allocVector(VECSXP, 2));
    {
        SEXP resultnames, resultknots, resultvalues;
        PROTECT(resultnames = allocVector(STRSXP, 2));
        SET_STRING_ELT(resultnames, 0, mkChar("knots"));
        SET_STRING_ELT(resultnames, 1, mkChar("values"));
        namesgets(result, resultnames);
        PROTECT(resultknots = allocVector(REALSXP, lenout));
        PROTECT(resultvalues = allocVector(REALSXP, lenout));
        SET_VECTOR_ELT(result, 0, resultknots);
        SET_VECTOR_ELT(result, 1, resultvalues);
        for (k = 0; k < lenout; ++k) {
            REAL(resultknots)[k] = knots[k];
            REAL(resultvalues)[k] = values[k];
        }
        UNPROTECT(3);

    }

    UNPROTECT(1);
    return result;
}

