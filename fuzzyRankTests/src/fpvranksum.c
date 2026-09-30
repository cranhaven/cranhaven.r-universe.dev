
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include "fuzzyranktests.h"
#include "myutil.h"
#include <string.h>

// see section 6.7.1 of Writing R Extensions
#include "Rversion.h"
#if R_VERSION < R_Version(4, 2, 0)
extern void wilcox_free(void);
#endif

SEXP fpvranksum(SEXP xin, SEXP yin, SEXP muin, SEXP altin, SEXP tolin)
{
    double *x;
    double *y;
    double mu = getScalarReal(muin);
    double tol = getScalarReal(tolin);
    const char *alt = getScalarCharacter(altin);
    int i, j;
    int nx;
    int ny;
    int two_tail;
    int lower_tail;
    int ntieclass, itieclass;
    int *mtie = 0;
    int *ntie = 0;
    int t = 0;
    int itie;
    double *cdftie;
    double *pdftie;
    double *buf1;
    double *buf2;
    int l = 0;
    int u = 0;
    int wmin;
    int wmax;
    double *knots;
    int lenout;
    SEXP result;

    if (! isReal(xin))
        error("'x' must be real");
    if (! isAllFinite(xin))
        error("'x' must be all finite");
    if (! isReal(yin))
        error("'y' must be real");
    if (! isAllFinite(yin))
        error("'y' must be all finite");
    x = REAL(xin);
    y = REAL(yin);
    nx = LENGTH(xin);
    ny = LENGTH(yin);
    if (nx < 1)
        error("'x' length 0");
    if (ny < 1)
        error("'y' length 0");
    for (i = 1; i < nx; ++i)
        if (x[i] < x[i - 1])
            error("'x' must be nondecreasing vector");
    for (i = 1; i < ny; ++i)
        if (y[i] < y[i - 1])
            error("'y' must be nondecreasing vector");

   if (strncmp(alt, "less", 20) == 0) {
        lower_tail = 1;
        two_tail = 0;
    } else if (strncmp(alt, "greater", 20) == 0) {
        lower_tail = 0;
        two_tail = 0;
    } else if (strncmp(alt, "two.sided", 20) == 0) {
        lower_tail = 0;
        two_tail = 1;
    } else {
        error("'alternative' not recognized");
    }

    /* do twice to avoid mallocing 2 * length(x) memory */
    ntieclass = 0;
    for (i = 0, j = 0; i < nx; ++i)
        if (i == 0 || x[i] > x[i - 1] + tol) {
            while (j < ny && y[j] + mu < x[i] - tol)
                ++j;
            if (j >= ny)
                break;
            /* now j < ny && y[j] + mu >= x[i] - tol */
            if (y[j] + mu <= x[i] + tol)
                ++ntieclass;
            while (j < ny && y[j] + mu <= x[i] + tol)
                ++j;
            /* now j >= ny || y[j] + mu > x[i] + tol */
        }

    if (ntieclass > 0) {
        mtie = (int *) R_alloc(ntieclass, sizeof(int));
        ntie = (int *) R_alloc(ntieclass, sizeof(int));

        itieclass = -1;
        for (i = 0, j = 0; i < nx; ) {
            /* process tie class containing x[i], if any */
            while (j < ny && y[j] + mu < x[i] - tol)
                ++j;
            if (j >= ny)
                break;
            /* now j < ny && y[j] + mu >= x[i] - tol */
            if (y[j] + mu <= x[i] + tol) {
                /* we have a tie class */
                double tieval = x[i];
                if (y[j] + mu > tieval)
                    tieval = y[j] + mu;
                ++itieclass;
                mtie[itieclass] = 1;
                ntie[itieclass] = 1;
                ++i;
                ++j;
                while (i < nx && x[i] <= tieval + tol) {
                    mtie[itieclass]++;
                    if (x[i] > tieval)
                        tieval = x[i];
                    i++;
                }
                while (j < ny && y[j] + mu <= tieval + tol) {
                    ntie[itieclass]++;
                    if (y[j] + mu > tieval)
                        tieval = y[j] + mu;
                    j++;
                }
                /* now i >= nx || j >= ny || x[i] > tieval + tol ||
                       y[j] + mu > tieval + tol */
            } else {
                ++i;
            }
        }

        for (itieclass = 0; itieclass < ntieclass; ++itieclass)
            t += mtie[itieclass] * ntie[itieclass];
    }

    cdftie = (double *) R_alloc(t + 2, sizeof(double));
    cdftie[0] = 0.0;
    pdftie = cdftie + 1;
    pdftie[0] = 1.0;
    for (itie = 1; itie <= t; ++itie)
        pdftie[itie] = 0.0;

    if (ntieclass > 0) {
        buf1 = (double *) R_alloc(t + 1, sizeof(double));
        buf2 = (double *) R_alloc(t + 1, sizeof(double));

        for (itieclass = 0; itieclass < ntieclass; ++itieclass) {
            int mym = mtie[itieclass];
            int myn = ntie[itieclass];
            for (i = 0; i <= t; ++i) {
                buf1[i] = pdftie[i];
                pdftie[i] = 0.0;
            }
            for (j = 0; j <= mym * myn; ++j)
                buf2[j] = dwilcox(j, mym, myn, FALSE);
            for (i = 0; i <= t; ++i)
                for (j = 0; j <= mym * myn; ++j)
                    if (i + j <= t)
                        pdftie[i + j] += buf1[i] * buf2[j];
        }
    }

    for (i = 0; i < nx; ++i)
        for (j = 0; j < ny; ++j)
            if (x[i] > y[j] + mu + tol)
                ++l;
            else if (x[i] < y[j] + mu - tol)
                ++u;

    if (two_tail) {
        if (l < u)
            wmin = l;
        else
            wmin = u;
        wmax = wmin;
        for (i = 0; i <= t; ++i) {
           int w = wmin + i;
           int N = nx * ny;
           if (N - w < w)
               w = N - w;
           if (w > wmax)
               wmax = w;
        }
        for (i = 0; i <= t; ++i) {
           int N = nx * ny;
           int w = wmin + i;
           if (N - w < w) {
               w = N - w;
               j = w - wmin;
               pdftie[j] += pdftie[i];
           }
        }
    } else {
        if (lower_tail)
            wmin = l;
        else
            wmin = u;
        wmax = wmin + t;
    }

    knots = (double *) R_alloc(t + 2, sizeof(double));
    for (i = 0; i <= t + 1; ++i) {
        double fred = pwilcox(wmin + i - 1, nx, ny, TRUE, FALSE);
        if (two_tail)
           fred *= 2.0;
        knots[i] = fred;
    }
    // see section 6.7.1 of Writing R Extensions
    wilcox_free();

    for (i = 0; i <= t; ++i)
        cdftie[i + 1] += cdftie[i];
    if (2 * wmax == nx * ny)
        knots[wmax - wmin + 1] = 1.0;

    lenout = wmax - wmin + 2;

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
        for (i = 0; i < lenout; ++i) {
            REAL(resultknots)[i] = knots[i];
            REAL(resultvalues)[i] = cdftie[i];
        }
        UNPROTECT(3);
    }

    UNPROTECT(1);
    return result;
}

