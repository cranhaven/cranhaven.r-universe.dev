
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include "fuzzyranktests.h"
#include "myutil.h"
#include <math.h>
#include <string.h>

// see section 6.7.1 of Writing R Extensions
#include "Rversion.h"
#if R_VERSION < R_Version(4, 2, 0)
extern void wilcox_free(void);
extern void signrank_free(void);
#endif

SEXP fpvsignrank(SEXP xin, SEXP muin, SEXP altin, SEXP tolin)
{
    double *x;
    double mu = getScalarReal(muin);
    double tol = getScalarReal(tolin);
    const char *alt = getScalarCharacter(altin);
    int nx;
    int two_tail;
    int lower_tail;
    int i, j;
    int ilastminus;
    int *tieclasslabel;
    double tieval;
    int ntieclass;
    int ntiezero;
    int ntieclass_mann_whitney;
    int *mtie, *ntie;
    int l, t, u, N;
    double *cdftie, *pdftie;
    int itie, itieclass;
    double *knots;
    int wmax, wmin;
    int lenout;
    SEXP result;

    if (! isReal(xin))
        error("'x' must be real");
    if (! isAllFinite(xin))
        error("'x' must be all finite");
    x = REAL(xin);
    nx = LENGTH(xin);
    if (nx < 1)
        error("'x' length 0");
    for (i = 1; i < nx; ++i)
        if (x[i] < x[i - 1])
            error("'x' must be nondecreasing vector");

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

    tieclasslabel = (int *) R_alloc(nx, sizeof(int));
    for (i = 0; i < nx; ++i)
        tieclasslabel[i] = 0;

    for (i = 0, ilastminus = i - 1; i < nx; ++i) {
        double fred = x[i] - mu;
        if (fred < 0.0)
            ++ilastminus;
    }

    ntieclass = 0;
    tieval = 0.0;
    for (i = ilastminus, j = i + 1; i >= 0 || j < nx; ) {
        if (i >= 0 && j < nx) {
            double minusval = mu - x[i];
            double plusval = x[j] - mu;
            if (minusval < plusval) {
               if (minusval > tieval + tol)
                   ++ntieclass;
               tieval = minusval;
               tieclasslabel[i] = ntieclass;
               --i;
            } else {
               if (plusval > tieval + tol)
                   ++ntieclass;
               tieval = plusval;
               tieclasslabel[j] = ntieclass;
               ++j;
            }
        } else if (i >= 0) {
            double minusval = mu - x[i];
            if (minusval > tieval + tol)
                ++ntieclass;
            tieval = minusval;
            tieclasslabel[i] = ntieclass;
            --i;
        } else if (j < nx) {
            double plusval = x[j] - mu;
            if (plusval > tieval + tol)
                ++ntieclass;
            tieval = plusval;
            tieclasslabel[j] = ntieclass;
            ++j;
        }
    }

    ntiezero = 0;
    for (i = 0; i < nx; ++i)
        if (tieclasslabel[i] == 0)
            ++ntiezero;

    ntieclass_mann_whitney = 0;
    for (i = 1; i <= ntieclass; ++i) {
        int thisclassminus = 0;
        int thisclassplus = 0;
        for (j = ilastminus; j >= 0; --j)
            if (tieclasslabel[j] == i)
                ++thisclassminus;
        for (j = ilastminus + 1; j < nx; ++j)
            if (tieclasslabel[j] == i)
                ++thisclassplus;
        if (thisclassminus > 0 && thisclassplus > 0)
            ++ntieclass_mann_whitney;
#ifdef BLATHER
        printf("BLATHER: i = %d, thisclassminus = %d, thisclassplus = %d\n", i, thisclassminus, thisclassplus);
#endif /* BLATHER */
    }

    mtie = (int *) R_alloc(ntieclass_mann_whitney, sizeof(int));
    ntie = (int *) R_alloc(ntieclass_mann_whitney, sizeof(int));
    ntieclass_mann_whitney = 0;
    for (i = 1; i <= ntieclass; ++i) {
        int thisclassminus = 0;
        int thisclassplus = 0;
        for (j = ilastminus; j >= 0; --j)
            if (tieclasslabel[j] == i)
                ++thisclassminus;
        for (j = ilastminus + 1; j < nx; ++j)
            if (tieclasslabel[j] == i)
                ++thisclassplus;
        if (thisclassminus > 0 && thisclassplus > 0) {
            mtie[ntieclass_mann_whitney] = thisclassminus;
            ntie[ntieclass_mann_whitney] = thisclassplus;
            ++ntieclass_mann_whitney;
        }
    }

    N = nx * (nx + 1);
    N = N / 2;
    l = 0;
    t = 0;
    u = 0;

    for (i = 0; i < nx; ++i) {
        int fred = tieclasslabel[i];
        if (x[i] - mu < 0.0)
            fred = (- fred);
        if (fred == 0) {
            ++t;
        } else if (fred < 0) {
            ++l;
        } else {
            ++u;
        }
        for (j = i + 1; j < nx; ++j) {
            int sally = tieclasslabel[j];
            if (x[j] - mu < 0.0)
                sally = (- sally);
            if (fred + sally == 0) {
                ++t;
            } else if (fred + sally < 0) {
                ++l;
            } else {
                ++u;
            }
        }
    }

    cdftie = (double *) R_alloc(t + 2, sizeof(double));
    cdftie[0] = 0.0;
    pdftie = cdftie + 1;
    pdftie[0] = 1.0;
    for (itie = 1; itie <= t; ++itie)
        pdftie[itie] = 0.0;

#ifdef BLATHER
    printf("BLATHER: nx = %d, ilastminus = %d\n", nx, ilastminus);
    for (i = 0; i < nx; ++i)
        printf("BLATHER: x[%d] = %f, tieclasslabel[%d] = %d\n", i, x[i], i, tieclasslabel[i]);
    printf("BLATHER: ntiezero = %d\n", ntiezero);
    printf("BLATHER: ntieclass_mann_whitney = %d\n", ntieclass_mann_whitney);
    for (i = 0; i < ntieclass_mann_whitney; ++i)
        printf("BLATHER: mtie[%d] = %d, ntie[%d] = %d\n", i, mtie[i], i, ntie[i]);
    printf("BLATHER: N = %d, l = %d, t = %d, u = %d\n", N, l, t, u);
#endif /* BLATHER */

    if (ntieclass_mann_whitney > 0 || ntiezero > 0) {
        double *buf1 = (double *) R_alloc(t + 1, sizeof(double));
        double *buf2 = (double *) R_alloc(t + 1, sizeof(double));

        if (ntiezero > 0) {
            int myn = ntiezero * (ntiezero + 1);
            myn = myn / 2;
            for (j = 0; j <= myn; ++j)
                pdftie[j] = dsignrank(j, ntiezero, FALSE);
        }

        for (itieclass = 0; itieclass < ntieclass_mann_whitney; ++itieclass) {
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

#ifdef BLATHER
    for (i = 0; i <= t; ++i)
        printf("BLATHER: pdftie[%d] = %f\n", i, pdftie[i]);
#endif /* BLATHER */

    if (two_tail) {
#ifdef BLATHER
        printf("BLATHER: we are in two tail case\n");
#endif /* BLATHER */
        if (l < u)
            wmin = l;
        else
            wmin = u;
        wmax = wmin;
        for (i = 0; i <= t; ++i) {
           int w = wmin + i;
           if (N - w < w)
               w = N - w;
           if (w > wmax)
               wmax = w;
        }
        for (i = 0; i <= t; ++i) {
           int w = wmin + i;
           if (N - w < w) {
               w = N - w;
               j = w - wmin;
               pdftie[j] += pdftie[i];
           }
        }
    } else {
        if (lower_tail)
            wmin = u;
        else
            wmin = l;
        wmax = wmin + t;
    }

#ifdef BLATHER
    printf("BLATHER: wmin = %d, wmax = %d\n", wmin, wmax);
#endif /* BLATHER */

    knots = (double *) R_alloc(t + 2, sizeof(double));
    for (i = 0; i <= t + 1; ++i) {
        double fred = psignrank(wmin + i - 1, nx, TRUE, FALSE);
        if (two_tail)
           fred *= 2.0;
        knots[i] = fred;
    }
    // see section 6.7.1 of Writing R Extensions
    signrank_free();
    wilcox_free();

    for (i = 0; i <= t; ++i)
        cdftie[i + 1] += cdftie[i];
    if (2 * wmax == N)
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

