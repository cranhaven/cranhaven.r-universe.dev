#include <math.h>
#include "clp.h"
#include "clpsol.h"
#include "clpmisc.h"
#include <R.h>
#include <R_ext/BLAS.h>
#include <R_ext/Lapack.h>


double iprod(const vecCLP *x, const vecCLP *s)
{
    double z = 0.0;
    CLP_INT nLP, nblkSDP;
    nLP = x->nLP;
    nblkSDP = x->nblkSDP;

    if (nLP > 0)
    {
        CLP_INT incx = 1;
        CLP_INT incs = 1;
        z += F77_NAME(ddot)(&nLP, x->LP, &incx, s->LP, &incs);
    }
    if (nblkSDP > 0)
    {
        CLP_INT incx = 1;
        CLP_INT incs = 1;
        for (size_t k=0; k<nblkSDP; ++k)
        {
            CLP_INT n1 = x->nvblkSDP[k];
            CLP_INT n2 = n1 * n1;
            z += F77_NAME(ddot)(&n2, x->SDP[k], &incx, s->SDP[k], &incs);
        }
    }

    return z;
}

double extendedDualityGap(const regionInfo *rinfo,
    const vecCLP *x, const vecCLP *s)
{
    // CLP_INT N = rinfo->N;
    double dg = iprod(x, s);
    CLP_INT rid = find_region(rinfo, dg);

    double dg0 = 0.0;
    for (size_t i=0; i<rid; ++i)
    {
        dg0 += rinfo->unique_coeffs[i] * (double)rinfo->degNv[i];
    }

    return (dg - dg0);
    
}

double extendedNormalizedDualityGap(const regionInfo *rinfo,
    const vecCLP *x, const vecCLP *s)
{
    CLP_INT N = rinfo->N;
    double dg = iprod(x, s);
    CLP_INT rid = find_region(rinfo, dg);

    CLP_INT degs = 0;
    for (size_t i=rid; i<N; ++i)
    {
        degs += rinfo->degNv[i];
    }

    double dg0 = 0.0;
    for (size_t i=0; i<rid; ++i)
    {
        dg0 += rinfo->unique_coeffs[i] * (double)rinfo->degNv[i];
    }

    return (dg - dg0) / (double)degs;
    
}

CLP_INT search_region(const double *nuv, const CLP_INT N, const double x)
{
    CLP_INT idx0 = 0;
    CLP_INT idx1 = N-1;

    while(true)
    {
        if (idx0 >= idx1)
        {
            return idx0;
        }
        else
        {
            CLP_INT idx = (idx0 + idx1) / 2;
            if (x <= nuv[idx])
            {
                CLP_INT idxA = idx+1;
                if (idxA == N || nuv[idxA] < x)
                {
                    return (idxA);
                }
                else
                {
                    idx0 = idxA;
                }
            }
            else
            {
                idx1 = idx;
            }
        }
    }
}

CLP_INT find_region(const regionInfo *rinfo, const double x)
{
    return search_region(rinfo->nuv, rinfo->N, x);
}

CLP_INT compute_Aty(const ACLP *A, const double *y, vecCLP *z)
{
    INFO_CLP info = SUCCESS;
    CHECKNULL2(z);
    
    CLP_INT m, nLP, nblkSDP;
    m = A->m;
    nLP = A->nLP;
    nblkSDP = A->nblkSDP;

    if (nLP > 0)
    {
        char trans = 'N';
        double alpha = 1.0;
        double beta = 0.0;
        CLP_INT incy = 1;
        CLP_INT incz = 1;
        F77_NAME(dgemv)(&trans, &nLP, &m, &alpha, A->LP, &nLP, y, &incy,
            &beta, z->LP, &incz FCONE);
    }
    if (nblkSDP > 0)
    {
        for (size_t k=0; k<nblkSDP; ++k)
        {
            CLP_INT n1 = A->nvblkSDP[k];
            CLP_INT n2 = n1 * n1;
            CLP_INT incs = 1;
            CLP_INT incz = 1;
            double alpha = 0.0;
            F77_NAME(dscal)(&n2, &alpha, z->SDP[k], &incz);
            double *S = A->SDP[k];
            for (size_t j=0; j<m; ++j)
            {
                F77_NAME(daxpy)(&n2, &(y[j]), &(S[j*n2]), &incs, z->SDP[k], &incz);
            }
        }
    }

EXCEPTION:
    return info;
}

void mul_coeffvecCLP(const coeffCLP *coeff, vecCLP *z)
{
    CLP_INT nLP, nblkSDP;
    nLP = z->nLP;
    nblkSDP = z->nblkSDP;

    if (nLP > 0)
    {
        for (size_t i=0; i<nLP; ++i)
        {
            z->LP[i] *= coeff->LP[i];
        }
    }
    if (nblkSDP > 0)
    {
        CLP_INT incx = 1;
        for (size_t k=0; k<nblkSDP; ++k)
        {
            CLP_INT n1 = z->nvblkSDP[k];
            CLP_INT n2 = n1 * n1;
            double alpha = coeff->vSDP[k];
            F77_NAME(dscal)(&n2, &alpha, z->SDP[k], &incx);
        }
    }
}

void compute_Rp(const double *b, const ACLP *A, const vecCLP *x, double *Rp)
{
    CLP_INT m, nLP, nblkSDP;
    m = A->m;
    nLP = A->nLP;
    nblkSDP = A->nblkSDP;

    CLP_INT incb = 1;
    CLP_INT incy = 1;
    F77_NAME(dcopy)(&m, b, &incb, Rp, &incy);

    if (nLP > 0)
    {
        char trans = 'T';
        double alpha = -1.0;
        double beta = 1.0;
        CLP_INT incx = 1;
        CLP_INT incy = 1;
        F77_NAME(dgemv)(&trans, &nLP, &m, &alpha, A->LP, &nLP, x->LP, &incx,
            &beta, Rp, &incy FCONE);
    }
    if (nblkSDP > 0)
    {
        char trans = 'T';
        double alpha = -1.0;
        double beta = 1.0;
        CLP_INT incx = 1;
        CLP_INT incy = 1;
        for (size_t k=0; k<nblkSDP; ++k)
        {
            CLP_INT n1 = A->nvblkSDP[k];
            CLP_INT n2 = n1 * n1;
            F77_NAME(dgemv)(&trans, &n2, &m, &alpha, A->SDP[k], &n2,
                x->SDP[k], &incx, &beta, Rp, &incy FCONE);
        }
    }
}

void compute_Rd(const CLPinfo *clpinfo, const vecCLP *Asty, const vecCLP *c_s,
     const vecCLP *ss, vecCLP *Rd)
{
    CLP_INT nLP, nblkSDP;
    nLP = clpinfo->nLP;
    nblkSDP = clpinfo->nblkSDP;

    if (nLP > 0)
    {
        for (size_t i=0; i<nLP; ++i)
        {
            Rd->LP[i] = c_s->LP[i] - Asty->LP[i] - ss->LP[i];
        }
    }
    if (nblkSDP > 0)
    {
        for (size_t k=0; k<nblkSDP; ++k)
        {
            CLP_INT n1 = ss->nvblkSDP[k];
            CLP_INT n2 = n1 * n1;
            for (size_t i=0; i<n2; ++i)
            {
                Rd->SDP[k][i] = c_s->SDP[k][i] - Asty->SDP[k][i] - ss->SDP[k][i];
            }
        }
    }
}

void compute_Rc(const CLPinfo *clpinfo, const coeffCLP* nu, const vecCLP *dinv,
    const vecCLP *d, const vecCLP* cor, vecCLP *Rc)
{
    CLP_INT nLP, nblkSDP;
    nLP = clpinfo->nLP;
    nblkSDP = clpinfo->nblkSDP;

    if (nLP > 0)
    {
        if (NULLP(cor))
        {
            for (size_t i=0; i<nLP; ++i)
            {
                Rc->LP[i] = nu->LP[i] * dinv->LP[i] - d->LP[i];
            }            
        }
        else
        {
            for (size_t i=0; i<nLP; ++i)
            {
                Rc->LP[i] = nu->LP[i] * dinv->LP[i] - d->LP[i] - cor->LP[i];
            }        
        }
    }
    if (nblkSDP > 0)
    {
        if (NULLP(cor))
        {
            for (size_t k=0; k<nblkSDP; ++k)
            {
                CLP_INT n1 = clpinfo->nvblkSDP[k];
                CLP_INT n2 = n1 * n1;
                for (size_t i=0; i<n2; ++i)
                {
                    Rc->SDP[k][i] = nu->vSDP[k] * dinv->SDP[k][i] - d->SDP[k][i];
                }
            }            
        }
        else
        {
            for (size_t k=0; k<nblkSDP; ++k)
            {
                CLP_INT n1 = clpinfo->nvblkSDP[k];
                CLP_INT n2 = n1 * n1;
                for (size_t i=0; i<n2; ++i)
                {
                    Rc->SDP[k][i] = nu->vSDP[k] * dinv->SDP[k][i] - d->SDP[k][i]
                         - cor->SDP[k][i];
                }
            }     
        }
    }

}

void compute_nucoeffCLP(const CLPinfo *clpinfo, const regionInfo *rinfo,
    const double nu, const double dg, coeffCLP *coeff)
{
    CLP_INT N, nLP, rid;
    N = rinfo->N;
    nLP = clpinfo->nLP;
    // nblkSDP = clpinfo->nblkSDP;
    rid = find_region(rinfo, dg);

    for (size_t k=rid; k<N; ++k)
    {
        for (size_t j=rinfo->ridx[k]; j<rinfo->ridx[k+1]; ++j)
        {
            CLP_INT idx = rinfo->iv[j];
            if (idx < nLP)
            {
                coeff->LP[idx] = nu;
            }
            else
            {
                coeff->vSDP[idx-nLP] = nu;
            }
        }
    }
}

CLP_INT compute_cor(const CLPinfo *infoclp, const vecCLP *dx, const vecCLP *ds,
    const vecCLP *dinv, vecCLP *cor)
{
    INFO_CLP info = SUCCESS;
    CLP_INT nLP, nblkSDP;
    double *TZ1=NULL, *TZ2=NULL;
    nLP = infoclp->nLP;
    nblkSDP = infoclp->nblkSDP;

    if (nLP > 0)
    {
        for (size_t i=0; i<nLP; ++i)
        {
           cor->LP[i] = dx->LP[i] * ds->LP[i] * dinv->LP[i];
        }
    }
    if (nblkSDP > 0)
    {
        CLP_INT n1 = 0;
        for (size_t k=0; k<nblkSDP; ++k)
        {
            n1 = (infoclp->nvblkSDP[k] > n1) ? infoclp->nvblkSDP[k] : n1;
        }
        CLP_INT n2 = n1*n1;
        TZ1 = create_dvec(n2);
        TZ2 = create_dvec(n2);
        CHECKNULL2(TZ1);
        CHECKNULL2(TZ2);

        for (size_t k=0; k<nblkSDP; ++k)
        {
            CLP_INT n1 = infoclp->nvblkSDP[k];
            compute_quadcorSDP(n1, dx->SDP[k], ds->SDP[k], dinv->SDP[k],
                TZ1, TZ2, cor->SDP[k]);
        }
    }

EXCEPTION:
    CLP_FREE(TZ1);
    CLP_FREE(TZ2);
    return info;
}

CLP_INT scalingOpNTCLP(const vecCLP *x, const vecCLP *s, vecCLP *d, vecCLP *dinv,
    vecCLP *d05inv, vecCLP *g, vecCLP *ginv)
{
    INFO_CLP info = SUCCESS;
    CLP_INT nLP, nblkSDP;
    nLP = x->nLP;
    nblkSDP = x->nblkSDP;

    if (nLP > 0)
    {
        for (size_t i=0; i<nLP; ++i)
        {
            double xi = x->LP[i];
            double si = s->LP[i];
            double di = sqrt(xi*si);

            d->LP[i] = di;
            dinv->LP[i] = 1.0 / di;
            d05inv->LP[i] = 1.0 / sqrt(di);
            g->LP[i] = sqrt(xi/si);
            ginv->LP[i] = sqrt(si/xi);
        }
    }
    if (nblkSDP > 0)
    {
        for (size_t k=0; k<nblkSDP; ++k)
        {
            CLP_INT n1 = x->nvblkSDP[k];
            info = compute_scalingOpNTSDP(n1, n1, x->SDP[k], s->SDP[k],
                                    d->SDP[k], dinv->SDP[k], d05inv->SDP[k],
                                    g->SDP[k], ginv->SDP[k]);
            CHECKINFO(info);
        }
    }

EXCEPTION:
    return info;
}

CLP_INT scaleACLP(const CLPinfo *infoclp, const ACLP *A, const vecCLP *g, ACLP* A1)
{
    INFO_CLP info = SUCCESS;
    double *Z = NULL;
    CLP_INT m, nLP, nblkSDP;
    m = A->m;
    nLP = A->nLP;
    nblkSDP = A->nblkSDP;

    if (nLP > 0)
    {
        for (size_t j=0; j<m; ++j)
        {
            for (size_t i=0; i<nLP; ++i)
            {
                A1->LP[i+j*nLP] = A->LP[i+j*nLP] * g->LP[i];
            }
        }
    }
    if (nblkSDP > 0)
    {
        CLP_INT n0 = 0;
        for (size_t k=0; k<nblkSDP; ++k)
        {
            CLP_INT n1 =infoclp->nvblkSDP[k];
            n0 = (n1 > n0) ? n1 : n0;
        }
        Z = create_dvec(n0*n0);
        CHECKNULL2(Z);
        for (size_t k=0; k<nblkSDP; ++k)
        {
            CLP_INT n1 = infoclp->nvblkSDP[k];
            CLP_INT n2 = n1 * n1;
            for (size_t j=0; j<m; ++j)
            {
                scaleDualSDP(n1, &(A->SDP[k][j*n2]), g->SDP[k], Z, &(A1->SDP[k][j*n2]));
            }
        }
    }

EXCEPTION:
    CLP_FREE(Z);
    return info;
}

CLP_INT scaleDualvecCLP(const vecCLP *s, const vecCLP *g, vecCLP *ss)
{
    INFO_CLP info = SUCCESS;
    double *z = NULL;
    CLP_INT nLP, nblkSDP;
    nLP = s->nLP;
    nblkSDP = s->nblkSDP;

    if (nLP > 0)
    {
        for (size_t i=0; i<nLP; ++i)
        {
            ss->LP[i] = g->LP[i] * s->LP[i];
        }
    }
    if (nblkSDP > 0)
    {
        CLP_INT n0 = 0;
        for (size_t k=0; k<nblkSDP; ++k)
        {
            CLP_INT n1 = s->nvblkSDP[k];
            n0 = (n1 > n0) ? n1 : n0;
        }
        z = create_dvec(n0*n0);
        CHECKNULL2(z);

        for (size_t k=0; k<nblkSDP; ++k)
        {
            CLP_INT n1 = s->nvblkSDP[k];
            scaleDualSDP(n1, s->SDP[k], g->SDP[k], z, ss->SDP[k]);
        }
    }

EXCEPTION:
    CLP_FREE(z);
    return info;
}

CLP_INT scalebackDualvecCLP(const vecCLP *ss, const vecCLP *ginv, vecCLP *s)
{
    INFO_CLP info = SUCCESS;
    double *z = NULL;
    CLP_INT nLP, nblkSDP;
    nLP = ss->nLP;
    nblkSDP = ss->nblkSDP;

    if (nLP > 0)
    {
        for (size_t i=0; i<nLP; ++i)
        {
            s->LP[i] = ginv->LP[i] * ss->LP[i];
        }
    }
    if (nblkSDP > 0)
    {
        CLP_INT n0 = 0;
        for (size_t k=0; k<nblkSDP; ++k)
        {
            CLP_INT n1 = s->nvblkSDP[k];
            n0 = (n1 > n0) ? n1 : n0;
        }
        z = create_dvec(n0*n0);
        CHECKNULL2(z);

        for (size_t k=0; k<nblkSDP; ++k)
        {
            CLP_INT n1 = ss->nvblkSDP[k];
            scalebackDualSDP(n1, ss->SDP[k], ginv->SDP[k], z, s->SDP[k]);
        }
    }

EXCEPTION:
    CLP_FREE(z);
    return info;
}

CLP_INT scalebackPrimalvecCLP(const vecCLP *xs, const vecCLP *g, vecCLP *x)
{
    INFO_CLP info = SUCCESS;
    double *z = NULL;
    CLP_INT nLP, nblkSDP;
    nLP = xs->nLP;
    nblkSDP = xs->nblkSDP;

    if (nLP > 0)
    {
        for (size_t i=0; i<nLP; ++i)
        {
            x->LP[i] = g->LP[i] * xs->LP[i];
        }
    }
    if (nblkSDP > 0)
    {
        CLP_INT n0 = 0;
        for (size_t k=0; k<nblkSDP; ++k)
        {
            CLP_INT n1 = x->nvblkSDP[k];
            n0 = (n1 > n0) ? n1 : n0;
        }
        z = create_dvec(n0*n0);
        CHECKNULL2(z);

        for (size_t k=0; k<nblkSDP; ++k)
        {
            CLP_INT n1 = xs->nvblkSDP[k];
            scalebackPrimalSDP(n1, xs->SDP[k], g->SDP[k], z, x->SDP[k]);
        }
    }

EXCEPTION:
    CLP_FREE(z);
    return info;
}

/*
    x <- x + alpha*dx
*/
void update_vecCLP(const double alpha, const vecCLP *dx, vecCLP *x)
{
    CLP_INT nLP, nblkSDP;
    nLP = x->nLP;
    nblkSDP = x->nblkSDP;

    if (nLP > 0)
    {
        CLP_INT incx = 1;
        F77_NAME(daxpy)(&nLP, &alpha, dx->LP, &incx, x->LP, &incx);
    }
    if (nblkSDP > 0)
    {
        for (size_t k=0; k<nblkSDP; ++k)
        {
            CLP_INT n1 = x->nvblkSDP[k];
            CLP_INT n2 = n1 * n1;
            CLP_INT incx = 1;
            F77_NAME(daxpy)(&n2, &alpha, dx->SDP[k], &incx, x->SDP[k], &incx);
        }
    }
}

/*
    dx <- x + alpha*dx
*/
void update_vecCLP2(const vecCLP *x, const double alpha, vecCLP *dx)
{
    double beta = 1.0;
    CLP_INT nLP, nblkSDP;
    nLP = x->nLP;
    nblkSDP = x->nblkSDP;

    if (nLP > 0)
    {
        CLP_INT incx = 1;
        F77_NAME(dscal)(&nLP, &alpha, dx->LP, &incx);
        F77_NAME(daxpy)(&nLP, &beta, x->LP, &incx, dx->LP, &incx);
    }
    if (nblkSDP > 0)
    {
        for (size_t k=0; k<nblkSDP; ++k)
        {
            CLP_INT n1 = x->nvblkSDP[k];
            CLP_INT n2 = n1 * n1;
            CLP_INT incx = 1;
            F77_NAME(dscal)(&n2, &alpha, dx->SDP[k], &incx);
            F77_NAME(daxpy)(&n2, &beta, x->SDP[k], &incx, dx->SDP[k], &incx);
        }
    }
}

CLP_INT compute_stepsize(const regionInfo *rinfo, const CLP_INT rid, 
    const vecCLP *d, const vecCLP *d05inv, const vecCLP *dx_s, const vecCLP *ds_s,
    double *alpha, double *beta)
{
    INFO_CLP info = SUCCESS;
    double *Z = NULL;
    CLP_INT nLP, nblkSDP;
    nLP = d->nLP;
    nblkSDP = d->nblkSDP;
    CLP_INT N = rinfo->N;
    CLP_INT *ridx = rinfo->ridx;
    CLP_INT *iv = rinfo->iv;
    double alpha1 = 1.0;
    double beta1 = 1.0;
    if (nblkSDP > 0)
    {
        CLP_INT n0 = 0;
        for (size_t k=0; k<nblkSDP; ++k)
        {
            CLP_INT n1 = d->nvblkSDP[k];
            n0 = (n1 > n0) ? n1 : n0;
        }
        Z = (double*) CLP_MALLOC(sizeof(double)*n0*n0);
        CHECKNULL2(Z);
    }

    for (size_t k=rid; k<N; ++k)
    {
        for (size_t j=ridx[k]; j<ridx[k+1]; ++j)
        {
            CLP_INT idx = iv[j];
            if (idx < nLP) // LP 
            {
                if (dx_s->LP[idx] < 0.0)
                {
                    double a = -d->LP[idx]/dx_s->LP[idx];
                    alpha1 = fmin(alpha1, a);
                }
                if (ds_s->LP[idx] < 0.0)
                {
                    double b = -d->LP[idx]/ds_s->LP[idx];
                    beta1 = fmin(beta1, b);
                }
            }
            else // SDP
            {
                CLP_INT sidx = idx - nLP;
                CLP_INT n1 = d->nvblkSDP[sidx];
                CLP_INT n2 = n1 * n1;
                CLP_INT incx = 1;
// printvec(n2, dx_s->SDP[sidx]);
                F77_NAME(dcopy)(&n2, dx_s->SDP[sidx], &incx, Z, &incx);
                char side = 'L';
                char uplo = 'U';
                char transa = 'N';
                char diag = 'N';
                double a1 = 1.0;
// printvec(n2, Z);
                F77_CALL(dtrmm)(&side, &uplo, &transa, &diag, &n1, &n1, &a1,
                                d05inv->SDP[sidx], &n1, Z, &n1 FCONE FCONE FCONE FCONE);
// printvec(n2, Z);
                side = 'R';
                F77_CALL(dtrmm)(&side, &uplo, &transa, &diag, &n1, &n1, &a1,
                                d05inv->SDP[sidx], &n1, Z, &n1 FCONE FCONE FCONE FCONE);
// printvec(n2, Z);
                double lmd;
                CLP_INT m1 = 0;
                info = compute_minEig(n1, Z, &lmd, &m1);
//  CLP_PRINTF("cstempp: %d, %d\n", idx, info);
                CHECKINFO(info);
                // if (m1 != 1)
                // {
                //     info = ERROR_EIG;
                //     goto EXCEPTION;
                // }

                if (lmd < 0.0)
                {
                    double a = - 1.0 / lmd;
                    alpha1 = fmin(alpha1, a);
                }

                F77_NAME(dcopy)(&n2, ds_s->SDP[sidx], &incx, Z, &incx);
                side = 'L';
                F77_CALL(dtrmm)(&side, &uplo, &transa, &diag, &n1, &n1, &a1,
                                d05inv->SDP[sidx], &n1, Z, &n1 FCONE FCONE FCONE FCONE);
                side = 'R';
                F77_CALL(dtrmm)(&side, &uplo, &transa, &diag, &n1, &n1, &a1,
                                d05inv->SDP[sidx], &n1, Z, &n1 FCONE FCONE FCONE FCONE);
                m1 = 0;
                info = compute_minEig(n1, Z, &lmd, &m1);
// CLP_PRINTF("cstempd: %d, %d\n", idx, info);
                CHECKINFO(info);
                if (m1 != 1)
                {
                    info = ERROR_EIG;
                    goto EXCEPTION;
                }
                if (lmd < 0.0)
                {
                    double b = - 1.0 / lmd;
                    beta1 = fmin(beta1, b);
                }
            }
        }
    }
    *alpha = alpha1;
    *beta = beta1;

EXCEPTION:
    CLP_FREE(Z);
    return info;
}

CLP_INT directinNTMHPC(const CLPinfo *clpinfo, const dataCLP *data, 
    const regionInfo *rinfo, const vecCLP *x, const vecCLP *s, const double *y,
    vecCLP *dx, vecCLP *ds, double *dy, double *alpha, double *beta)
{
    INFO_CLP info = SUCCESS;
    CLP_INT N, m, n2, nLP, nblkSDP;
    vecCLP *Rd=NULL, *Rc=NULL, *c_s=NULL, *dx_s=NULL, *ds_s=NULL;
    vecCLP *Asty=NULL, *cor=NULL;
    vecCLP *d=NULL, *dinv=NULL, *d05inv=NULL, *g=NULL, *ginv=NULL;
    coeffCLP *nu=NULL;
    ACLP *As=NULL;
    double *Rp=NULL, *Rd_cv=NULL, *Rc_cv=NULL, *As_cv=NULL, *tau=NULL;
    double *dx_cv=NULL, *ds_cv=NULL;
    
    N = rinfo->N;
    m = clpinfo->m;
    nLP = clpinfo->nLP;
    nblkSDP = clpinfo->nblkSDP;
    n2 = nLP;
    if (nblkSDP > 0)
    {
        for (size_t k=0; k<nblkSDP; ++k)
        {
            CLP_INT n1 = clpinfo->nvblkSDP[k];
            n2 += n1 * (n1+1) / 2;
        }
    }

    CLP_INT dg = iprod(x, s);
    CLP_INT rid = find_region(rinfo, dg);

    d = create_vecCLP(clpinfo);
    dinv = create_vecCLP(clpinfo);
    d05inv = create_vecCLP(clpinfo);
    g = create_vecCLP(clpinfo);
    ginv = create_vecCLP(clpinfo);
    CHECKNULL2(d);
    CHECKNULL2(dinv);
    CHECKNULL2(d05inv);
    CHECKNULL2(g);
    CHECKNULL2(ginv);

    info = scalingOpNTCLP(x, s, d, dinv, d05inv, g, ginv);
    CHECKINFO(info);

    c_s = create_vecCLP(clpinfo);
    CHECKNULL2(c_s);
    info = scaleDualvecCLP(data->c, g, c_s);
    CHECKINFO(info);

    As = create_ACLP(clpinfo);
    CHECKNULL2(As);
    info = scaleACLP(clpinfo, data->A, g, As);
    CHECKINFO(info);

    Asty = create_vecCLP(clpinfo);
    CHECKNULL2(Asty);

    compute_Aty(As, y, Asty);

    Rp = (double*) CLP_MALLOC(sizeof(double)*m);
    CHECKNULL2(Rp);
    compute_Rp(data->b, data->A, x, Rp);

    Rd = create_vecCLP(clpinfo);
    CHECKNULL2(Rd);
    compute_Rd(clpinfo, Asty, c_s, d, Rd);

    nu = create_coeffCLP(clpinfo);
    CHECKNULL2(nu);
    if (NOTNULLP(data->nu))
    {
        copy_coeffCLP(data->nu, nu);
    }

    // In case the current iterate is not in the last region, 
    // it heads for the entrance of the next region.
    // In the last region, it heads for an optimal region.
    if (rid < (N-1))
    {
        double nu0 = rinfo->unique_coeffs[rid] - 0.0025;
        compute_nucoeffCLP(clpinfo, rinfo, nu0, dg, nu);
    }
    Rc = create_vecCLP(clpinfo);
    CHECKNULL2(Rc);
    compute_Rc(clpinfo, nu, dinv, d, cor, Rc);
    
    dx_cv = create_dvec(n2);
    ds_cv = create_dvec(n2);
    Rd_cv = create_dvec(n2);
    Rc_cv = create_dvec(n2);
    As_cv = create_dvec(m*n2);
    tau = create_dvec(m);
    CHECKNULL2(dx_cv);
    CHECKNULL2(ds_cv);
    CHECKNULL2(Rd_cv);
    CHECKNULL2(Rc_cv);
    CHECKNULL2(As_cv);
    CHECKNULL2(tau);

    cvec_vecCLP(Rd, Rd_cv);
    cvec_vecCLP(Rc, Rc_cv);
    cmatACLP(As, As_cv);

    info = compute_qr(n2, m, As_cv, tau);
    CHECKINFO(info);
    char trans = 'T';
    solve_Rx(trans, n2, m, As_cv, Rp);
    solve_normalEquation(n2, m, As_cv, tau, Rp, Rd_cv, Rc_cv, dx_cv, ds_cv, dy);
    dx_s = create_vecCLP(clpinfo);
    ds_s = create_vecCLP(clpinfo);
    CHECKNULL2(dx_s);
    CHECKNULL2(ds_s);
    create_vecCLPfromcvec(dx_cv, dx_s);
    create_vecCLPfromcvec(ds_cv, ds_s);

    // In case not in the last region, compute stepsize and scale back direction
    // vectors, and return.
    if (rid < (N-1))
    {
        info = compute_stepsize(rinfo, rid, d, d05inv, dx_s, ds_s, alpha, beta);
        CHECKINFO(info);
        info = scalebackPrimalvecCLP(dx_s, g, dx);
        CHECKINFO(info);
        info = scalebackDualvecCLP(ds_s, ginv, ds);
        CHECKINFO(info);
        goto PROLOGUE;
    }

    // From here, we deal with the case in the last region.
    // We adapt for a variant of Mehotra PC algorithm.

    cor = create_vecCLP(clpinfo);
    CHECKNULL2(cor);
    info = compute_cor(clpinfo, dx_s, ds_s, dinv, cor);
    CHECKINFO(info);
    
    double alpha1, beta1;
    info = compute_stepsize(rinfo, rid, d, d05inv, dx_s, ds_s, &alpha1, &beta1);
    CHECKINFO(info);

    double mu0 = extendedNormalizedDualityGap(rinfo, d, d);
    update_vecCLP2(d, alpha1, dx_s);
    update_vecCLP2(d, beta1, ds_s);
    double mu1 = extendedNormalizedDualityGap(rinfo, dx_s, ds_s);
    
    double expon = 1.0;
    if (mu0 > 1.0e-6)
    {
        expon = fmax(1.0, 3.0 * pow(fmin(alpha1, beta1), 2.0));
    }
    double sig = fmin(1.0, pow(mu1 / mu0, expon));

    double nu0 = sig * mu0;
    compute_nucoeffCLP(clpinfo, rinfo, nu0, dg, nu);
    compute_Rc(clpinfo, nu, dinv, d, cor, Rc);
    cvec_vecCLP(Rc, Rc_cv);

    solve_normalEquation(n2, m, As_cv, tau, Rp, Rd_cv, Rc_cv, dx_cv, ds_cv, dy);
    create_vecCLPfromcvec(dx_cv, dx_s);
    create_vecCLPfromcvec(ds_cv, ds_s);

    info = compute_stepsize(rinfo, rid, d, d05inv, dx_s, ds_s, alpha, beta);
    CHECKINFO(info);
    scalebackPrimalvecCLP(dx_s, g, dx);
    scalebackDualvecCLP(ds_s, ginv, ds);

PROLOGUE:
EXCEPTION:
    delete_vecCLP(Rd);
    delete_vecCLP(Rc);
    delete_vecCLP(c_s);
    delete_vecCLP(Asty);
    delete_vecCLP(dx_s);
    delete_vecCLP(ds_s);
    delete_vecCLP(cor);
    delete_vecCLP(d);
    delete_vecCLP(dinv);
    delete_vecCLP(d05inv);
    delete_vecCLP(g);
    delete_vecCLP(ginv);
    delete_coeffCLP(nu);
    delete_ACLP(As);
    CLP_FREE(Rp);
    CLP_FREE(As_cv);
    CLP_FREE(Rd_cv);
    CLP_FREE(Rc_cv);
    CLP_FREE(dx_cv);
    CLP_FREE(ds_cv);
    return info;
}



double normCLP(const vecCLP *x)
{
    double z = 0.0;
    CLP_INT nLP, nblkSDP;
    nLP = x->nLP;
    nblkSDP = x->nblkSDP;

    if (nLP > 0)
    {
        CLP_INT incx = 1;
        z += F77_NAME(dnrm2)(&nLP, x->LP, &incx);
    }
    if (nblkSDP > 0)
    {
        for (size_t k=0; k<nblkSDP; ++k)
        {
            CLP_INT n1 = x->nvblkSDP[k];
            CLP_INT n2 = n1 * n1;
            CLP_INT incx = 1;
            z += F77_NAME(dnrm2)(&n2, x->SDP[k], &incx);
        }
    }

    return z;
}

void init_point(const dataCLP *data, const CLPinfo *clpinfo, 
    const regionInfo *rinfo,  const double nu0, 
    vecCLP *x, vecCLP *s, double *y)
{
    vecCLP *c;
    double nu1, alpha;
    double *ALP, *b;
    CLP_INT m, nLP, nblkSDP;
    m = clpinfo->m;
    nLP = clpinfo->nLP;
    nblkSDP = clpinfo->nblkSDP;
    b = data->b;
    c = data->c;

    for (size_t j=0; j<m; ++j)
    {
        y[j] = 0.0;
    }
    
    if (nLP > 0)
    {
        ALP = data->A->LP;
        double xi_LP, eta_LP;
        double txi = 0.0, teta = 0.0;
        CLP_INT incx = 1;
        for (size_t j=0; j<m; ++j)
        {
            double Acnorm = F77_NAME(dnrm2)(&nLP, &(ALP[j*nLP]), &incx);
            txi = fmax(txi, (1.0+fabs(b[j])/(1.0+Acnorm)));
            teta = fmax(teta, Acnorm);
        }
        xi_LP = fmax(10.0, fmax(sqrt((double)nLP), sqrt((double)nLP)*txi));
        double cnorm = F77_NAME(dnrm2)(&nLP, c->LP, &incx);
        eta_LP = fmax(10.0, fmax(sqrt((double)nLP), fmax(cnorm, teta)));

        for (size_t i=0; i<nLP; ++i)
        {
            x->LP[i] = xi_LP;
            s->LP[i] = eta_LP;
        }
    }
    if (nblkSDP > 0)
    {
        for (size_t k=0; k<nblkSDP; ++k)
        {
            double txi = 0.0, teta = 0.0;
            double *S = data->A->SDP[k];
            CLP_INT n1 = clpinfo->nvblkSDP[k];
            CLP_INT n2 = n1*n1;
            CLP_INT incx = 1;

            for (size_t j=0; j<m; ++j)
            {
                double Asnorm = F77_NAME(dnrm2)(&n2, &(S[j*n2]), &incx);
                txi = fmax(txi, (1.0+fabs(b[j])/(1.0+Asnorm)));
                teta = fmax(teta, Asnorm);
            }

            double *xS = x->SDP[k];
            double *sS = s->SDP[k];
            // double alpha = 0.0;
            // F77_NAME(dscal)(&n2, &alpha, xS, &incx);
            // F77_NAME(dscal)(&n2, &alpha, sS, &incx);
            zerofill_mat('A', n1, n1, xS, n1);
            zerofill_mat('A', n1, n1, sS, n1);
            for (size_t j=0; j<n1; ++j)
            {
                xS[j+j*n1] = txi;
                sS[j+j*n1] = teta;
            }
        }
    }
    nu1 = iprod(x,s);
    if (nu1 <= 2 * nu0)
    {
        alpha = 4*sqrt(nu0/nu1);
        mulscalar_vecCLP(alpha, x);
        mulscalar_vecCLP(alpha, s);
    }
    return;
}


CLP_INT feasibility(const dataCLP *data, const regionInfo *rinfo, 
    const vecCLP *x, const vecCLP *s, const double *y,
    double *gap, double *relgap, double *pobj, double *dobj,
    double *pinfeas, double *dinfeas)
{
    INFO_CLP info = SUCCESS;
    CLPinfo *clpinfo = data->clpinfo;
    coeffCLP *nu=NULL;
    double nrmRp, nrmRd, nrmb, nrmc;
    double *Rp=NULL, *TZ=NULL;
    vecCLP *Rd=NULL, *Aty=NULL;
    CLP_INT m, nLP, nblkSDP;
    m = clpinfo->m;
    nLP = clpinfo->nLP;
    nblkSDP = clpinfo->nblkSDP;

    Rp = (double*) CLP_MALLOC(sizeof(double)*m);
    CHECKNULL2(Rp);
    compute_Rp(data->b, data->A, x, Rp);
    CLP_INT incx = 1;
    nrmRp = F77_NAME(dnrm2)(&m, Rp, &incx);
    nrmb = F77_NAME(dnrm2)(&m, data->b, &incx);
    *pinfeas = nrmRp / (1.0 + nrmb);

    Rd = create_vecCLP(clpinfo);
    Aty = create_vecCLP(clpinfo);
    CHECKNULL2(Rd);
    CHECKNULL2(Aty);
    compute_Aty(data->A, y, Aty);
    compute_Rd(clpinfo, Aty, data->c, s, Rd);
    nrmRd = normCLP(Rd);
    nrmc = normCLP(data->c);
    *dinfeas = nrmRd / (1.0 + nrmc);

    double gap0, plogdet, dlogdet;
    gap0 = iprod(x, s);
    plogdet = 0.0;
    dlogdet = 0.0;
    if (clpinfo->logdet_p)
    {
        nu = data->nu;
        if (nLP > 0)
        {  
            for (size_t k=0; k<nLP; ++k)
            {
                double nu0 = nu->LP[k];
                if ( nu0 > 0.0)
                {
                    gap0 -= nu0 * (1.0 + log(x->LP[k] * s->LP[k] / nu0));
                    plogdet -= nu0 * log(x->LP[k]);
                    dlogdet += (nu0 * log(s->LP[k]) + nu0 * (1.0 - log(nu0)));
                }
            }
        }
        if (nblkSDP > 0)
        {
            for (size_t k=0; k<nblkSDP; ++k)
            {
                double nu0 = nu->vSDP[k];
                if (nu0 > 0.0)
                {
                    CLP_INT n1 = clpinfo->nvblkSDP[k];
                    CLP_INT n2 = n1*n1;
                    TZ = (double*) CLP_MALLOC(sizeof(double)*n2);
                    CHECKNULL2(TZ);
                    char transa = 'N';
                    char transb = 'N';
                    double alpha = 1.0 / nu0;
                    double beta = 0.0;
                    double val;
                    F77_NAME(dgemm)(&transa, &transb, &n1, &n1, &n1, &alpha,
                        x->SDP[k], &n1, s->SDP[k], &n1, &beta, TZ, &n1 FCONE FCONE);
                    info = det(n1, TZ, &val);
                    CLP_FREE(TZ);
                    CHECKINFO(info);
                    gap0 -= nu0 * ((double)n1 + log(val));

                    TZ = (double*) CLP_MALLOC(sizeof(double)*n2);
                    CHECKNULL2(TZ);
                    memcpy(TZ, x->SDP[k], sizeof(double)*n2);
                    info = det(n1, TZ, &val);
                    CLP_FREE(TZ);
                    CHECKINFO(info);
                    plogdet -= nu0 * log(val);
                    
                    TZ = (double*) CLP_MALLOC(sizeof(double)*n2);
                    CHECKNULL2(TZ);
                    memcpy(TZ, s->SDP[k], sizeof(double)*n2);
                    info = det(n1, TZ, &val);
                    CLP_FREE(TZ);
                    CHECKINFO(info);
                    dlogdet += (nu0 * log(val) + nu0 * (double)n1 *(1.0 - log(nu0)));

                }
            }
        }
    }

    double cx, by;
    cx = iprod(data->c, x);
    by = F77_NAME(ddot)(&m, data->b, &incx, y, &incx);

    *gap = gap0;
    *relgap = gap0 / (1.0 + fabs(cx) + fabs(by));
    *pobj = cx + plogdet;
    *dobj = by + dlogdet;

EXCEPTION:
    CLP_FREE(Rp);
    CLP_FREE(Aty);
    CLP_FREE(Rd);
    return info;
}


CLP_INT solver(const dataCLP *data, const OPTIONS *options, RESULTS *results)
// CLP_INT solver(const dataCLP *data)
{
    INFO_CLP info = SUCCESS;
    CLPinfo *clpinfo = data->clpinfo;
    regionInfo *rinfo=NULL;
    vecCLP *x=NULL, *s=NULL;
    double *y=NULL, nu0;
    vecCLP *dx=NULL, *ds=NULL;
    double *dy=NULL;
    CLP_INT m, regionN, last0region; //, last1region;
    itrCLP *snapshot=NULL;
    
    m = clpinfo->m;
    // nLP = clpinfo->nLP;
    // nblkSDP = clpinfo->nblkSDP;
    CLP_INT incy = 1;

    double tol = 1e-7;
    CLP_INT NITER = 20;
    bool verbose = true;
    if (NOTNULLP(options))
    {
        tol = options->gaptol;
        NITER = options->NITER;
        verbose = options->verbose;
    }
    rinfo = create_regionInfo(clpinfo, data->nu);
    CHECKNULL2(rinfo);
    regionN = rinfo->N;
    last0region = regionN-1;
    // last1region = last0region-1;
    nu0 = rinfo->nuv[0];

    x = create_vecCLP(clpinfo);
    s = create_vecCLP(clpinfo);
    y = create_dvec(m);
    CHECKNULL2(x);
    CHECKNULL2(s);
    CHECKNULL2(y);

    if (NOTNULLP(options) && NOTNULLP(options->x0))
    {
        copy_vecCLP(options->x0, x);
        copy_vecCLP(options->s0, s);
        F77_NAME(dcopy)(&m, options->y0, &incy, y, &incy);
    }
    else
    {
        init_point(data, clpinfo, rinfo, nu0, x, s, y);
    }

    dx = create_vecCLP(clpinfo);
    ds = create_vecCLP(clpinfo);
    dy = create_dvec(m);
    CHECKNULL2(dx);
    CHECKNULL2(ds);
    CHECKNULL2(dy);

    CLP_INT NSTEP = 1;
    CLP_INT ITR0 = 0;
    double stepdefault[] = {0.6};
    double *stepratio = stepdefault;
    double alpha, beta;
    if (NOTNULLP(options) && options->NSTEP != 0)
    {
        NSTEP = options->NSTEP;
        stepratio = options->stepvec;
    }

    double snapshotmu1, snapshotcriterion1;
    snapshot = create_itrCLP(clpinfo);
    CHECKNULL2(snapshot);

    double gap=0.0, relgap=0.0, pobj=0.0, dobj=0.0, pinfeas=0.0, dinfeas=0.0;
    double tol1=0.0, feas1=0.0, dg0=0.0, dg1=0.0, mu0=0.0, mu1=0.0, criterion0=0.0, criterion1=0.0;
    CLP_INT region0, region1;
    dg0 = iprod(x, s);
    region0 = find_region(rinfo, dg0);
    region1 = region0;

    mu0 = extendedNormalizedDualityGap(rinfo, x, s);
    info = feasibility(data, rinfo, x, s, y, &gap, &relgap, &pobj, &dobj,
                &pinfeas, &dinfeas);
    CHECKINFO(info);
    // feas0 = fmax(pinfeas, dinfeas);
    // tol0 = fmax(relgap, feas0);
    criterion0 = 0.5*relgap + 0.25*(pinfeas + dinfeas);

    if (verbose)
    {
        CLP_PRINTF("CLP SOLVER 2022 (C)\n");
        CLP_PRINTF("Regions: %d, Constraints: %d\n", regionN, m);
        CLP_PRINTF("LP dims: %d\n", clpinfo->nLP);
        CLP_PRINTF("SDP blocks: %d,", clpinfo->nblkSDP);
        if (clpinfo->nblkSDP > 0)
        {
            CLP_PRINTF(" dims: [");
            for (size_t k=0; k<clpinfo->nblkSDP; ++k)
            {
                CLP_PRINTF("%d,", clpinfo->nvblkSDP[k]);
            }
            CLP_PRINTF("].\n");
        }
        CLP_PRINTF("Itr|Rg|  rPD gap |  pinfeas |  dinfeas |   Primal Obj  |    Dual Obj   |alpha| beta|\n");
        CLP_PRINTF("---+--+----------+----------+----------+---------------+---------------+-----+-----+\n");
        CLP_PRINTF("%3d|%2d|%+7.3e|%+7.3e|%+7.3e|%+.8e|%+.8e|%.3f|%.3f|\n", 0, region0, relgap, pinfeas, dinfeas, pobj, dobj, 0.0, 0.0);
    }
    for (size_t k=0; k<NITER; ++k)
    {
        info = directinNTMHPC(clpinfo, data, rinfo, x, s, y, dx, ds, dy,
                     &alpha, &beta);
// CLP_PRINTF("direction: %d\n", info);
        CHECKINFO(info);

        alpha = alpha * stepratio[ITR0];
        beta = beta * stepratio[ITR0];

        update_vecCLP(alpha, dx, x);
        update_vecCLP(beta, ds, s);
        F77_NAME(daxpy)(&m, &beta, dy, &incy, y, &incy);
        // print_vecCLP(x);
        // print_vecCLP(s);

        mu1 = extendedNormalizedDualityGap(rinfo, x, s);
        dg1 = iprod(x, s);
        region1 = find_region(rinfo, dg1);
        info = feasibility(data, rinfo, x, s, y, &gap, &relgap, 
                    &pobj, &dobj, &pinfeas, &dinfeas);
// CLP_PRINTF("feas: %d\n", info);
        CHECKINFO(info);
        if (verbose)
        {
            CLP_PRINTF("%3d|%2d|%+7.3e|%+7.3e|%+7.3e|%+.8e|%+.8e|%.3f|%.3f|\n", (CLP_INT)k+1, region1, relgap, pinfeas, dinfeas, pobj, dobj, alpha, beta);
        }
        feas1 = fmax(pinfeas, dinfeas);
        tol1 = fmax(relgap, feas1);
        criterion1 = 0.5*relgap + 0.25*(pinfeas + dinfeas);

        if (NSTEP > 1 && regionN > 1 && region1 < last0region)
        {
            copy_itrCLP(clpinfo, x, s, y, snapshot);
            snapshotmu1 = mu1;
            snapshotcriterion1 = criterion1;
        }

        if (NOTNULLP(results))
        {
            if (relgap >= 0.0 && criterion1 <= 5e-5 && criterion1 < criterion0)
            {
                results->opt = true;
                results->relgap = relgap;
                results->pinfeas = pinfeas;
                results->dinfeas = dinfeas;
                copy_vecCLP(x, results->x);
                copy_vecCLP(s, results->s);
                F77_NAME(dcopy)(&m, y, &incy, results->y, &incy);
            }            
        }

        if (relgap < 0.0)
        {
            CLP_PRINTF("Status: Minus relgap\n");
            break;
        }
        else if (k > 10 && region1 == last0region && mu1 / mu0 > 0.9)
        {
            if (NSTEP > 1 && ITR0 < NSTEP-1)
            {
                CLP_PRINTF("Status: Stagnation1: Rollback\n");
                copyback_itrCLP(clpinfo, snapshot, x, s, y);
                mu1 = snapshotmu1;
                criterion1 = snapshotcriterion1;
                ITR0++;
            }
            else
            {
                CLP_PRINTF("Status: Stagnation\n");
                break;
            }
        }
        else if (tol1 < tol)
        {
            CLP_PRINTF("Status: Normal Termination.\n");
            break;
        }
        else if (k > 10 && region1 == last0region && criterion1 / criterion0 > 0.9)
        {
            if (NSTEP > 1 && ITR0 < NSTEP-1)
            {
                CLP_PRINTF("Status: Stagnation2: Rollback\n");
                copyback_itrCLP(clpinfo, snapshot, x, s, y);
                mu1 = snapshotmu1;
                criterion1 = snapshotcriterion1;
                ITR0++;
            }
            else
            {
                CLP_PRINTF("Status: Stagnation2\n");
                break;
            }
        }

        mu0 = mu1;
        dg0 = dg1;
        // tol0 = tol1;
        criterion0 = criterion1;
    }

EXCEPTION:
    delete_regionInfo(rinfo);
    delete_vecCLP(x);
    delete_vecCLP(s);
    CLP_FREE(y);
    delete_vecCLP(dx);
    delete_vecCLP(ds);
    CLP_FREE(dy);
    delete_itrCLP(snapshot);
    return info;
}
