#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <math.h>
#include "clp.h"
#include "clpmisc.h"


double* create_dvec(const CLP_INT n)
{
    double *z;
    z = (double*) CLP_MALLOC(sizeof(double)*n);
    if (NOTNULLP(z))
    {
        for (size_t i=0; i<n; ++i)
        {
            z[i] = 0.0;
        }
    }
    return z;
}

void delete_dvec(double *z)
{
    if (NOTNULLP(z))
    {
        CLP_FREE(z);
    }
}
/*
    Create and initialize dimensional information of CLP
*/
CLPinfo* create_CLPinfo(const CLP_INT m, const CLP_INT nLP, const CLP_INT nblkSDP,
    const CLP_INT *nvblkSDP, const bool logdet_p)
{
    CLPinfo *clpinfo = NULL;
    
    clpinfo = (CLPinfo *) CLP_MALLOC(sizeof(CLPinfo));
    CHECKNULL(clpinfo);

    clpinfo->m = m;
    clpinfo->nLP = nLP;
    clpinfo->nblkSDP = nblkSDP;
    clpinfo->nvblkSDP = NULL;
    clpinfo->n2vblkSDP = NULL;
    clpinfo->nc2vblkSDP = NULL;
    clpinfo->logdet_p = logdet_p;

    if (nblkSDP > 0)
    {
        CHECKNULL(nvblkSDP);
        clpinfo->nvblkSDP = (CLP_INT *) CLP_MALLOC(sizeof(CLP_INT)*nblkSDP);
        CHECKNULL(clpinfo->nvblkSDP);
        memcpy(clpinfo->nvblkSDP, nvblkSDP, sizeof(CLP_INT)*nblkSDP);

        clpinfo->n2vblkSDP = (CLP_INT*) CLP_MALLOC(sizeof(CLP_INT)*nblkSDP);
        clpinfo->nc2vblkSDP = (CLP_INT*) CLP_MALLOC(sizeof(CLP_INT)*nblkSDP);
        CHECKNULL(clpinfo->n2vblkSDP);
        CHECKNULL(clpinfo->nc2vblkSDP);

        CLP_INT n2SDP = 0;
        CLP_INT nc2SDP = 0;
        for (size_t k=0; k<nblkSDP; ++k)
        {
            CLP_INT n1 = nvblkSDP[k];
            CLP_INT n2 = n1 * n1;
            CLP_INT nc2 = n1 * (n1 + 1) / 2;
            n2SDP += n2;
            nc2SDP += nc2;
            clpinfo->n2vblkSDP[k] = n2;
            clpinfo->nc2vblkSDP[k] = nc2;
        }
        clpinfo->n2SDP = n2SDP;
        clpinfo->nc2SDP = nc2SDP;

    }

    return clpinfo;

EXCEPTION:
    delete_CLPinfo(clpinfo);
    return NULL;
}

/*
    Deallocate CLPinfo struct
*/
void delete_CLPinfo(CLPinfo *clpinfo)
{
    if (NULLP(clpinfo))
    {
        return;
    }

    CLP_FREE(clpinfo->nvblkSDP);
    CLP_FREE(clpinfo->n2vblkSDP);
    CLP_FREE(clpinfo->nc2vblkSDP);
    CLP_FREE(clpinfo);
}


/*
    Create varCLP struct from CLPinfo
    The memory for LP/SDP data, if designated in CLPinfo, is allocated 
    but not initialized
*/
vecCLP* create_vecCLP(const CLPinfo* clpinfo)
{
    vecCLP *z = NULL;
    CHECKNULL(clpinfo);

    CLP_INT nLP = clpinfo->nLP;
    CLP_INT nblkSDP = clpinfo->nblkSDP;

    z = (vecCLP*) CLP_MALLOC(sizeof(vecCLP));
    CHECKNULL(z);

    z->nLP = nLP;
    z->nblkSDP = nblkSDP;
    z->LP = NULL;
    z->nvblkSDP = NULL;
    z->SDP = NULL;

    if (nLP > 0)
    {
        z->LP = (double*) CLP_MALLOC(sizeof(double)*nLP);
        CHECKNULL(z->LP);
        zerofill_vec(nLP, z->LP);
    }
    if (nblkSDP > 0)
    {
        z->nvblkSDP = (CLP_INT *) CLP_MALLOC(sizeof(CLP_INT)*nblkSDP);
        CHECKNULL(z->nvblkSDP);
        memcpy(z->nvblkSDP, clpinfo->nvblkSDP, sizeof(CLP_INT)*nblkSDP);

        z->SDP = (double **) CLP_MALLOC(sizeof(double*)*nblkSDP);
        CHECKNULL(z->SDP);
        for (size_t i=0; i<nblkSDP; ++i)
        {
            z->SDP[i] = NULL;
        }
        for (size_t i=0; i<nblkSDP; ++i)
        {
            CLP_INT n1 = z->nvblkSDP[i];
            z->SDP[i] = (double*) CLP_MALLOC(sizeof(double)*n1*n1);
            CHECKNULL(z->SDP[i]);
            zerofill_mat('A', n1, n1, z->SDP[i], n1);
        }
    }
    return z;

EXCEPTION:
    delete_vecCLP(z);
    return NULL;
}

/*
    Create and initialize the varCLP struct from CLPinfo
    Simultaneously, LP/SDP data are initialized
*/
vecCLP* init_vecCLP(const CLPinfo *clpinfo, double *LP, double **SDP)
{
    vecCLP *z = NULL;
    CHECKNULL(clpinfo);
    z = create_vecCLP(clpinfo);
    CHECKNULL(z);

    CLP_INT nLP = clpinfo->nLP;
    CLP_INT nblkSDP = clpinfo->nblkSDP;
    if (nLP > 0)
    {
        CHECKNULL(LP);
        memcpy(z->LP, LP, sizeof(double)*nLP);
    }
    if (nblkSDP > 0)
    {
        CHECKNULL(SDP);
        for (size_t k=0; k<nblkSDP; ++k)
        {
            CHECKNULL(SDP[k]);
            CLP_INT n1 = clpinfo->nvblkSDP[k];
            memcpy(z->SDP[k], SDP[k], sizeof(double)*n1*n1);
        }
    }
    return z;

EXCEPTION:
    delete_vecCLP(z);
    return NULL;
}

/*
    Copy varCLP struct
*/
void copy_vecCLP(const vecCLP* x, vecCLP *z)
{
    CLP_INT nLP = x->nLP;
    CLP_INT nblkSDP = x->nblkSDP;

    if (nLP > 0)
    {
        memcpy(z->LP, x->LP, sizeof(double)*nLP);
    }
    if (nblkSDP > 0)
    {
        for (size_t k=0; k<nblkSDP; ++k)
        {
            CLP_INT n1 = x->nvblkSDP[k];
            CLP_INT n2 = n1*n1;
            memcpy(z->SDP[k], x->SDP[k], sizeof(double)*n2);
        }
    }
}

void mulscalar_vecCLP(const double alpha, vecCLP *x)
{
    CLP_INT nLP = x->nLP;
    CLP_INT nblkSDP = x->nblkSDP;

    if (nLP > 0)
    {
        for (size_t i=0; i<nLP; ++i)
        {
            x->LP[i] = alpha * x->LP[i];
        }
    }
    if (nblkSDP > 0)
    {
        for (size_t k=0; k<nblkSDP; ++k)
        {
            CLP_INT n1 = x->nvblkSDP[k];
            CLP_INT n2 = n1*n1;
            double *S = x->SDP[k];
            for (size_t j=0; j<n2; ++j)
            {
                S[j] = alpha * S[j];
            }
        }
    }
}

/*
    Convert varCLP to compact form of data array
*/
void cvec_vecCLP(const vecCLP *x, double* z)
{
    CLP_INT nLP = x->nLP;
    CLP_INT nblkSDP = x->nblkSDP;

    if (nLP > 0)
    {
        memcpy(z, x->LP, sizeof(double)*nLP);
    }
    if (nblkSDP > 0)
    {
        CLP_INT c = nLP;
        // double sqrt2 = sqrt(2.0);
        for (size_t k=0; k<nblkSDP; ++k)
        {
            CLP_INT n1 = x->nvblkSDP[k];
            double *S = x->SDP[k];
            for (size_t j=0; j<n1; ++j)
            {
                for (size_t i=0; i<j; ++i)
                {
                    z[c] = sqrt2 * S[i+j*n1];
                    ++c;
                }
                z[c] = S[j+j*n1];
                ++c;
            }
        }
    }
}

/*
    Create varCLP from compact vector form array cvec
*/
void create_vecCLPfromcvec(const double *cvec, vecCLP *z)
{
    CLP_INT nLP, nblkSDP;
    nLP = z->nLP;
    nblkSDP = z->nblkSDP;

    if (nLP > 0)
    {
        memcpy(z->LP, cvec, sizeof(double)*nLP);
    }
    if (nblkSDP > 0)
    {
        // double sqrt05 = 1.0 / sqrt(2.0);
        CLP_INT c = nLP;
        for (size_t k=0; k<nblkSDP; ++k)
        {
            CLP_INT n1 = z->nvblkSDP[k];
            double *S = z->SDP[k];
            for (size_t j=0; j<n1; ++j)
            {
                for (size_t i=0; i<j; ++i)
                {
                    S[i+j*n1] = sqrt05*cvec[c];
                    S[j+i*n1] = sqrt05*cvec[c];
                    ++c;
                }
                S[j+j*n1] = cvec[c];
                ++c;
            }
        }
    }
}

/*
    Deallocate memeory of varCLP
*/
void delete_vecCLP(vecCLP *z)
{
    if (NULLP(z))
    {
        return;
    }

    CLP_FREE(z->LP);
    CLP_FREE(z->nvblkSDP);
    if (NOTNULLP(z->SDP))
    {
        for (size_t i=0; i<z->nblkSDP; ++i)
        {
            CLP_FREE(z->SDP[i]);
        }
        CLP_FREE(z->SDP);
    }
    CLP_FREE(z);
}

void print_vecCLP(const vecCLP *z)
{
    CLP_INT nLP, nblkSDP;
    nLP = z->nLP;
    nblkSDP = z->nblkSDP;
    if (nLP > 0)
    {
        printvec(nLP, z->LP);
    }
    if (nblkSDP > 0)
    {
        for (size_t k=0; k<nblkSDP; ++k)
        {
            CLP_INT n1 = z->nvblkSDP[k];
            printmat(n1, n1, z->SDP[k]);
        }
    }
}

coeffCLP* create_coeffCLP(const CLPinfo *clpinfo)
{
    coeffCLP *c = NULL;
    CHECKNULL(clpinfo);
    CLP_INT nLP, nblkSDP;

    c = (coeffCLP*) CLP_MALLOC(sizeof(coeffCLP));
    CHECKNULL(c);
    
    nLP = clpinfo->nLP;
    nblkSDP = clpinfo->nblkSDP;
    c->nLP = nLP;
    c->nblkSDP = nblkSDP;
    c->LP = NULL;
    c->vSDP = NULL;

    if (nLP > 0)
    {
        c->LP = (double*) CLP_MALLOC(sizeof(double) * nLP);
        CHECKNULL(c->LP);
        zerofill_vec(nLP, c->LP);
    }
    if (nblkSDP > 0)
    {
        c->vSDP = (double*) CLP_MALLOC(sizeof(double) * nblkSDP);
        CHECKNULL(c->vSDP);
        zerofill_vec(nblkSDP, c->vSDP);
    }
    return c;

EXCEPTION:
    delete_coeffCLP(c);
    return NULL;
} 

coeffCLP* init_coeffCLP(const CLPinfo *clpinfo, double *LP, double *vSDP)
{
    coeffCLP *z = NULL;
    CHECKNULL(clpinfo);
    z = create_coeffCLP(clpinfo);
    CHECKNULL(z);

    CLP_INT nLP = clpinfo->nLP;
    CLP_INT nblkSDP = clpinfo->nblkSDP;
    if (nLP > 0)
    {
        CHECKNULL(LP);
        memcpy(z->LP, LP, sizeof(double)*nLP);
    }
    if (nblkSDP > 0)
    {
        CHECKNULL(vSDP);
        memcpy(z->vSDP, vSDP, sizeof(double)*nblkSDP);
    }
    return z;

EXCEPTION:
    delete_coeffCLP(z);
    return NULL;
}

void copy_coeffCLP(const coeffCLP *coeff, coeffCLP *c)
{
    CLP_INT nLP, nblkSDP;  
    nLP = coeff->nLP;
    nblkSDP = coeff->nblkSDP;

    if (nLP > 0)
    {
        memcpy(c->LP, coeff->LP, sizeof(double) * nLP);
    }
    if (nblkSDP > 0)
    {
        memcpy(c->vSDP, coeff->vSDP, sizeof(double) * nblkSDP);
    }
} 

/*
    Return vector form of coeffCLP
*/
double* vec_coeffCLP(const coeffCLP *c)
{
    double *z = NULL;
    CHECKNULL(c);

    CLP_INT nLP, nblkSDP, N;
    nLP = c->nLP;
    nblkSDP = c->nblkSDP;

    N = nLP + nblkSDP;
    z = (double*) CLP_MALLOC(sizeof(double)*N);
    CHECKNULL(z);
    if (nLP > 0)
    {
        memcpy(z, c->LP, sizeof(double)*nLP);
    }
    if (nblkSDP > 0)
    {
        memcpy(&z[nLP], c->vSDP, sizeof(double)*nblkSDP);
    }
    return z;

EXCEPTION:
    CLP_FREE(z);
    return NULL;
}

void delete_coeffCLP(coeffCLP *c)
{
    if (NULLP(c))
    {
        return;
    }

    CLP_FREE(c->LP);
    CLP_FREE(c->vSDP);
    CLP_FREE(c);
    return;
}

itrCLP* create_itrCLP(const CLPinfo *clpinfo)
{
    itrCLP *z=NULL;

    z = (itrCLP*) CLP_MALLOC(sizeof(itrCLP));
    CHECKNULL(z);
    z->x = create_vecCLP(clpinfo);
    z->s = create_vecCLP(clpinfo);
    z->y = create_dvec(clpinfo->m);
    CHECKNULL(z->x);
    CHECKNULL(z->s);
    CHECKNULL(z->y);

    return z;

EXCEPTION:
    delete_itrCLP(z);
    return NULL;
}

void copy_itrCLP(CLPinfo *clpinfo, const vecCLP *x, const vecCLP *s, 
    const double *y, itrCLP *z)
{
    copy_vecCLP(x, z->x);
    copy_vecCLP(s, z->s);
    memcpy(z->y, y, sizeof(double)*clpinfo->m);
}

void copyback_itrCLP(const CLPinfo *clpinfo, const itrCLP *z, vecCLP *x,
    vecCLP *s, double *y)
{
    copy_vecCLP(z->x, x);
    copy_vecCLP(z->s, s);
    memcpy(y, z->y, sizeof(double)*clpinfo->m);
}

void delete_itrCLP(itrCLP *z)
{
    if (NULLP(z))
    {
        return;
    }

    delete_vecCLP(z->x);
    delete_vecCLP(z->s);
    delete_dvec(z->y);
    CLP_FREE(z);
}

/*
    Create ACLP object from clpinfo, and allocate LP and SDP data memory
*/
ACLP* create_ACLP(const CLPinfo *clpinfo)
{
    ACLP *A = NULL;
    CHECKNULL(clpinfo);

    CLP_INT m, nLP, nblkSDP;

    A = (ACLP*) CLP_MALLOC(sizeof(ACLP));
    CHECKNULL(A);
    
    m = clpinfo->m;
    nLP = clpinfo->nLP;
    nblkSDP = clpinfo->nblkSDP;

    A->m = m;
    A->nLP = nLP;
    A->nblkSDP = nblkSDP;
    A->nvblkSDP = NULL;
    A->LP = NULL;
    A->SDP =NULL;

    if (nLP > 0)
    {
        A->LP = (double*) CLP_MALLOC(sizeof(double)*m*nLP);
        CHECKNULL(A->LP);
        zerofill_mat('A', nLP, m, A->LP, nLP);
    }
    
    if (nblkSDP > 0)
    {
        A->nvblkSDP = (CLP_INT*) CLP_MALLOC(sizeof(CLP_INT)*nblkSDP);
        CHECKNULL(A->nvblkSDP);
        memcpy(A->nvblkSDP, clpinfo->nvblkSDP, sizeof(CLP_INT)*nblkSDP);

        A->SDP = (double**) CLP_MALLOC(sizeof(double*) * nblkSDP);
        CHECKNULL(A->SDP);
        for (size_t i=0; i<nblkSDP; ++i)
        {
            A->SDP[i] = NULL;
        }
        for (size_t i=0; i<nblkSDP; ++i)
        {   
            CLP_INT n1 = clpinfo->nvblkSDP[i];
            A->SDP[i] = (double*) CLP_MALLOC(sizeof(double)*n1*n1*m);
            CHECKNULL(A->SDP[i]);
            zerofill_mat('A', n1*n1, m, A->SDP[i], n1*n1);
        }
    }
    return A;

EXCEPTION:
    delete_ACLP(A);
    return NULL;
}

/*
    Create and initialize ACLP with data LP and SDP
*/
ACLP* init_ACLP(const CLPinfo *infoclp, const double *LP, const double **SDP)
{
    ACLP *A = NULL;
    A = create_ACLP(infoclp);
    CHECKNULL(A);

    CLP_INT m = infoclp->m;
    CLP_INT nLP = infoclp->nLP;
    CLP_INT nblkSDP = infoclp->nblkSDP;
    if (nLP > 0)
    {
        memcpy(A->LP, LP, sizeof(double)*nLP*m);
    }
    if (nblkSDP > 0)
    {
        for (size_t i=0; i<nblkSDP; ++i)
        {
            CLP_INT n1 = A->nvblkSDP[i];
            memcpy(A->SDP[i], SDP[i], sizeof(double)*n1*n1*m);
        }
    }
    return A;

EXCEPTION:
    return NULL;
}

/*
    Convert compact matrix form of ACLP
*/
void cmatACLP(const ACLP *A, double* Z)
{
    CLP_INT m = A->m;
    CLP_INT nLP = A->nLP;
    CLP_INT nblkSDP = A->nblkSDP;
    CLP_INT ncSDP = 0;
    if (nblkSDP > 0)
    {
        for (size_t i=0; i<nblkSDP; ++i)
        {
            CLP_INT n1 = A->nvblkSDP[i];
            ncSDP += n1*(n1+1)/2;
        }
    }
    CLP_INT lda = nLP + ncSDP;

    if (nLP > 0)
    {
        for (size_t j=0; j<m; ++j)
        {
            memcpy(&Z[j*lda], &(A->LP[j*nLP]), sizeof(double)*nLP);
        }
    }
    if (nblkSDP > 0)
    {
        // double sqrt2 = sqrt(2.0);
        for (size_t jA=0; jA<m; ++jA)
        {
            CLP_INT c = nLP;
            for (size_t k=0; k<nblkSDP; ++k)
            {
                double *S = A->SDP[k];
                CLP_INT n1 = A->nvblkSDP[k];
                CLP_INT n2 = n1 * n1;
                for (size_t j=0; j<n1; ++j)
                {
                    for (size_t i=0; i<j; ++i)
                    {
                        Z[c+jA*lda] = sqrt2 * S[i+j*n1+jA*n2];
                        ++c;
                    }
                    Z[c+jA*lda] = S[j+j*n1+jA*n2];
                    ++c;
                }
            }
        }
    }
}

void delete_ACLP(ACLP *A)
{
    if (NULLP(A))
    {
        return;
    }

    CLP_FREE(A->LP);
    CLP_FREE(A->nvblkSDP);
    if (NOTNULLP(A->SDP))
    {
        for (size_t i=0; i<A->nblkSDP; ++i)
        {
            CLP_FREE(A->SDP[i]);
        }
        CLP_FREE(A->SDP);
    }
    CLP_FREE(A);
}


typedef struct elm
{
    double nu;
    CLP_INT idx;
} elm;

int key_cmp2(const void *e1, const void *e2)
{
    double v1 = ((elm*)e1)->nu;
    double v2 = ((elm*)e2)->nu;
    CLP_INT idx1 = ((elm*)e1)->idx;
    CLP_INT idx2 = ((elm*)e2)->idx;

    if (v1 < v2)
    {
        return 1;
    }
    else if (v1 > v2)
    {
        return -1;
    }

    if (idx1 < idx2)
    {
        return -1;
    }
    else if ( idx1 > idx2)
    {
        return 1;
    }
    else
    {
        return 0;
    }
}

CLP_INT sort_coeffs2(const CLP_INT n, double *coeffs, CLP_INT *iv, CLP_INT *N)
{
    INFO_CLP info = SUCCESS;
    elm *ev = NULL;

    ev = (elm*) CLP_MALLOC(sizeof(elm) * n);
    CHECKNULL2(ev);

    for (size_t i=0; i<n; ++i)
    {
        ev[i].nu = coeffs[i];
        ev[i].idx = iv[i];
    }
    qsort((void*)ev, n, sizeof(elm), key_cmp2);
    
    CLP_INT c = 0;
    double x=INFINITY;
    for (size_t i=0; i<n; ++i)
    {
        coeffs[i] = ev[i].nu;
        iv[i] = ev[i].idx;
        if (x > coeffs[i])
        {
            // ridx[c] = i;
            x = coeffs[i];
            ++c;
        }
    }
    // ridx[c] = n;
    *N = c;

EXCEPTION:
    CLP_FREE(ev);
    return info;
}

void compute_degs(const CLPinfo *clpinfo, const CLP_INT n, const CLP_INT N,
    const double *coeffs, const CLP_INT *iv, CLP_INT *ridx, CLP_INT *degNv, 
    double *unique_coeffs, double *nuv)
{
    CLP_INT nLP = clpinfo->nLP;

    double x = INFINITY;
    CLP_INT c = 0;
    for (size_t i=0; i<n; ++i)
    {
        if (x > coeffs[i])
        {
            ridx[c] = i;
            x = coeffs[i];
            ++c;
        }
    }
    ridx[c] = n;

    for (size_t i=0; i<N; ++i)
    {
        CLP_INT degc = 0;
        CLP_INT idx0 = ridx[i];
        CLP_INT idx1 = ridx[i+1];
        for (int k=idx0; k<idx1; ++k)
        {
            CLP_INT idx = iv[k];
            if (idx < nLP)
            {
                degc += 1;
            }
            else
            {
                degc += clpinfo->nvblkSDP[idx-nLP];
            }
        }
        unique_coeffs[i] = coeffs[idx0];
        degNv[i] = degc;
    }

    for (size_t i=0; i<N; ++i)
    {
        double z = 0.0;
        for (size_t j=0; j<=i; ++j)
        {
            z += unique_coeffs[j] * (double)degNv[j];
        }
        for (size_t j=i+1; j<N; ++j)
        {
            z += unique_coeffs[i] * (double)degNv[j];
        }
        nuv[i] = z;
    }
    return;
}

regionInfo *create_regionInfo(const CLPinfo *clpinfo, const coeffCLP *coeff)
{
    INFO_CLP info = SUCCESS;
    regionInfo *rinfo = NULL;
    double *v = NULL;
    CLP_INT n, N, nLP, nblkSDP;
    nLP = clpinfo->nLP;
    nblkSDP = clpinfo->nblkSDP;
    n = nLP + nblkSDP;

    rinfo = (regionInfo*) CLP_MALLOC(sizeof(regionInfo));
    CHECKNULL(rinfo);
    rinfo->n = n;
    rinfo->iv = NULL;
    rinfo->degNv = NULL;
    rinfo->ridx = NULL;
    rinfo->nuv = NULL;
    rinfo->unique_coeffs = NULL;

    if (NOTNULLP(coeff))
    {
        v = (double*) CLP_MALLOC(sizeof(double)*n);
        CHECKNULL(v);
        rinfo->iv = (CLP_INT*) CLP_MALLOC(sizeof(CLP_INT)*n);
        CHECKNULL(rinfo->iv);
        for (size_t i=0; i<nLP; ++i)
        {
            v[i] = coeff->LP[i];
        }
        for (size_t i=0; i<nblkSDP; ++i)
        {
            v[i+nLP] = coeff->vSDP[i];
        }
        for (size_t i=0; i<n; ++i)
        {
            rinfo->iv[i] = (CLP_INT)i;
        }

        info = sort_coeffs2(n, v, rinfo->iv, &N);
        if (info != SUCCESS)
        {
            goto EXCEPTION;
        }
        rinfo->N = N;
        rinfo->degNv = (CLP_INT*) CLP_MALLOC(sizeof(CLP_INT)*N);
        rinfo->ridx = (CLP_INT*) CLP_MALLOC(sizeof(CLP_INT)*(N+1));
        rinfo->nuv = (double*) CLP_MALLOC(sizeof(double)*N);
        rinfo->unique_coeffs = (double*) CLP_MALLOC(sizeof(double)*N);
        CHECKNULL(rinfo->degNv);
        CHECKNULL(rinfo->ridx);
        CHECKNULL(rinfo->nuv);
        CHECKNULL(rinfo->unique_coeffs);
        
        compute_degs(clpinfo, n, N, v, rinfo->iv, rinfo->ridx, rinfo->degNv, 
            rinfo->unique_coeffs, rinfo->nuv);

        CLP_FREE(v);
    }
    else
    {
        N = 1;
        rinfo->N = N;
        rinfo->iv = (CLP_INT*) CLP_MALLOC(sizeof(CLP_INT)*n);
        rinfo->degNv = (CLP_INT*) CLP_MALLOC(sizeof(CLP_INT)*N);
        rinfo->ridx = (CLP_INT*) CLP_MALLOC(sizeof(CLP_INT)*(N+1));
        rinfo->nuv = (double*) CLP_MALLOC(sizeof(double)*N);
        rinfo->unique_coeffs = (double*) CLP_MALLOC(sizeof(double)*N);
        CHECKNULL(rinfo->iv);
        CHECKNULL(rinfo->degNv);
        CHECKNULL(rinfo->ridx);
        CHECKNULL(rinfo->nuv);
        CHECKNULL(rinfo->unique_coeffs);
        for (size_t i=0; i<n; ++i)
        {
            rinfo->iv[i] = (CLP_INT)i;
        }
        CLP_INT degN = nLP;
        if (nblkSDP > 0)
        {
            for (size_t k=0; k<nblkSDP; ++k)
            {
                degN += clpinfo->nvblkSDP[k];
            }
        }
        rinfo->degNv[0] = degN;
        rinfo->ridx[0] = 0;
        rinfo->ridx[1] = n;
        rinfo->nuv[0] = 0.0;
        rinfo->unique_coeffs[0] = 0.0;
    }
    return rinfo;

EXCEPTION:
    delete_regionInfo(rinfo);
    CLP_FREE(v);
    return NULL;
}

void delete_regionInfo(regionInfo *rinfo)
{
    if (NULLP(rinfo))
    {
        return;
    }

    CLP_FREE(rinfo->iv);
    CLP_FREE(rinfo->degNv);
    CLP_FREE(rinfo->ridx);
    CLP_FREE(rinfo->unique_coeffs);
    CLP_FREE(rinfo->nuv);
    CLP_FREE(rinfo);
    return;
}

dataCLP* create_dataCLP(CLPinfo *clpinfo, ACLP *aCLP, double *b, vecCLP *c,
    coeffCLP *logdet)
{
    dataCLP *dataclp=NULL;
    dataclp = (dataCLP*) CLP_MALLOC(sizeof(dataCLP));
    CHECKNULL(dataclp);
    dataclp->clpinfo = clpinfo;
    dataclp->A = aCLP;
    dataclp->b = b;
    dataclp->c = c;
    dataclp->nu = logdet;

    return dataclp;
EXCEPTION:
    return NULL;
}

/*
    delete dataclp object only
    Used only in factory method
*/
void delete_dataCLP(dataCLP *dataclp)
{
    if (NULLP(dataclp))
    {
        return;
    }
    CLP_FREE(dataclp);
    return;
}

/*
    Delete dataclp and its sub data.
*/
void deleteAll_dataCLP(dataCLP *dataclp)
{
    if (NULLP(dataclp))
    {
        return;
    }
    delete_CLPinfo(dataclp->clpinfo);
    delete_ACLP(dataclp->A);
    delete_dvec(dataclp->b);
    delete_vecCLP(dataclp->c);
    delete_coeffCLP(dataclp->nu);

    CLP_FREE(dataclp);
    return;
}

OPTIONS* init_OPTIONS(vecCLP *x0, vecCLP *s0, double *y0,
    const CLP_INT NITER, const CLP_INT verbose, const double gaptol,
    const CLP_INT NSTEP, double *stepvec)
{
    OPTIONS *z=NULL;
    z = (OPTIONS*) CLP_MALLOC(sizeof(OPTIONS));
    CHECKNULL(z);

    z->x0 = x0;
    z->s0 = s0;
    z->y0 = y0;
    z->NITER = NITER;
    z->verbose = verbose;
    z->gaptol = gaptol;
    z->NSTEP = NSTEP;
    z->stepvec = stepvec;

    return z;
EXCEPTION:
    CLP_FREE(z);
    return NULL;
}

void delete_OPTIONS(OPTIONS *ops)
{
    CLP_FREE(ops);
    return;
}

RESULTS* create_RESULTS(const CLPinfo *clpinfo)
{
    RESULTS *z=NULL;
    CLP_INT m;
    m = clpinfo->m;
    z = (RESULTS*) CLP_MALLOC(sizeof(RESULTS));
    CHECKNULL(z);

    z->opt = false;
    z->x = NULL;
    z->s = NULL;
    z->y = NULL;

    z->x = create_vecCLP(clpinfo);
    z->s = create_vecCLP(clpinfo);
    z->y = create_dvec(m);
    CHECKNULL(z->x);
    CHECKNULL(z->s);
    CHECKNULL(z->y);
    return z;

EXCEPTION:
    delete_RESULTS(z);
    return NULL;
}

void delete_RESULTS(RESULTS *z)
{
    if (NULLP(z))
    {
        return;
    }
    
    delete_vecCLP(z->x);
    delete_vecCLP(z->s);
    delete_dvec(z->y);
    CLP_FREE(z);
    return;
}
