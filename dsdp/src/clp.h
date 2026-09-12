#ifndef _CLP_H_
#define _CLP_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <stdbool.h>
#include <math.h>

// #ifndef USE_FC_LEN_T
// #define USE_FC_LEN_T
// #endif

#include <R.h>

#ifndef M_PI
#define M_PI 3.141592653589793238462
#endif

#define sqrt2 1.4142135623730950488016895
#define sqrt05 0.70710678118654752440084392

#define CLP_INT int
#define EVENP(d) ((CLP_INT)(d) % 2 == 0)
#define ODDP(d) ((CLP_INT)(d) % 2 == 1)

#define NULLP(x) ((x) == NULL)
#define NOTNULLP(x) ((x) != NULL)

#define CHECKNULL(x) \
    do{ \
        if (NULLP(x)) \
        {   \
            CLP_PRINTF("ERROR: %s, %d\n", __FILE__, __LINE__);\
            goto EXCEPTION;  \
        }   \
    }while(0)


#define CHECKNULL2(x) \
    do{ \
        if (NULLP(x)) \
        {   \
            info = ERROR_MEMORY;\
            CLP_PRINTF("ERROR: %s, %d\n", __FILE__, __LINE__);\
            goto EXCEPTION;  \
        }   \
    }while(0)

#define CHECKINFO(x) \
    do{ \
        if ((x) != SUCCESS) \
        {   \
            CLP_PRINTF("ERROR: %s, %d\n", __FILE__, __LINE__);\
            goto EXCEPTION;  \
        }   \
    }while(0)


#define CLP_MALLOC(x)	malloc(x)
#define CLP_FREE(x)	free(x)
#define CLP_PRINTF(...) Rprintf(__VA_ARGS__)


typedef struct CLPinfo
{
    CLP_INT m;
    CLP_INT nLP;
    CLP_INT nblkSDP;
    CLP_INT n2SDP;
    CLP_INT nc2SDP;
    CLP_INT *nvblkSDP;
    CLP_INT *n2vblkSDP;
    CLP_INT *nc2vblkSDP;
    bool logdet_p;
} CLPinfo;

typedef struct coeffCLP
{
    CLP_INT nLP;
    CLP_INT nblkSDP;
    double *LP;
    double *vSDP;
} coeffCLP;

typedef struct vecCLP
{
    CLP_INT nLP;
    CLP_INT nblkSDP;
    CLP_INT *nvblkSDP;
    double *LP;
    double **SDP;
} vecCLP;

typedef struct ACLP
{
    CLP_INT m;
    CLP_INT nLP;
    CLP_INT nblkSDP;
    CLP_INT *nvblkSDP;
    double *LP;
    double **SDP;
} ACLP;

/*
    n: nLP + nblkSDP
    N: # of different logdet coefficients, including 0
    unique_coeffs: unique logdet coefficients in descending
        order, length N
    iv: n length vector
    ridx: (n+1) length vector, which indicates the range of iv vector
        corresponding each element of unique_coeffs.
        The index between iv[ridx[i]] and iv[ridx[i+1]-1] correspond to
        unique_coeffs[i], and the index between each region are ordered 
        in ascending order.
    nuv: n length vector whose elements are boundary values of each regions.
*/
typedef struct regionInfo
{
    CLP_INT N;
    CLP_INT n;
    CLP_INT *iv;
    CLP_INT *degNv;
    CLP_INT *ridx;
    double *unique_coeffs;
    double *nuv;
} regionInfo;

typedef struct dataCLP
{
    CLPinfo *clpinfo;
    ACLP *A;
    double *b;
    vecCLP *c;
    coeffCLP *nu;
} dataCLP;

typedef struct itrCLP
{
    vecCLP *x;
    vecCLP *s;
    double *y;
} itrCLP;

typedef struct RESULTS
{
    bool opt;
    vecCLP *x;
    vecCLP *s;
    double *y;
    double relgap;
    double pinfeas;
    double dinfeas;
} RESULTS;

typedef struct OPTIONS
{
    vecCLP *x0;
    vecCLP *s0;
    double *y0;
    CLP_INT NITER;
    CLP_INT verbose;
    double gaptol;
    CLP_INT NSTEP;
    double *stepvec;
} OPTIONS;

typedef enum INFO_CLP {
    SUCCESS = 0,
    ERROR_MEMORY,
    ERROR_FEASIBILITY_LU,
    ERROR_QR,
    ERROR_QX,
    ERROR_CHOL,
    ERROR_CHOLESKY_X,
    ERROR_CHOLESKY_S,
    ERROR_SVD,
    ERROR_INV, 
    ERROR_EIG,
    ERROR_IGAMMA,
    FAIL_OPT
} INFO_CLP;


double* create_dvec(const CLP_INT n);
void delete_dvec(double *z);
CLPinfo* create_CLPinfo(const CLP_INT m, const CLP_INT nLP, const CLP_INT nblkSDP,
    const CLP_INT *nvblkSDP, const bool logdet_p);
void delete_CLPinfo(CLPinfo *clpinfo);
vecCLP* create_vecCLP(const CLPinfo* clpinfo);
void copy_vecCLP(const vecCLP* x, vecCLP *z);
void mulscalar_vecCLP(const double alpha, vecCLP *z);
void cvec_vecCLP(const vecCLP *x, double* z);
void create_vecCLPfromcvec(const double *cvec, vecCLP *z);
void delete_vecCLP(vecCLP *z);
void print_vecCLP(const vecCLP *z);
itrCLP* create_itrCLP(const CLPinfo *clpinfo);
void copy_itrCLP(CLPinfo *clpinfo, const vecCLP *x, const vecCLP *s, 
    const double *y, itrCLP *z);
void copyback_itrCLP(const CLPinfo *clpinfo, const itrCLP *z, vecCLP *x,
    vecCLP *s, double *y);
void delete_itrCLP(itrCLP *z);
coeffCLP* create_coeffCLP(const CLPinfo *clpinfo);
void copy_coeffCLP(const coeffCLP *coeff, coeffCLP *c);
void delete_coeffCLP(coeffCLP *c);
ACLP* create_ACLP(const CLPinfo *clpinfo);
ACLP* init_ACLP(const CLPinfo *infoclp, const double *LP, const double **SDP);
void cmatACLP(const ACLP *A, double* Z);
void delete_ACLP(ACLP *A);
regionInfo *create_regionInfo(const CLPinfo *clpinfo, const coeffCLP *coeff);
void delete_regionInfo(regionInfo *rinfo);
dataCLP* create_dataCLP(CLPinfo *clpinfo, ACLP *aCLP, double *b, vecCLP *c,
    coeffCLP *logdet);
void delete_dataCLP(dataCLP *dataclp);
void deleteAll_dataCLP(dataCLP *dataclp);
OPTIONS* init_OPTIONS(vecCLP *x0, vecCLP *s0, double *y0,
    const CLP_INT NITER, const CLP_INT verbose, const double gaptol,
    const CLP_INT NSTEP, double *stepvec);
void delete_OPTIONS(OPTIONS *ops);
RESULTS* create_RESULTS(const CLPinfo *clpinfo);
void delete_RESULTS(RESULTS *z);
#endif
