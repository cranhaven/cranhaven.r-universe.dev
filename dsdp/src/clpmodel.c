#include "clp.h"
#include "clpsol.h"
#include "clputil.h"

/*
    Create Gauss model
    d: degree of polynomial, must be positive and even
    mu: mean of model
    sig: standard deviation of model
    nSample: # of samples
    data: data vector for estimation
    chat: # of occurrence for each sample value
*/
dataCLP* create_GaussModel(const CLP_INT d, const double mu, const double sig,
    CLP_INT nSample, const double *data, const double *chat)
{
    INFO_CLP info=SUCCESS;
    CLP_INT m, nLP, nblkSDP, n1, n1len;
    CLP_INT nvblkSDP[1];
    CLPinfo *clpinfo=NULL;
    dataCLP *dataclp=NULL;
    ACLP *aCLP=NULL;
    double *b=NULL;
    vecCLP *c=NULL;
    coeffCLP *clogdet=NULL;

    n1 = d/2 + 1;
    n1len = n1*n1;
    m = nSample + 1;
    nLP = nSample;
    nblkSDP = 1;
    nvblkSDP[0] = n1;
    clpinfo = create_CLPinfo(m, nLP, nblkSDP, nvblkSDP, true);
    CHECKNULL(clpinfo);

    aCLP = create_ACLP(clpinfo);
    CHECKNULL(aCLP);
    for (CLP_INT j=0; j<nSample; ++j)
    {
        aCLP->LP[j+j*nLP] = -1.0;
    }
    info = compute_GaussDistDataMatrix(d, nSample, data, aCLP->SDP[0]);
    CHECKINFO(info);
    compute_GaussDistMomentMatrix(d, mu, sig, &(aCLP->SDP[0][nSample*n1len]));

    b = create_dvec(m);
    CHECKNULL(b);
    b[m-1] = 1.0;

    c = create_vecCLP(clpinfo);
    CHECKNULL(c);

    clogdet = create_coeffCLP(clpinfo);
    CHECKNULL(clogdet);
    if (NULLP(chat))
    {
        for (CLP_INT i=0; i<nSample; ++i)
        {
            clogdet->LP[i] = 1.0;
        }
    }
    else
    {
        for (CLP_INT i=0; i<nSample; ++i)
        {
            clogdet->LP[i] = chat[i];
        }
    }
    dataclp = create_dataCLP(clpinfo, aCLP, b, c, clogdet);
    CHECKNULL(dataclp);

    return dataclp;

EXCEPTION:
    delete_CLPinfo(clpinfo);
    
    delete_ACLP(aCLP);
    delete_dvec(b);
    delete_vecCLP(c);
    delete_coeffCLP(clogdet);
    delete_dataCLP(dataclp);
    return NULL;
}

/*
    Create model for d >= 3
    d: degree of polynomial
    lmd: exponential model parameter, lmd * exp(-lmd*x)
    nSample: # of samples
    data: sample vector
    chat: # of occurrence for each sample value
*/
dataCLP* create_ExpModel(const CLP_INT d, const double lmd,
    CLP_INT nSample, const double *data, const double *chat)
{
    INFO_CLP info=SUCCESS;
    CLP_INT m, nLP, nblkSDP, n1, n2, n1len, n2len;
    CLP_INT nvblkSDP[2];
    CLPinfo *clpinfo=NULL;
    dataCLP *dataclp=NULL;
    ACLP *aCLP=NULL;
    double *b=NULL;
    vecCLP *c=NULL;
    coeffCLP *clogdet=NULL;

    if (ODDP(d))
    {
        n1 = (d-1)/2 + 1;
        n2 = n1;
    }
    else
    {
        n1 = d/2+1;
        n2 = d/2;
    }
    n1len = n1*n1;
    n2len = n2*n2;
    m = nSample + 1;
    nLP = nSample;
    nblkSDP = 2;
    nvblkSDP[0] = n1;
    nvblkSDP[1] = n2;
    clpinfo = create_CLPinfo(m, nLP, nblkSDP, nvblkSDP, true);
    CHECKNULL(clpinfo);

    aCLP = create_ACLP(clpinfo);
    CHECKNULL(aCLP);
    for (CLP_INT j=0; j<nSample; ++j)
    {
        aCLP->LP[j+j*nLP] = -1.0;
    }
    info = compute_ExpDistDataMatrix(d, nSample, data, aCLP->SDP[0], aCLP->SDP[1]);
    CHECKINFO(info);
    compute_ExpDistMomentMatrix(d, lmd, &(aCLP->SDP[0][nSample*n1len]),
        &(aCLP->SDP[1][nSample*n2len]));

    b = create_dvec(m);
    CHECKNULL(b);
    b[m-1] = 1.0;

    c = create_vecCLP(clpinfo);
    CHECKNULL(c);

    clogdet = create_coeffCLP(clpinfo);
    CHECKNULL(clogdet);
    if (NULLP(chat))
    {
        for (CLP_INT i=0; i<nSample; ++i)
        {
            clogdet->LP[i] = 1.0;
        }
    }
    else
    {
        for (CLP_INT i=0; i<nSample; ++i)
        {
            clogdet->LP[i] = chat[i];
        }
    }
    dataclp = create_dataCLP(clpinfo, aCLP, b, c, clogdet);
    CHECKNULL(dataclp);

    return dataclp;

EXCEPTION:
    delete_CLPinfo(clpinfo);
    
    delete_ACLP(aCLP);
    delete_dvec(b);
    delete_vecCLP(c);
    delete_coeffCLP(clogdet);
    delete_dataCLP(dataclp);
    return NULL;
}

/*
    Create Exp model of 1 degree polynomial.
    This model is formulated via LP.
*/
dataCLP* create_ExpModel1d(const double lmd,
    CLP_INT nSample, const double *data, const double *chat)
{
    CLP_INT m, nLP;
    CLPinfo *clpinfo=NULL;
    dataCLP *dataclp=NULL;
    ACLP *aCLP=NULL;
    double *b=NULL;
    vecCLP *c=NULL;
    coeffCLP *clogdet=NULL;

    m = nSample + 1;
    nLP = nSample + 2;

    clpinfo = create_CLPinfo(m, nLP, 0, NULL, true);
    CHECKNULL(clpinfo);

    aCLP = create_ACLP(clpinfo);
    CHECKNULL(aCLP);
    for (CLP_INT j=0; j<nSample; ++j)
    {
        aCLP->LP[j+j*nLP] = -1.0;
    }
    for (CLP_INT j=0; j<nSample; ++j)
    {
        aCLP->LP[nSample+j*nLP] = 1.0;
        aCLP->LP[nSample+1+j*nLP] = data[j];
    }
    aCLP->LP[nSample+(m-1)*nLP] = momentExpDist(0, lmd);
    aCLP->LP[nSample+1+(m-1)*nLP] = momentExpDist(1, lmd);

    b = create_dvec(m);
    CHECKNULL(b);
    b[m-1] = 1.0;

    c = create_vecCLP(clpinfo);
    CHECKNULL(c);

    clogdet = create_coeffCLP(clpinfo);
    CHECKNULL(clogdet);
    if (NULLP(chat))
    {
        for (CLP_INT i=0; i<nSample; ++i)
        {
            clogdet->LP[i] = 1.0;
        }
    }
    else
    {
        for (CLP_INT i=0; i<nSample; ++i)
        {
            clogdet->LP[i] = chat[i];
        }
    }
    dataclp = create_dataCLP(clpinfo, aCLP, b, c, clogdet);
    CHECKNULL(dataclp);

    return dataclp;

EXCEPTION:
    delete_CLPinfo(clpinfo);
    
    delete_ACLP(aCLP);
    delete_dvec(b);
    delete_vecCLP(c);
    delete_coeffCLP(clogdet);
    delete_dataCLP(dataclp);
    return NULL;
}

/*
    Create Exp model of 2 degree polynomial.
    This model can be formulated by LP + single SDP block.
*/
dataCLP* create_ExpModel2d(const double lmd,
    CLP_INT nSample, const double *data, const double *chat)
{
    CLP_INT m, nLP, nblkSDP;
    CLP_INT nvblkSDP[1];
    CLPinfo *clpinfo=NULL;
    dataCLP *dataclp=NULL;
    ACLP *aCLP=NULL;
    double *b=NULL;
    vecCLP *c=NULL;
    coeffCLP *clogdet=NULL;

    m = nSample + 1;
    nLP = nSample + 1;
    nblkSDP = 1;
    nvblkSDP[0] = 2;
    clpinfo = create_CLPinfo(m, nLP, nblkSDP, nvblkSDP, true);
    CHECKNULL(clpinfo);

    aCLP = create_ACLP(clpinfo);
    CHECKNULL(aCLP);
    for (CLP_INT j=0; j<nSample; ++j)
    {
        aCLP->LP[j+j*nLP] = -1.0;
    }
    // 2nd constraints in nSample-th row of LP blk
    for (CLP_INT j=0; j<nSample; ++j)
    {
        aCLP->LP[nSample+j*nLP] = data[j];
    }
    aCLP->LP[nSample+(m-1)*nLP] = momentExpDist(1, lmd);

    // SDP BLK
    double *S = aCLP->SDP[0];
    for (CLP_INT j=0; j<nSample; ++j)
    {
        double x = data[j];
        S[j*4] = 1.0;
        S[1+j*4] = x;
        S[2+j*4] = x;
        S[3+j*4] = x*x;
    }
    S[nSample*4] = momentExpDist(0,lmd);
    S[1+nSample*4] = momentExpDist(1,lmd);
    S[2+nSample*4] = momentExpDist(1,lmd);
    S[3+nSample*4] = momentExpDist(2,lmd);

    b = create_dvec(m);
    CHECKNULL(b);
    b[m-1] = 1.0;

    c = create_vecCLP(clpinfo);
    CHECKNULL(c);

    clogdet = create_coeffCLP(clpinfo);
    CHECKNULL(clogdet);
    if (NULLP(chat))
    {
        for (CLP_INT i=0; i<nSample; ++i)
        {
            clogdet->LP[i] = 1.0;
        }
    }
    else
    {
        for (CLP_INT i=0; i<nSample; ++i)
        {
            clogdet->LP[i] = chat[i];
        }
    }
    dataclp = create_dataCLP(clpinfo, aCLP, b, c, clogdet);
    CHECKNULL(dataclp);

    return dataclp;

EXCEPTION:
    delete_CLPinfo(clpinfo);
    
    delete_ACLP(aCLP);
    delete_dvec(b);
    delete_vecCLP(c);
    delete_coeffCLP(clogdet);
    delete_dataCLP(dataclp);
    return NULL;
}

CLP_INT solve_GaussModel(const CLP_INT d, const double mu, const double sig,
    CLP_INT nSample, const double *data, const double *chat, const bool verbose,
    double *M1, double *aic, double *gap, const CLP_INT NSTEP, double *stepvec)
{
    INFO_CLP info=SUCCESS;
    CLP_INT n1;
    CLPinfo *clpinfo=NULL;
    dataCLP *dataclp=NULL;
    OPTIONS *options=NULL;
    RESULTS *results=NULL;

    dataclp = create_GaussModel(d, mu, sig, nSample, data, chat);
    CHECKNULL2(dataclp);
    clpinfo = dataclp->clpinfo;
    options = init_OPTIONS(NULL, NULL, NULL, 400, verbose, 1e-7, NSTEP, stepvec);
    CHECKNULL2(options);
    results = create_RESULTS(clpinfo);
    CHECKNULL2(results);
    
    info = solver(dataclp, options, results);
    CHECKINFO(info);
    if (results->opt)
    {
        n1 = clpinfo->nvblkSDP[0];
        *aic = aic_GaussDist(d, nSample, results->x->LP, chat, data, mu, sig);
        memcpy(M1, results->x->SDP[0], sizeof(double)*n1*n1);
        *gap = fmax(results->relgap, fmax(results->pinfeas, results->dinfeas));
    }
    else
    {
        info = FAIL_OPT;
    }

EXCEPTION:
    deleteAll_dataCLP(dataclp);
    delete_OPTIONS(options);
    delete_RESULTS(results);
    return info;
}

CLP_INT solve_ExpModel(const CLP_INT d, const double lmd, CLP_INT nSample,
     const double *data, const double *chat, const bool verbose,
     double *M1, double *M2, double *aic, double *gap,
     const CLP_INT NSTEP, double *stepvec)
{
    INFO_CLP info=SUCCESS;
    CLP_INT n1, n2;
    CLPinfo *clpinfo=NULL;
    dataCLP *dataclp=NULL;
    OPTIONS *options=NULL;
    RESULTS *results=NULL;

    if (d == 1)
    {
        dataclp = create_ExpModel1d(lmd, nSample, data, chat);
    }
    else if (d == 2)
    {
        dataclp = create_ExpModel2d(lmd, nSample, data, chat);
    }
    else if (d > 2)
    {
        dataclp = create_ExpModel(d, lmd, nSample, data, chat);
    }
    CHECKNULL2(dataclp);
    clpinfo = dataclp->clpinfo;
    options = init_OPTIONS(NULL, NULL, NULL, 500, verbose, 1e-7, NSTEP, stepvec);
    CHECKNULL2(options);
    results = create_RESULTS(clpinfo);
    CHECKNULL2(results);

    info = solver(dataclp, options, results);
    CHECKINFO(info);
    if (results->opt)
    {
        *aic = aic_ExpDist(d, nSample, results->x->LP, chat, data, lmd);
        *gap = fmax(results->relgap, fmax(results->pinfeas, results->dinfeas));

        if (d == 1)
        {
            *M1 = results->x->LP[nSample];
            *M2 = results->x->LP[nSample+1];
        }
        else if (d == 2)
        {
            n1 = 2;
            memcpy(M1, results->x->SDP[0], sizeof(double)*n1*n1);
            *M2 = results->x->LP[nSample];
        }
        else if (d > 2)
        {
            n1 = clpinfo->nvblkSDP[0];
            n2 = clpinfo->nvblkSDP[1];
            memcpy(M1, results->x->SDP[0], sizeof(double)*n1*n1);
            memcpy(M2, results->x->SDP[1], sizeof(double)*n2*n2);
        }

    }
    else
    {
        info = FAIL_OPT;
    }

EXCEPTION:
    deleteAll_dataCLP(dataclp);
    delete_OPTIONS(options);
    delete_RESULTS(results);
    return info;
}

