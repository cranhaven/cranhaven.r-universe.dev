#ifndef _CLP_MODEL_
#define _CLP_MODEL_
#include "clp.h"
#include "clpsol.h"

dataCLP* create_GaussModel(const CLP_INT d, const double mu, const double sig,
    CLP_INT nSample, const double *data, const double *chat);
dataCLP* create_ExpModel(const CLP_INT d, const double lmd,
    CLP_INT nSample, const double *data, const double *chat);
dataCLP* create_ExpModel1d(const double lmd,
    CLP_INT nSample, const double *data, const double *chat);
dataCLP* create_ExpModel2d(const double lmd,
    CLP_INT nSample, const double *data, const double *chat);
CLP_INT solve_GaussModel(const CLP_INT d, const double mu, const double sig,
    CLP_INT nSample, const double *data, const double *chat, const bool verbose,
    double *M1, double *aic, double *gap, const CLP_INT NSTEP, double *stepvec);
CLP_INT solve_ExpModel(const CLP_INT d, const double lmd, CLP_INT nSample,
     const double *data, const double *chat, const bool verbose,
     double *M1, double *M2, double *aic, double *accuracy,
     const CLP_INT NSTEP, double *stepvec);
#endif
