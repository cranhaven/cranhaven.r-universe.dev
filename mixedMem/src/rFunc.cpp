#include "varInf.h"
#include "varInfExt.h"

//[[Rcpp::depends(RcppArmadillo)]]
//[[Rcpp::export]]
Rcpp::List varInfInputC(Rcpp::List model_r, int print,
                    int printMod, int stepType, int maxTotalIter,
                    int maxEIter, int maxAlphaIter, int maxThetaIter, int maxLSIter,
                    double elboTol, double alphaTol, double thetaTol, double aNaught,
                    double tau, int bMax, double bNaught, double bMult, int vCutoff, SEXP holdConstSEXP) {

    mm_model model = mm_model(model_r);
    NumericVector holdConst(holdConstSEXP);
    varInfC(model, print , printMod, stepType, maxTotalIter, maxEIter, maxAlphaIter, maxThetaIter, maxLSIter,
                              elboTol, alphaTol, thetaTol, aNaught, tau, bMax, bNaught, bMult, vCutoff, holdConst);
    return(model.returnModel());
}

//[[Rcpp::depends(RcppArmadillo)]]
//[[Rcpp::export]]
double computeElboC(Rcpp::List model_r)
{
    mm_model model = mm_model(model_r);
    return compute_ELBO(model);
}

//[[Rcpp::depends(RcppArmadillo)]]
//[[Rcpp::export]]
Rcpp::List varInfInputExtC(Rcpp::List model_r, int print,
                    int printMod, int stepType, int maxTotalIter,
                    int maxEIter, int maxAlphaIter, int maxThetaIter, int maxLSIter,
                    double elboTol, double alphaTol, double thetaTol, double aNaught,
                    double tau, int bMax, double bNaught, double bMult, int vCutoff, SEXP holdConstSEXP) {

    mm_modelExt model = mm_modelExt(model_r);
    NumericVector holdConst(holdConstSEXP);
    varInfExtC(model, print , printMod, stepType, maxTotalIter, maxEIter, maxAlphaIter, maxThetaIter, maxLSIter,
                              elboTol, alphaTol, thetaTol, aNaught, tau, bMax, bNaught, bMult, vCutoff, holdConst);
    return(model.returnModel());
}


//[[Rcpp::depends(RcppArmadillo)]]
//[[Rcpp::export]]
double computeElboExtC(Rcpp::List model_r)
{
    mm_modelExt model = mm_modelExt(model_r);
    return compute_ELBOExt(model);
}
