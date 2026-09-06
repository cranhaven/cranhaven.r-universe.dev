#ifndef MSTEPEXT_H
#define MSTEPEXT_H


#define BOOST_DISABLE_ASSERTS true

#include <stdlib.h>
#include <math.h>
#include <RcppArmadillo.h>
#include <boost/math/special_functions/digamma.hpp>
#include <boost/math/special_functions/gamma.hpp>
#include "mm_modelExt.h"
#include "settings.h"
#include "varInfExt.h"
#include "utils.h"



using namespace Rcpp ;
using namespace arma;
double mStepExt(mm_modelExt model, double elbo_T, int stepType, int maxAlphaIter, int maxThetaIter, int maxLSIter,
                              double alphaTol, double thetaTol, double aNaught, double tau,
                               int bMax, double bNaught, double bMult, int vCutoff, NumericVector holdConst, NumericVector iterReached);
void updateThetaExt(mm_modelExt model, int maxThetaIter,
                 int maxLSIter, double thetaTol, double aNaught,
                 double tau, int bMax,
                 double bNaught, double bMult, int vCutoff, NumericVector holdConst, NumericVector iterReached);
vec getGradExt(mm_modelExt model);
mat getHessExt(mm_modelExt model);
vec getGradPLExt(mm_modelExt model, int j, int k, double b);
mat getHessPLExt(mm_modelExt model, int j, int k, double b);
double rank_ObjectiveExt(mm_modelExt model, vec theta, int j, int k, double b);
void update_PL_ThetaExt(mm_modelExt model, int j, int maxThetaIter,
                     int maxLSIter, double thetaTol, double aNaught,
                     double tau, double bMax,
                     double bNaught, double bMult, int vCutoff, NumericVector holdConst, NumericVector iterReached);
#endif
