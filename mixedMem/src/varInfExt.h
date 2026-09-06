#ifndef VARINFEXT_H
#define VARINFEXT_H

#define BOOST_DISABLE_ASSERTS true

#include <stdlib.h>
#include <math.h>
#include <RcppArmadillo.h>
#include <boost/math/special_functions/digamma.hpp>
#include <boost/math/special_functions/gamma.hpp>
#include "mm_modelExt.h"
#include "settings.h"
#include "mStepExt.h"
#include "eStepExt.h"
#include "extended.h"


using namespace Rcpp;
using namespace arma;

double varInfExtC(mm_modelExt model_old, int print,
                    int printMod, int stepType, int maxTotalIter,
                    int maxEIter, int maxAlphaIter, int maxThetaIter, int maxLSIter,
                    double elboTol, double alphaTol, double thetaTol, double aNaught,
                    double tau, int bMax, double bNaught, double bMult, int vCutoff, NumericVector holdConst);
double compute_logfExt(mm_modelExt model);
double compute_ELBOExt(mm_modelExt model);
double alpha_ObjectiveExt(mm_modelExt model, vec alph);
double alpha_ObjectiveExt(mm_modelExt model);


#endif

