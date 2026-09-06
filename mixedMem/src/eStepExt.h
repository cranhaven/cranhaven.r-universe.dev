#ifndef ESTEPEXT_H
#define ESTEPEXT_H


#define BOOST_DISABLE_ASSERTS true

#include<stdlib.h>
#include<math.h>
#include <RcppArmadillo.h>
#include <boost/math/special_functions/digamma.hpp>
#include <boost/math/special_functions/gamma.hpp>
#include "mm_modelExt.h"
#include "settings.h"
#include "varInfExt.h"
#include "utils.h"


using namespace Rcpp ;
using namespace arma;

double eStepExt(mm_modelExt model, double elbo_E, int maxEIter, double elboTol, NumericVector iterReached);
double dl_ddeltaExt(mm_modelExt model, int i, int j, int r, int n, int k);

#endif

