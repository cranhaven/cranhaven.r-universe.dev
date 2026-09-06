#ifndef ESTEP_H
#define ESTEP_H


#define BOOST_DISABLE_ASSERTS true

#include<stdlib.h>
#include<math.h>
#include <RcppArmadillo.h>
#include <boost/math/special_functions/digamma.hpp>
#include <boost/math/special_functions/gamma.hpp>
#include "mm_model.h"
#include "settings.h"
#include "varInf.h"
#include "utils.h"


using namespace Rcpp ;
using namespace arma;

double eStep_C(mm_model model, double elbo_E, int maxEIter, double elboTol, NumericVector iterReached);
double dl_ddelta(mm_model model, int i, int j, int r, int n, int k);

#endif

