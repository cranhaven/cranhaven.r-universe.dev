#ifndef EXTENDED_H
#define EXTENDED_H


#include <stdlib.h>
#include <math.h>
#include <RcppArmadillo.h>
#include "mm_modelExt.h"
#include "settings.h"
#include "varInfExt.h"

using namespace Rcpp;
using namespace arma;

void updateP(mm_modelExt model);
void updateBeta(mm_modelExt model);
double getStayerProb(mm_modelExt model);
double getStayer_logf(mm_modelExt model, int stayerID);

#endif

