#ifndef SETTINGS_H
#define SETTINGS_H

#include <stdlib.h>
#include <math.h>
#include <RcppArmadillo.h>


// After normalization, if any of the values hit numerical 0, then bump by this amount
const double BUMP =  1e-16;
const double COND_BOUND =  1e18;
const std::string BERNOULLI =  "bernoulli"; //value for dist when identifying distribution type
const std::string MULTINOMIAL =  "multinomial";
const std::string RANK =  "rank";

#endif
