#ifndef BAYESMETA_RANDOM_H
#define BAYESMETA_RANDOM_H
#include <stdio.h>
#include <cmath>
#include <Rmath.h>
#include <RcppArmadillo.h>
#include <Rdefines.h>

namespace RNG
{
    arma::mat rwish(arma::mat S, double v);
    arma::mat rwish(arma::mat &S, double v);
    arma::mat riwish(arma::mat S, double v);
    arma::mat riwish(arma::mat &S, double v);
}
#endif
