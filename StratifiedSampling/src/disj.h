#ifndef disj_H
#define disj_H

#include <RcppArmadillo.h>



arma::umat disj(arma::uvec strata);
arma::rowvec ncat(arma::umat Xcat);
arma::umat disjMatrix(arma::umat strata);
arma::mat findBarma(arma::mat X,arma::umat Xcat);

#endif
