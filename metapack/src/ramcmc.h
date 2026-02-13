#ifndef BAYESMETA_RAMCMC_H
#define BAYESMETA_RAMCMC_H
#include <RcppArmadillo.h>
#include <cmath>

arma::mat chol_update(arma::mat& L, arma::vec& u);
arma::mat chol_downdate(arma::mat& L, arma::vec& u);
void adapt_S(arma::mat& S, arma::vec& u, const double& current, const double& target, const int& n, const double& gamma);

#endif
