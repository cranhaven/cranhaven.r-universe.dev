/*
 *  SBMTrees: Sequential imputation with Bayesian Trees Mixed-Effects models
 *  Copyright (C) 2024 Jungang Zou
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/GPL-2
 */

#ifndef ARMADILLO_H_
#define ARMADILLO_H_
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
#endif

#ifndef DIST_H_
#define DIST_H_
#include <RcppDist.h>
// [[Rcpp::depends(RcppArmadillo, RcppDist)]]
#endif


#include <Rcpp.h>
#ifndef UTILS_H_
#define UTILS_H_
#include "utils.h"
#endif
#include <cmath>

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix update_Covariance(NumericMatrix B, NumericMatrix Mu, NumericMatrix inverse_wishart_matrix, double df, long N_subject){
  NumericMatrix B_diff_mu = matrix_add(B, matrix_mul_scalar(Mu, -1));
  NumericMatrix B_B_A = matrix_multiply(transpose(B_diff_mu), B_diff_mu);
  //Rcout <<  B_B_A  << std::endl;
  NumericMatrix Iwish_para = matrix_add(inverse_wishart_matrix, B_B_A);
  //Rcout << Iwish_para << std::endl;
  //Rcout <<  arma::chol(as<arma::mat>(Iwish_para)) << std::endl;
  //Iwish_para = fix_riwish(Iwish_para);
  //Iwish_para = make_symmetric(Iwish_para);
  NumericMatrix covariance = wrap(riwishArma(df + N_subject, as<arma::mat>(Iwish_para)));
  //Rcout << covariance << std::endl;
  //return fix_riwish(covariance);
  return covariance;
}
