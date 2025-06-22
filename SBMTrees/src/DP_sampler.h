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

#ifndef DIST_H_
#define DIST_H_
#include <RcppDist.h>
// [[Rcpp::depends(RcppArmadillo, RcppDist)]]
#endif

#include <Rcpp.h>
#include <cmath>
#ifndef UTILS_H_
#define UTILS_H_
#include "utils.h"
#endif


using namespace Rcpp;

// [[Rcpp::export]]
List DP_sampler(long N, List & parameters){
  if(!parameters.containsElementNamed("distribution")){
    parameters["distribution"] = "normal";
  }
  if(!parameters.containsElementNamed("p")){
    parameters["p"] = 1;
  }
  mat y(N, (long)parameters["p"], fill::zeros);
  if(strcmp(parameters["distribution"], "normal") == 0){ // if strings are same, strcmp return 0
    
    if(!parameters.containsElementNamed("pi")){
      parameters["pi"] = 0.99;
    }
    
    if(!parameters.containsElementNamed("a")){
      parameters["a"] = 1.5;
    }
    if(!parameters.containsElementNamed("b")){
      parameters["b"] = (std::pow(as<double>(parameters["pi"]), 2) / 2 / (1 - std::pow(as<double>(parameters["pi"]), 2)));
    }
    double lambda_ = qinvgamma(0.95, as<double>(parameters["a"]), as<double>(parameters["b"]));//rinvgamma(as<double>(parameters["a"]), as<double>(parameters["b"]));//as<double>(parameters["b"]) / (as<double>(parameters["a"]) - 1);
    parameters["lambda"] = lambda_;
    
    if(parameters.containsElementNamed("sd")){
      //if(as<int>(parameters["p"]) == 1){
      if(!parameters.containsElementNamed("sd")){
        parameters["sd"] = 1;
      }
      if(!parameters.containsElementNamed("v")){
        parameters["v"] = 3;
      }
      if(!parameters.containsElementNamed("k")){
        parameters["k"] = (std::pow(as<double>(parameters["sd"]), 2) * (1 - std::pow(as<double>(parameters["pi"]), 2))) * (as<double>(parameters["v"]) - 2) / as<double>(parameters["v"]);
      }
      if(!parameters.containsElementNamed("mu")){
        parameters["mu"] = 0;
      }
      if(!parameters.containsElementNamed("sigma")){
        parameters["sigma"] = sqrt(as<double>(parameters["v"]) * as<double>(parameters["k"]) / (as<double>(parameters["v"]) - 2));
      }
      for(int k = 0 ; k < N ; ++k)
        y(k, 0) = R::rnorm(parameters["mu"], as<double>(parameters["sigma"]) * sqrt(lambda_));
    }else{
      if(!parameters.containsElementNamed("cov")){
        mat cv(parameters["p"], parameters["p"], fill::eye);
        parameters["cov"] = wrap(cv);
      }
      if(!parameters.containsElementNamed("mu")){
        parameters["mu"] = wrap(vec(as<long>(parameters["p"]), fill::zeros));
      }
      if(!parameters.containsElementNamed("d")){
        parameters["d"] = as<double>(parameters["p"]) + 2;
      }
      if(!parameters.containsElementNamed("Psi")){
        mat cv_2 = parameters["cov"];
        double sc = (as<double>(parameters["d"]) - as<double>(parameters["p"]) - 1) / (1 + as<double>(parameters["b"]) / (as<double>(parameters["a"]) - 1));
        parameters["Psi"] = wrap(cv_2 * sc);
      }
      if(!parameters.containsElementNamed("Sigma")){
        mat S = as<mat>(parameters["Psi"]) * (1 / (as<double>(parameters["d"]) - as<double>(parameters["p"]) - 1));
        parameters["Sigma"] = wrap(S);
      }
      //parameters["inv_Sigma"] = solve_pos_def(parameters["Sigma"]);
      mat covar = as<mat>(parameters["Sigma"]) * lambda_;
      for(int k = 0 ; k < N ; ++k)
        y.row(k) = rmvnorm(1, as<vec>(parameters["mu"]), covar);
    }
  }
  return List::create(Named("parameters") = parameters, Named("y") = wrap(y));
}
