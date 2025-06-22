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

#ifndef UTILS_H_
#define UTILS_H_
#include "utils.h"
#endif
#include <cmath>
#include <iostream>
#include <unistd.h>

using namespace Rcpp;
using namespace std;

NumericMatrix update_B(NumericVector re, NumericMatrix Z, CharacterVector subject_id, NumericMatrix Mu, std::unordered_map<std::string, int> subject_to_B, NumericMatrix Covariance, double sigma){
  NumericMatrix B(subject_to_B.size(), Z.ncol());
                              
  CharacterVector subject_names = unique(subject_id);
  //Rcout << "inv_covariance" << std::endl;
  NumericMatrix inv_covariance = solve_pos_def(Covariance);
  //Rcout << as<NumericVector>(Mu) << std::endl;
  //NumericMatrix inv_covariance = solve_pos_def(make_symmetric(make_nonsingular(Covariance)));
  //NumericMatrix inv_covariance = solve(Covariance);
  //return inv_covariance;
  //Rcout << "B" << std::endl;
  //Rcout << inv_covariance << "  |  ";
  for(int i = 0; i < subject_names.length(); ++i){
    //Rcout << i << std::endl;
    //sleep(1);
    auto it = subject_to_B.find(std::string(subject_names[i]));
    if (it != subject_to_B.end()) {
      String name = subject_names[i];
      long B_position = (int)(it->second);
      //return inv_covariance;
      //std::cout << name << " ";
      //Rcout << i << " " << B_position << std::endl;
      CharacterVector subject = {name};
      LogicalVector index = character_vector_equals(subject_id, subject);
      //Rcout << index << std::endl;
      //return inv_covariance;
      //return List::create(Named("index") = index);
      NumericMatrix Zi = row_matrix(Z, index);
      //Rcout << index.length() << std::endl;
      //Rcout << re.length() << std::endl;
      //return inv_covariance;
      NumericVector Ri = re[index];
      //return List::create(Named("Zi") = Zi, Named("Ri") = Ri);
      //return inv_covariance;
      //double alpha_s = alpha[B_position];
      //Rcout << inv_covariance;
      NumericMatrix a = matrix_add(inv_covariance, matrix_mul_scalar(matrix_multiply(transpose(Zi), Zi), 1 / pow(sigma, 2)));
      NumericMatrix var = solve_pos_def(a);
      
      NumericVector mui_v = Mu(B_position, _);
      NumericMatrix mui = NumericMatrix(1, mui_v.length(), mui_v.begin());
      NumericMatrix ri = NumericMatrix(Ri.length(), 1, Ri.begin());
      NumericVector mu = wrap(matrix_multiply( matrix_add(matrix_multiply(mui, inv_covariance), transpose(matrix_mul_scalar(matrix_multiply(transpose(Zi), ri), 1/ pow(sigma,2)))), var));
      
      //Rcout << mu << " | " << var(0, 0) << " ||| ";
      
      NumericVector B_s = wrap(rmvnorm(1, mu, as<arma::mat>(var)));
      B(B_position, _) = B_s;
    }else{ 
      
    }
    
    
  //for(int i = 0; i < 1; ++i){
    
  }
  //Rcout <<std::endl;
  return B;
}
