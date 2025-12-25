/*
  This file is part of SNPknock.

    Copyright (C) 2017-2019 Matteo Sesia

    SNPknock is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    SNPknock is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with SNPknock.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "interface.h"

#include <progress.hpp>
#include <progress_bar.hpp>

using namespace Rcpp;

//' Wrapper for GroupGenotypes
//'
//' @keywords internal
// [[Rcpp::export]]
IntegerMatrix GroupGenotypes_wrapper(SEXP X_, SEXP r_, SEXP alpha_, SEXP theta_, SEXP groups_,
                                     SEXP n_, SEXP p_, SEXP seed_, SEXP display_progress_) {
  int n = as<int>(n_);
  int p = as<int>(p_);
  IntegerMatrix X = as<IntegerMatrix>(X_);
  vector r = numToVec(r_);
  vector2 alpha = numToVec2(alpha_,p);
  vector2 theta = numToVec2(theta_,p);
  ivector groups = numToIntVec(groups_);
  int seed = as<int>(seed_);
  bool display_progress = as<bool>(display_progress_);

  // Initialize knockoff generator
  GroupGenotypes knock(r, alpha, theta, groups, seed);

  // Generate knockoffs, one at a time
  Progress progr(n, display_progress);
  IntegerMatrix Xk_ = IntegerMatrix( Dimension(n,p));
  ivector Xk(p,0);
  for(int i=0; i<n; i++) {
    if (Progress::check_abort() )
      return (Xk_);
    Xk = knock.sample(ivector(X(i,_).begin(),X(i,_).end()));
    for(int j=0; j<p; j++) {
      Xk_(i,j) = Xk[j];
    }
    progr.increment(); // update progress
  }
  return(Xk_);
}

//' Wrapper for GroupHaplotypes
//'
//' @keywords internal
// [[Rcpp::export]]
IntegerMatrix GroupHaplotypes_wrapper(SEXP X_, SEXP r_, SEXP alpha_, SEXP theta_, SEXP groups_,
                                      SEXP n_, SEXP p_, SEXP seed_, SEXP display_progress_) {
  int n = as<int>(n_);
  int p = as<int>(p_);
  IntegerMatrix X = as<IntegerMatrix>(X_);
  vector r = numToVec(r_);
  vector2 alpha = numToVec2(alpha_,p);
  vector2 theta = numToVec2(theta_,p);
  ivector groups = numToIntVec(groups_);
  int seed = as<int>(seed_);
  bool display_progress = as<bool>(display_progress_);

  // Initialize knockoff generator
  GroupHaplotypes knock(r, alpha, theta, groups, seed);

  // Generate knockoffs, one at a time
  Progress progr(n, display_progress);
  IntegerMatrix Xk_ = IntegerMatrix( Dimension(n,p));
  ivector Xk(p,0);
  for(int i=0; i<n; i++) {
    if (Progress::check_abort() )
      return (Xk_);
    Xk = knock.sample(ivector(X(i,_).begin(),X(i,_).end()));
    for(int j=0; j<p; j++) {
      Xk_(i,j) = Xk[j];
    }
    progr.increment(); // update progress
  }
  return(Xk_);
}

//' Wrapper for DMC group knockoffs
//'
//' @keywords internal
// [[Rcpp::export]]
IntegerMatrix knockoffDMC_wrapper(SEXP X_, SEXP pInit_, SEXP Q_, SEXP n_, SEXP p_, SEXP K_,
                                   SEXP seed_, SEXP G_, SEXP display_progress_) {
  int n = as<int>(n_);
  int p = as<int>(p_);
  int K = as<int>(K_);
  IntegerMatrix X = as<IntegerMatrix>(X_);
  vector pInit = numToVec(pInit_);
  vector3 Q = numToVec3(Q_, p-1, K);
  int seed = as<int>(seed_);
  bool display_progress = as<bool>(display_progress_);
  ivector G = numToIntVec(G_);

  // Initialize knockoff generator
  KnockoffDMC knock(pInit, Q, G, seed);

  // Generate knockoffs, one at a time
  Progress progr(n, display_progress);
  IntegerMatrix Xk_ = IntegerMatrix( Dimension(n,p));
  ivector Xk(p,0);
  for(int i=0; i<n; i++) {
    if (Progress::check_abort() )
      return (Xk_);
    Xk = knock.sample(ivector(X(i,_).begin(),X(i,_).end()));
    for(int j=0; j<p; j++) {
      Xk_(i,j) = Xk[j];
    }
    progr.increment();
  }
  return(Xk_);
}

//' Wrapper for HMM group knockoffs
//'
//' @keywords internal
// [[Rcpp::export]]
IntegerMatrix knockoffHMM_wrapper(SEXP X_, SEXP pInit_, SEXP Q_, SEXP pEmit_,
                                   SEXP n_, SEXP p_, SEXP K_, SEXP M_,
                                   SEXP seed_, SEXP G_, SEXP display_progress_) {
  int n = as<int>(n_);
  int p = as<int>(p_);
  int K = as<int>(K_);
  int M = as<int>(M_);
  IntegerMatrix X = as<IntegerMatrix>(X_);
  vector pInit = numToVec(pInit_);
  vector3 Q = numToVec3(Q_, p-1, K);
  vector3 pEmit = numToVec3(pEmit_, p, M);
  int seed = as<int>(seed_);
  bool display_progress = as<bool>(display_progress_);
  ivector G = numToIntVec(G_);

  // Initialize knockoff generator
  KnockoffHMM knock(pInit, Q, pEmit, G, seed);

  // Generate knockoffs, one at a time
  Progress progr(n, display_progress);
  IntegerMatrix Xk_ = IntegerMatrix( Dimension(n,p));
  ivector Xk(p,0);
  for(int i=0; i<n; i++) {
    if (Progress::check_abort() )
      return (Xk_);
    Xk = knock.sample(ivector(X(i,_).begin(),X(i,_).end()));
    for(int j=0; j<p; j++) {
      Xk_(i,j) = Xk[j];
    }
    progr.increment();
  }
  return(Xk_);
}
