// -----------------------------------------------------------------------------
// Copyright (C) 2019 University of Liège
// <penPHcure is an R package for for estimation, variable selection and
//  simulation of the semiparametric proportional-hazards (PH) cure model with
//  time-varying covariates.>
//  Authors: Alessandro Beretta & Cédric Heuchenne
//  Contact: a.beretta@uliege.be
//     
//  Licence as published by the Free Software Foundation, either version 3 of 
//  the Licence, or any later version. This program is distributed in the hope 
//  that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
//  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
//  General Public Licence for more details. You should have received a copy of 
//  the GNU General Public Licence along with this program.
//  If not, see <http://www.gnu.org/licenses/>.
// -----------------------------------------------------------------------------

#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]
#include <iostream>

using namespace arma;
using namespace std;
using namespace Rcpp;


// [[Rcpp::export]]
Rcpp::List initialize_PHcure_cpp(arma::vec tstart,arma::vec tstop,
                                 arma::uvec status,arma::mat X,
                                 std::string whichX){
  uword nobs = tstop.n_elem;
  uword i = 1;
  uword id = 0;
  uword j = 1;
  uword N;
  uword nvarX = X.n_cols;
  uvec status_FIX(nobs);
  status_FIX.fill(0);
  mat X_FIX = zeros(nobs,nvarX);
  uvec nobs_i(nobs);

  while (i < nobs){
    if (tstart[i]<tstop[i-1]){
      status_FIX[id] = status[i-1];
      nobs_i[id] = j;
      if (whichX=="mean")
        X_FIX.row(id) = sum((repmat(tstop.rows(i-j,i-1),1,nvarX) - 
          repmat(tstart.rows(i-j,i-1),1,nvarX)) % X.rows(i-j,i-1),0) /
            sum((repmat(tstop.rows(i-j,i-1),1,nvarX) - 
              repmat(tstart.rows(i-j,i-1),1,nvarX)),0); 
      else X_FIX.row(id) = X.row(i-1);
      id++;
      j = 0;
    }
    j++;
    i++;
  }
  N = id+1;
  status_FIX[id] = status[i-1];
  status_FIX = status_FIX.rows(0,id);
  nobs_i[id] = j;
  nobs_i = nobs_i.rows(0,id);
  if (whichX=="mean")
    X_FIX.row(id) = sum((repmat(tstop.rows(nobs-j,nobs-1),1,nvarX) - 
      repmat(tstart.rows(nobs-j,nobs-1),1,nvarX)) % X.rows(nobs-j,nobs-1),0) /
        sum((repmat(tstop.rows(nobs-j,nobs-1),1,nvarX) - 
          repmat(tstart.rows(nobs-j,nobs-1),1,nvarX)),0); 
  else X_FIX.row(id) = X.row(nobs-1);
  X_FIX = X_FIX.rows(0,id);
  return Rcpp::List::create(Rcpp::Named("N") = N,
                            Rcpp::Named("status_FIX") = status_FIX,
                            Rcpp::Named("X_FIX") = X_FIX,
                            Rcpp::Named("nobs_i") = nobs_i);
}


// [[Rcpp::export]]
Rcpp::List initialize_PHcure_noX_cpp(arma::vec tstart,arma::vec tstop,
                                     arma::uvec status){
  uword nobs = tstop.n_elem;
  uword i = 1;
  uword id = 0;
  uword j = 1;
  uword N;
  uvec status_FIX(nobs);
  status_FIX.fill(0);
  uvec nobs_i(nobs);
  
  while (i < nobs){
    if (tstart[i]<tstop[i-1]){
      status_FIX[id] = status[i-1];
      nobs_i[id] = j;
      id++;
      j = 0;
    }
    j++;
    i++;
  }
  N = id+1;
  status_FIX[id] = status[i-1];
  status_FIX = status_FIX.rows(0,id);
  nobs_i[id] = j;
  nobs_i = nobs_i.rows(0,id);
  return Rcpp::List::create(Rcpp::Named("N") = N,
                            Rcpp::Named("status_FIX") = status_FIX,
                            Rcpp::Named("nobs_i") = nobs_i);
}
