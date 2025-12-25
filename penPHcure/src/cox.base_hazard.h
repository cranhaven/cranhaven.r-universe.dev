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

#ifndef cox_base_hazard_hpp
#define cox_base_hazard_hpp

#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]
#include <iostream>

using namespace arma;
using namespace std;

arma::vec cox_base_hazard_cpp(const arma::vec& beta,const arma::uword& nused,
                              const arma::uword& K,const arma::vec& start,
                              const arma::vec& tstop,const arma::uvec& event,
                              const arma::uvec& sort1,const arma::uvec& sort2,
                              const arma::mat& covar, const arma::vec& offset,
                              const arma::uword& method);

#endif
