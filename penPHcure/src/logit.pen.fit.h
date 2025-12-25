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

#ifndef logit_pen_fit_hpp
#define logit_pen_fit_hpp

#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]
#include <iostream>
#include <string>

using namespace arma;
using namespace std;

Rcpp::List fit_pen_logit_cpp(const arma::vec& b_SV,const arma::mat& X,
                             const arma::vec& Y,const bool& warnings,
                             const arma::uword& maxiter,const double& tol,
                             const arma::uword& N,const arma::vec& tun_par,
                             const arma::vec& pen_weights,
                             const std::string& pen_type,const double& epsilon);

#endif
