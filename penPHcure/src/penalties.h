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

#ifndef penalties_hpp
#define penalties_hpp

#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]
#include <iostream>

using namespace arma;
using namespace std;


//  LASSO Penalties
double lasso_penalty_pert(const double& beta,const double& lambda,
                          const double& epsilon);

double lasso_penalty_prime_pert(const double& beta,const double& lambda,
                                const double& epsilon);


// SCAD Penalties   
double scad_penalty_pert(const double& beta,const double& lambda,
                         const double& a,const double& epsilon);

double scad_penalty_prime_pert(const double& beta,const double& lambda,
                               const double& a,const double& epsilon);

#endif
