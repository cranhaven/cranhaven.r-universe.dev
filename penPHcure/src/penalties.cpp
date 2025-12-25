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

////////////////////////////////////////////////////////////////////////////////
// BEGIN - LASSO Penalties                                                    //
////////////////////////////////////////////////////////////////////////////////

double lasso_penalty(const double& beta,const double& lambda){
  double pen=abs(beta)*lambda;
  return pen;
}

double lasso_penalty_prime(const double& beta,const double& lambda){
  double pen_prime=lambda;
  return pen_prime;
}

double lasso_penalty_pert(const double& beta,const double& lambda,
                          const double& epsilon){
  double pen = lasso_penalty(beta,lambda);
  double pen_pert = pen - 
    epsilon * lambda * (log(epsilon+abs(beta)) - log(epsilon));
  return pen_pert;
}

double lasso_penalty_prime_pert(const double& beta,const double& lambda,
                                const double& epsilon){
  double pen_prime = lasso_penalty_prime(beta,lambda);
  double pen_prime_pert = pen_prime /  (epsilon + abs(beta));
  return pen_prime_pert;
}


////////////////////////////////////////////////////////////////////////////////
// END - LASSO Penalties                                                      //
////////////////////////////////////////////////////////////////////////////////






////////////////////////////////////////////////////////////////////////////////
// BEGIN - SCAD Penalties                                                     //
////////////////////////////////////////////////////////////////////////////////

double scad_penalty(const double& beta,const double& lambda,const double& a){
  double pen;
  if (abs(beta)>=0 && abs(beta)<=lambda){
    pen = abs(beta) * lambda;
  }
  else if (abs(beta)>lambda && abs(beta)<=a*lambda){
    pen = ((pow(a,2.0)-1.0) * pow(lambda,2.0) - pow(abs(beta) - a*lambda,2.0)) /
      (2.0*(a-1.0));
  }
  else if (abs(beta)>a*lambda){
    pen = (a+1.0) * pow(lambda,2.0) / 2.0;
  }
  else{
    pen=0;
  }
  return pen;
}

double scad_penalty_prime(const double& beta,const double& lambda,
                          const double& a){
  double pen_prime;
  if (abs(beta)>=0 && abs(beta)<=lambda){
    pen_prime = lambda;
  }
  else if (abs(beta)>lambda && abs(beta)<=a*lambda){
    pen_prime = (a * lambda - abs(beta)) / (a - 1.0);
  }
  else if (abs(beta)>a*lambda){
    pen_prime = 0;
  }
  else{
    pen_prime = 0;
  }
  return pen_prime;
}

double scad_penalty_pert(const double& beta,const double& lambda,
                         const double& a,const double& epsilon){
  double pen_pert;
  double pen = scad_penalty(beta,lambda,a);
  if (abs(beta)>=0 && abs(beta)<=lambda){
    pen_pert=pen - epsilon * lambda * log((epsilon + abs(beta)) / epsilon);
  }
  else if (abs(beta)>lambda && abs(beta)<=a*lambda){
    pen_pert=pen - epsilon*( lambda*log((epsilon + lambda) / epsilon) +
        (lambda-abs(beta) + (a * lambda + epsilon) * log((epsilon+abs(beta)) / 
        (epsilon+lambda) ) )/(a-1.0) );
    
  }
  else if (abs(beta)>a*lambda){
    pen_pert=pen - epsilon*(lambda * log((epsilon+lambda)/epsilon) + 
      (lambda-a*lambda+(a*lambda+epsilon)*log( (epsilon+a*lambda) / 
      (epsilon+lambda) ) )/(a-1.0) );
  }
  else{
    pen_pert=0;
  }
  return pen_pert;
}

double scad_penalty_prime_pert(const double& beta,const double& lambda,
                               const double& a,const double& epsilon){
  double pen_prime = scad_penalty_prime(beta,lambda,a);
  double pen_prime_pert = pen_prime /  (epsilon + abs(beta));
  return pen_prime_pert;
}

////////////////////////////////////////////////////////////////////////////////
// END - SCAD Penalties                                                       //
////////////////////////////////////////////////////////////////////////////////

