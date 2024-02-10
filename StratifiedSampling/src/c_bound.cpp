#include <RcppArmadillo.h>
#include "inclprob.h"
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]


//' @title C bound
//' @name c_bound
//' @description This function is returning the number of unit that we need such that some conditions are fulfilled. See Details
//' 
//' @param pik vector of the inclusion probabilities.
//' 
//' @details
//' The function is computing the number of unit \eqn{K} that we need to add such that the following conditions are fulfilled :
//'
//' \itemize{
//' \item \eqn{\sum_{k = 1}^K \pi_k \geq 1}
//' \item \eqn{\sum_{k = 1}^K 1 - \pi_k \geq 1}
//' \item Let \eqn{c} be the constant such that \eqn{\sum_{k = 2}^K min(c\pi_k,1) = n }, we must have that \eqn{ \pi_1 \geq 1- 1/c}
//' }
//' 
//' @return An integer value, the number of units that we need to respect the constraints.
//'
//' @seealso \code{\link{osod}}
//'
//' @author Raphael Jauslin \email{raphael.jauslin@@unine.ch}
//'
//' 
//' @export
// [[Rcpp::export]]
int c_bound(arma::vec pik){
  
  int N = pik.size();
  // double n = arma::sum(pik);
  double eps = 1e-6;
  
  double x = pik[0.0];
  double x2 = (1-pik[0.0]);
  
  bool check = true;
  int j = 1;
  
  arma::vec pik_tmp;
  arma::vec pik_tmp2;
  
  do{
    
    arma::uvec index = arma::regspace<arma::uvec>(0, j);   
    arma::uvec index2 = arma::regspace<arma::uvec>(1, j);   
    
    x += pik[j];
    x2 += (1-pik[j]);
    
    double n_tmp = arma::sum(pik.elem(index));
    pik_tmp = pik.elem(index2);
    pik_tmp = inclprob(pik_tmp,n_tmp);
    
    double c = max(pik_tmp/pik.elem(index2));
    
    if(c < 1.0/(1.0-pik[0]) ){
      check = false;
    }else{
      check = true;
    }
    
    j = j+1;
    if(j > N-1){
      j = N;
      break;
    }
    
  } while ( (x < (1.0-eps)) || (x2 < (1.0-eps)) || check == true );
  
  return(j-1);
  
  
}

/*** R

pik <- inclprob(runif(100),10)
t <- c_bound(pik)
t
cond(pik)


*/



//' @title C bound
//' @name c_bound2
//' @description This function is returning the number of unit that we need such that some conditions are fulfilled. See Details
//' 
//' @param pik vector of the inclusion probabilities.
//' 
//' @details
//' The function is computing the number of unit \eqn{K} that we need to add such that the following conditions are fulfilled :
//'
//' \itemize{
//' \item \eqn{\sum_{k = 1}^K \pi_k \geq 1}
//' \item \eqn{\sum_{k = 1}^K 1 - \pi_k \geq 1}
//' \item Let \eqn{c} be the constant such that \eqn{\sum_{k = 2}^K min(c\pi_k,1) = n }, we must have that \eqn{ \pi_1 \geq 1- 1/c}
//' }
//' 
//' @return An integer value, the number of units that we need to respect the constraints.
//'
//' @seealso \code{\link{osod}}
//'
//' @author Raphael Jauslin \email{raphael.jauslin@@unine.ch}
//'
//' 
//' @export
// [[Rcpp::export]]
bool c_bound2(arma::vec pik){
  
  int N = pik.size();
  double eps = 1e-6;
  
  arma::vec pik_tmp;
  double n_tmp = arma::sum(pik);
  
  
  pik_tmp = pik(arma::span(1,N-1));
  pik_tmp = inclprob(pik_tmp,n_tmp);
    
  double c = max(pik_tmp/pik(arma::span(1,N-1)));
  
  if(c < 1.0/(1.0-pik[0])){
    return(true);
  }else{
    return(false);
  }
  
}

