#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]


//' @title Inclusion Probabilities
//' @name inclprob
//' @description Computes first-order inclusion probabilities from a vector of positive numbers.
//' 
//' @param x vector of positive numbers.
//' @param n sample size (could be a positive real value).
//' 
//' @details
//' The function is implemented in C++ so that it can be used in the code of other C++ functions. The implementation is based on the function  \code{\link[sampling]{inclusionprobabilities}} of the package sampling.
//'
//' @return A vector of inclusion probabilities proportional to \code{x} and such that the sum is equal to the value \code{n}.
//'
//' @author Raphael Jauslin \email{raphael.jauslin@@unine.ch}
//'
//' @seealso \code{\link[sampling]{inclusionprobabilities}}
//' 
//' @examples
//' 
//' x <- runif(100)
//' pik <- inclprob(x,70)
//' sum(pik)
//' 
//' @export
// [[Rcpp::export]]
arma::vec inclprob(arma::vec& x,
                   const double& n){
  
  double eps = 1e-6;
  arma::vec pik_tmp(n*(x/arma::sum(x)));
  arma::uvec up = arma::find(pik_tmp > 1.0 - eps);
  
  int l = up.size();
  
  if(l > 0){
    arma::vec a;
    int l1 = 0;
    do{
      
      // find smaller than 1.0 value
      arma::uvec inside = arma::find(pik_tmp < 1.0-eps);    
      a = pik_tmp.elem(inside);
      
      
      pik_tmp.elem(inside) = (n-l)*(a/sum(a));// calculate new inside value
      pik_tmp.elem(up).ones();// put 1.0 to exceed value
      l1 = l;// store exceed 1.0 value
      
      // recompute exceed 1.0 value
      up = arma::find(pik_tmp > 1.0 - eps); 
      l = up.size();
      
      
      // compare l1 and l to verified that no more 1.0 value exists    
    } while (abs(l1 - l) > eps); 
  }
  
  return(pik_tmp);
}



/*** R


x <- runif(100)
pik <- inclprob(x,70)
sum(pik)


*/
