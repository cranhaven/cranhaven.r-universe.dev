#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
void Play() {
}
// double dlbinom(int x, int n, double pi) { 
//   double temp = std::lgamma(n + 1) - std::lgamma(x + 1) - std::lgamma((n - x) + 1);
//   temp += x * std::log(pi) + (n - x) * std::log(1 - pi);
//   return temp;
// }

// double dlbinom(int x, int n, double pi) { 
//   arma::vec const1(1), const2(1), const3(1), Pi(1);
//   const1(0) = n + 1; const2(0) = x + 1; const3(0) = (n - x) + 1; Pi(0) = pi;
//   arma::vec temp = arma::lgamma(const1);
//   temp -=  arma::lgamma(const2) + arma::lgamma(const3);
//   temp += x * arma::log(Pi) + (n - x) * arma::log(1 - Pi);
//   return arma::as_scalar(temp);
// }

/*** R
# dlbinom(4, 10, 0.3)

# dbinom(x = 4, size = 10, prob = 0.3, log = TRUE)
*/
