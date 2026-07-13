//Includes/namespaces
#include <Rcpp.h>
using namespace Rcpp;

//' @title
//' Rcpp Sugar version of runif
//' @description
//' Slightly faster than runif, and used a lot
//'
//' @param N Integer Vector
//' @export
//' @return
//' A vector of uniform random numbers as per \code{runif}
//'
//' @rdname rcpp-extras
// [[Rcpp::export]]
NumericVector runifC(const int N) {
  NumericVector X(N);
  X = runif(N);
  return X;
}
