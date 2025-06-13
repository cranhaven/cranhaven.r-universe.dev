#include <Rcpp.h>
using namespace Rcpp;

//' @title Return Triangular Numbers
//' 
//' @description
//' Return the series of triangular (/triangle) numbers up to a number of
//' \code{n} rows of a triangle. The series has the entry number "A000217"
//' at \url{https://oeis.org/A000217} and starts like this:
//' 0, 1, 3, 6, 10, ...
//' 
//' @usage ntri(n)
//' @param n Positive integer value for sequence length.
//'   
//' @return
//' Returns an integer vector of length \code{n}.
//' 
//' @name ntri
//' 
//' @keywords triangle triangular number
//' 
//' @import Rcpp
//' 
//' @author Sven E. Templer (\email{sven.templer@@gmail.com})

//' @export ntri
// [[Rcpp::export]]
NumericVector ntri(int n) {
  NumericVector ret(n);
  for (int i = 0; i < n; i ++) {
    ret[i] = i * (i + 1) / 2;
  }
  return ret ;
}
