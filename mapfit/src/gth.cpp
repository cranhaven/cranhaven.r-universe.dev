#include <Rcpp.h>
using namespace Rcpp;

#include "gth.h"

// [[Rcpp::export]]
NumericVector markov_gth_dense(NumericMatrix Q, NumericVector x) {
  markov_gth(Q, x);
  return x;
}

// [[Rcpp::export]]
NumericVector markov_gth_s4(S4 Q0, NumericVector x) {
  using MatrixT = S4matrix<CSCMatrixT>;
  auto Q = MatrixT(Q0);
  markov_gth(Q, x);
  return x;
}

// [[Rcpp::export]]
NumericVector map_gth_s4(S4 D00, S4 D10, NumericVector x) {
  using MatrixT = S4matrix<CSCMatrixT>;
  auto D0 = MatrixT(D00);
  auto D1 = MatrixT(D10);
  map_gth(D0, D1, x);
  return x;
}

/*** R
library(Matrix)

Q = rbind(
  c(-3, 2, 1),
  c(1, -2, 1),
  c(1, 0, -1)
)

x = c(0,0,0)

markov_gth_s4(as(Q, "dgCMatrix"), x)

print(Q)
print(x)

print(t(Q) %*% x)

D0 = rbind(
  c(-3, 2, 0),
  c(0, -2, 1),
  c(1, 0, -1)
)

D1 = rbind(
  c(1, 0, 0),
  c(0, 0, 1),
  c(0, 0, 0)
)

x = c(0,0,0)

map_gth_s4(as(D0, "dgCMatrix"), as(D1, "dgCMatrix"), x)

print(D0)
print(D1)
print(x)

print(t(D0 + D1) %*% x)
*/
