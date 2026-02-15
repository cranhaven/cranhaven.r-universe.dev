
#include "../unif.h"

using namespace Rcpp;

// [[Rcpp::export]]
void test_unif(NumericMatrix m) {
  unif(m, 1.01);
}

// [[Rcpp::export]]
void test_unif_sparse(S4 m1, S4 m2, S4 m3) {
  auto x1 = S4matrix<CSRMatrixT>(m1);
  auto x2 = S4matrix<CSCMatrixT>(m2);
  auto x3 = S4matrix<COOMatrixT>(m3);
  unif(x1, 1.01);
  unif(x2, 1.01);
  unif(x3, 1.01);
}

/*** R
library(Matrix)
m <- rbind(
  c(-10, 1, 9),
  c(0, -4, 4),
  c(2, 0, -2))
test_unif(m)
print(m)

m <- rbind(
  c(-10, 1, 9),
  c(0, -4, 4),
  c(2, 0, -2))
m1 <- as(m, "dgRMatrix")
m2 <- as(m, "dgCMatrix")
m3 <- as(m, "dgTMatrix")
test_unif_sparse(m1, m2, m3)
print(m1)
print(m2)
print(m3)
*/
