
#include "../blas.h"

using namespace Rcpp;

// [[Rcpp::export]]
void test_blas(NumericVector x, NumericVector y, NumericMatrix m, S4 m2) {
  // NumericVector x = NumericVector::create(100, -20, 3);
  // NumericVector y = NumericVector::create(1, 2, 3);
  Rcout << "The value of x: " << x << std::endl;
  Rcout << "The value of y: " << y << std::endl;
  Rcout << "dot " << dot(x, y) << std::endl;
  Rcout << "asum " << asum(x) << std::endl;
  Rcout << "iamax " << iamax(x) << std::endl;
  scal(10.0, x);
  Rcout << "The value of x (scal(10.0, x)): " << x << std::endl;
  fill(x, 0.0);
  Rcout << "The value of x (fill(x, 0.0)): " << x << std::endl;
  copy(y, x);
  Rcout << "The value of x (copy(y, x)): " << x << std::endl;
  Rcout << "Matrix m" << std::endl << m << std::endl;
  scal(10.0, m);
  Rcout << "Matrix m (scal(10.0, m))" << std::endl << m << std::endl;
  if (m2.inherits("dgCMatrix")) {
    S4matrix<CSCMatrixT> m3 = S4matrix<CSCMatrixT>(m2);
    scal(100.0, m3);
  } else if (m2.inherits("dgRMatrix")) {
    S4matrix<CSRMatrixT> m3 = S4matrix<CSRMatrixT>(m2);
    scal(100.0, m3);
  } else if (m2.inherits("dgTMatrix")) {
    S4matrix<COOMatrixT> m3 = S4matrix<COOMatrixT>(m2);
    scal(100.0, m3);
  } else if (m2.inherits("dgeMatrix")) {
    S4matrix<DenseMatrixT> m3 = S4matrix<DenseMatrixT>(m2);
    scal(100.0, m3);
  }
}

// // [[Rcpp::export]]
// void test_svec(NumericMatrix A) {
//   int n = A.nrow();
//   int m = A.ncol();
//   std::vector<std::vector<double>> pool(m, std::vector<double>(n));
//   for (int k=0; k<m; k++) {
//     pool[k].len = n;
//     pool[k].value = &A(0,k);
//   }
//   scal(1000.0, pool[0]);
//   auto x = Vec<double>(n, &A(0,1));
//   scal(-1000.0, x);
// }


// [[Rcpp::export]]
void test_level2(NumericVector x, NumericVector y, NumericMatrix m, S4 m2) {
  Rcout << "The value of x: " << x << std::endl;
  Rcout << "The value of y: " << y << std::endl;
  Rcout << "The value of m: " << m << std::endl;
  gemv(NOTRANS{}, 1.0, m, x, 0.0, y);
  Rcout << "The value of x: " << x << std::endl;
  Rcout << "The value of y: " << y << std::endl;
  if (m2.inherits("dgCMatrix")) {
    gemv(NOTRANS{}, 1.0, S4matrix<CSCMatrixT>(m2), x, 0.0, y);
  } else if (m2.inherits("dgRMatrix")) {
    gemv(NOTRANS{}, 1.0, S4matrix<CSRMatrixT>(m2), x, 0.0, y);
  } else if (m2.inherits("dgTMatrix")) {
    gemv(NOTRANS{}, 1.0, S4matrix<COOMatrixT>(m2), x, 0.0, y);
  } else if (m2.inherits("dgeMatrix")) {
    gemv(NOTRANS{}, 1.0, S4matrix<DenseMatrixT>(m2), x, 0.0, y);
  }
  Rcout << "The value of x: " << x << std::endl;
  Rcout << "The value of y: " << y << std::endl;
  ger(NOTRANS{}, 1.0, x, y, m);
  Rcout << "The value of m: " << m << std::endl;
  if (m2.inherits("dgCMatrix")) {
    S4matrix<CSCMatrixT> m3 = S4matrix<CSCMatrixT>(m2);
    ger(NOTRANS{}, 1.0, x, y, m3);
  } else if (m2.inherits("dgRMatrix")) {
    S4matrix<CSRMatrixT> m3 = S4matrix<CSRMatrixT>(m2);
    ger(NOTRANS{}, 1.0, x, y, m3);
  } else if (m2.inherits("dgTMatrix")) {
    S4matrix<COOMatrixT> m3 = S4matrix<COOMatrixT>(m2);
    ger(NOTRANS{}, 1.0, x, y, m3);
  } else if (m2.inherits("dgeMatrix")) {
    S4matrix<DenseMatrixT> m3 = S4matrix<DenseMatrixT>(m2);
    ger(NOTRANS{}, 1.0, x, y, m3);
  }
}

// [[Rcpp::export]]
void test_level3(NumericMatrix x, NumericMatrix y, NumericMatrix m, S4 m2) {
  Rcout << "The value of x: " << x << std::endl;
  Rcout << "The value of y: " << y << std::endl;
  Rcout << "The value of m: " << m << std::endl;
  gemm(NOTRANS{}, NOTRANS{}, 1.0, m, x, 0.0, y);
  Rcout << "The value of x: " << x << std::endl;
  Rcout << "The value of y: " << y << std::endl;
  if (m2.inherits("dgCMatrix")) {
    gemm(NOTRANS{}, NOTRANS{}, 1.0, S4matrix<CSCMatrixT>(m2), x, 0.0, y);
  } else if (m2.inherits("dgRMatrix")) {
    gemm(NOTRANS{}, NOTRANS{}, 1.0, S4matrix<CSRMatrixT>(m2), x, 0.0, y);
  } else if (m2.inherits("dgTMatrix")) {
    gemm(NOTRANS{}, NOTRANS{}, 1.0, S4matrix<COOMatrixT>(m2), x, 0.0, y);
  } else if (m2.inherits("dgeMatrix")) {
    gemm(NOTRANS{}, NOTRANS{}, 1.0, S4matrix<DenseMatrixT>(m2), x, 0.0, y);
  }
  Rcout << "The value of x: " << x << std::endl;
  Rcout << "The value of y: " << y << std::endl;
}

// [[Rcpp::export]]
void test_lapack(NumericMatrix A, NumericVector B, NumericVector C) {
  gesv(TRANS{}, -1.0, A, B, C);
}

// [[Rcpp::export]]
void test_mcopy_csr(S4 B, NumericMatrix A) {
  auto b = S4matrix<CSRMatrixT>(B);
  mcopy(b, A);
}

// [[Rcpp::export]]
void test_mcopy_csc(S4 B, NumericMatrix A) {
  auto b = S4matrix<CSCMatrixT>(B);
  mcopy(b, A);
}

// [[Rcpp::export]]
void test_mcopy_coo(S4 B, NumericMatrix A) {
  auto b = S4matrix<COOMatrixT>(B);
  mcopy(b, A);
}

// [[Rcpp::export]]
void test_mcopy_dge(S4 B, NumericMatrix A) {
  auto b = S4matrix<DenseMatrixT>(B);
  mcopy(b, A);
}

/*** R
library(Matrix)
x <- c(100,-20,3)
y <- c(1,2,3)
m <- matrix(c(1,0,3,0,5,6), 2, 3)
m2 <- as(m, "dgCMatrix")
test_blas(x, y, m, m2)
print(m2)

x <- c(100,-20,3)
y <- c(100,-20,3)
m <- matrix(c(1,0,3,0,5,6,2,3,4), 3, 3)
m2 <- as(m, "dgRMatrix")
test_level2(x, y, m, m2)
print(m2)
m <- matrix(c(1,0,3,0,5,6,2,3,4), 3, 3)
m2 <- as(m, "dgCMatrix")
test_level2(x, y, m, m2)
print(m2)
m <- matrix(c(1,0,3,0,5,6,2,3,4), 3, 3)
m2 <- as(m, "dgTMatrix")
test_level2(x, y, m, m2)
print(m2)
m <- matrix(c(1,0,3,0,5,6,2,3,4), 3, 3)
m2 <- as(m, "dgeMatrix")
test_level2(x, y, m, m2)
print(m2)

x <- matrix(c(100,-20,3,3,5,6,2,4,4), 3, 3)
y <- matrix(c(100,-20,3,1,3,4,5,6,6), 3, 3)
m <- matrix(c(1,0,3,0,5,6,2,3,4), 3, 3)
m2 <- as(m, "dgRMatrix")
test_level3(x, y, m, m2)
print(m2)
m <- matrix(c(1,0,3,0,5,6,2,3,4), 3, 3)
m2 <- as(m, "dgCMatrix")
test_level3(x, y, m, m2)
print(m2)
m <- matrix(c(1,0,3,0,5,6,2,3,4), 3, 3)
m2 <- as(m, "dgTMatrix")
test_level3(x, y, m, m2)
print(m2)
m <- matrix(c(1,0,3,0,5,6,2,3,4), 3, 3)
m2 <- as(m, "dgeMatrix")
test_level3(x, y, m, m2)
print(m2)
m <- matrix(c(1,0,3,0,5,6,2,3,4), 3, 3)
#test_svec(m)
print(m)

A <- cbind(
  c(-10, 8, 0),
  c(1, -2, 1),
  c(3, 0, -5))
B <- c(0.5, 0.4, 0.1)
C <- rep(0.0, 3)
test_lapack(A, B, C)
print(A)
print(B)
print(C)

m <- matrix(c(1,0,3,0,5,6,2,3,4), 3, 3)
m2 <- as(m, "dgRMatrix")
m1 <- matrix(0, 3, 3)
test_mcopy_csr(m2, m1)
print(m1)
print(m2)

m <- matrix(c(1,0,3,0,5,6,2,3,4), 3, 3)
m2 <- as(m, "dgCMatrix")
m1 <- matrix(0, 3, 3)
test_mcopy_csc(m2, m1)
print(m1)
print(m2)

m <- matrix(c(1,0,3,0,5,6,2,3,4), 3, 3)
m2 <- as(m, "dgTMatrix")
m1 <- matrix(0, 3, 3)
test_mcopy_coo(m2, m1)
print(m1)
print(m2)

m <- matrix(c(1,0,3,0,5,6,2,3,4), 3, 3)
m2 <- as(m, "dgeMatrix")
m1 <- matrix(0, 3, 3)
test_mcopy_dge(m2, m1)
print(m1)
print(m2)
*/
