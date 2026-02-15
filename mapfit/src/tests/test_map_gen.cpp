#include <Rcpp.h>
using namespace Rcpp;

#include "../map_gen.h"
#include "../emfit.h"

// [[Rcpp::export]]
void test_estep_group(NumericVector alpha,
                      NumericVector xi,
                      NumericMatrix D0,
                      NumericMatrix D1,
                      NumericMatrix P0,
                      NumericMatrix P1,
                      NumericMatrix H0,
                      NumericMatrix H1,
                      NumericMatrix en0,
                      NumericMatrix en1,
                      List data) {
  int n = alpha.length();
  int m = 5;
  auto tdat = as<NumericVector>(data["time"]);
  auto gdat = as<IntegerVector>(data["counts"]);
  auto idat = as<IntegerVector>(data["indicators"]);
  int maxcount = as<int>(data["maxcount"]);
  double maxtime = as<double>(data["maxtime"]);
  auto eres = MAPEres<NumericVector,NumericMatrix>(
    NumericVector(n),
    NumericVector(n),
    en0, en1);
  double llf;
  auto dat = MAPGroupSample<NumericVector,IntegerVector,IntegerVector>(tdat, gdat, idat, maxtime, maxcount);

  auto options = EMOptions();
  options.poisson_eps = 1.0e-8;
  options.ufactor = 1.01;

  IntegerVector di(n);
  diag(D0, di);
  copy(D0, P0);
  copy(D1, P1);
  double qv = unif(P0, di, options.ufactor);
  scal(1.0/qv, P1);

  auto model = MAP<NumericVector, NumericMatrix, IntegerVector>(alpha, xi, D0, D1, P0, P1, di, qv);
  auto work = MAPWorkSpace1<NumericMatrix>(m, n, H0, H1);
  llf = estep(model, dat, eres, options, work);
  Rcout << "llf=" << llf << std::endl;
  Rcout << eres.eb << std::endl;
  Rcout << sum(eres.eb) << std::endl;
  Rcout << eres.ez << std::endl;
  Rcout << sum(eres.ez) << std::endl;
  Rcout << eres.en0 << std::endl;
  Rcout << eres.en1 << std::endl;
  mstep(eres, model, options);
  // llf = estep(model, dat, eres, options, work);
  // mstep(eres, model, options);
  // Rcout << "llf=" << llf << std::endl;
  // llf = estep(model, dat, eres, options, work);
  // mstep(eres, model, options);
  // Rcout << "llf=" << llf << std::endl;
}

// [[Rcpp::export]]
void test_estep_group_csc(NumericVector alpha,
                      NumericVector xi,
                      S4 xD0,
                      S4 xD1,
                      S4 xP0,
                      S4 xP1,
                      S4 xH0,
                      S4 xH1,
                      S4 xen0,
                      S4 xen1,
                      List data) {
  using MatrixT = S4matrix<CSCMatrixT>;
  auto D0 = MatrixT(xD0);
  auto D1 = MatrixT(xD1);
  auto P0 = MatrixT(xP0);
  auto P1 = MatrixT(xP1);
  auto H0 = MatrixT(xH0);
  auto H1 = MatrixT(xH1);
  auto en0 = MatrixT(xen0);
  auto en1 = MatrixT(xen1);
  
  int n = alpha.length();
  int m = 5;
  auto tdat = as<NumericVector>(data["time"]);
  auto gdat = as<IntegerVector>(data["counts"]);
  auto idat = as<IntegerVector>(data["indicators"]);
  int maxcount = as<int>(data["maxcount"]);
  double maxtime = as<double>(data["maxtime"]);
  auto eres = MAPEres<NumericVector,MatrixT>(
    NumericVector(n),
    NumericVector(n),
    en0, en1);
  double llf;
  auto dat = MAPGroupSample<NumericVector,IntegerVector,IntegerVector>(tdat, gdat, idat, maxtime, maxcount);
  
  auto options = EMOptions();
  options.poisson_eps = 1.0e-8;
  options.ufactor = 1.01;
  
  IntegerVector di(n);
  diag(D0, di);
  copy(D0, P0);
  copy(D1, P1);
  double qv = unif(P0, di, options.ufactor);
  scal(1.0/qv, P1);
  
  auto model = MAP<NumericVector, MatrixT, IntegerVector>(alpha, xi, D0, D1, P0, P1, di, qv);
  auto work = MAPWorkSpace1<MatrixT>(m, n, H0, H1);
  llf = estep(model, dat, eres, options, work);
  Rcout << "llf=" << llf << std::endl;
  Rcout << eres.eb << std::endl;
  Rcout << sum(eres.eb) << std::endl;
  Rcout << eres.ez << std::endl;
  Rcout << sum(eres.ez) << std::endl;
  // Rcout << eres.en0 << std::endl;
  // Rcout << eres.en1 << std::endl;
  mstep(eres, model, options);
  llf = estep(model, dat, eres, options, work);
  Rcout << "llf=" << llf << std::endl;
}

// [[Rcpp::export]]
void test_estep_group_csr(NumericVector alpha,
                          NumericVector xi,
                          S4 xD0,
                          S4 xD1,
                          S4 xP0,
                          S4 xP1,
                          S4 xH0,
                          S4 xH1,
                          S4 xen0,
                          S4 xen1,
                          List data) {
  using MatrixT = S4matrix<CSRMatrixT>;
  auto D0 = MatrixT(xD0);
  auto D1 = MatrixT(xD1);
  auto P0 = MatrixT(xP0);
  auto P1 = MatrixT(xP1);
  auto H0 = MatrixT(xH0);
  auto H1 = MatrixT(xH1);
  auto en0 = MatrixT(xen0);
  auto en1 = MatrixT(xen1);
  
  int n = alpha.length();
  int m = 5;
  auto tdat = as<NumericVector>(data["time"]);
  auto gdat = as<IntegerVector>(data["counts"]);
  auto idat = as<IntegerVector>(data["indicators"]);
  int maxcount = as<int>(data["maxcount"]);
  double maxtime = as<double>(data["maxtime"]);
  auto eres = MAPEres<NumericVector,MatrixT>(
    NumericVector(n),
    NumericVector(n),
    en0, en1);
  double llf;
  auto dat = MAPGroupSample<NumericVector,IntegerVector,IntegerVector>(tdat, gdat, idat, maxtime, maxcount);
  
  auto options = EMOptions();
  options.poisson_eps = 1.0e-8;
  options.ufactor = 1.01;
  
  IntegerVector di(n);
  diag(D0, di);
  copy(D0, P0);
  copy(D1, P1);
  double qv = unif(P0, di, options.ufactor);
  scal(1.0/qv, P1);
  
  auto model = MAP<NumericVector, MatrixT, IntegerVector>(alpha, xi, D0, D1, P0, P1, di, qv);
  auto work = MAPWorkSpace1<MatrixT>(m, n, H0, H1);
  llf = estep(model, dat, eres, options, work);
  Rcout << "llf=" << llf << std::endl;
  Rcout << eres.eb << std::endl;
  Rcout << sum(eres.eb) << std::endl;
  Rcout << eres.ez << std::endl;
  Rcout << sum(eres.ez) << std::endl;
  // Rcout << eres.en0 << std::endl;
  // Rcout << eres.en1 << std::endl;
  mstep(eres, model, options);
  llf = estep(model, dat, eres, options, work);
  Rcout << "llf=" << llf << std::endl;
}

// [[Rcpp::export]]
void test_estep_group_coo(NumericVector alpha,
                          NumericVector xi,
                          S4 xD0,
                          S4 xD1,
                          S4 xP0,
                          S4 xP1,
                          S4 xH0,
                          S4 xH1,
                          S4 xen0,
                          S4 xen1,
                          List data) {
  using MatrixT = S4matrix<COOMatrixT>;
  auto D0 = MatrixT(xD0);
  auto D1 = MatrixT(xD1);
  auto P0 = MatrixT(xP0);
  auto P1 = MatrixT(xP1);
  auto H0 = MatrixT(xH0);
  auto H1 = MatrixT(xH1);
  auto en0 = MatrixT(xen0);
  auto en1 = MatrixT(xen1);
  
  int n = alpha.length();
  int m = 5;
  auto tdat = as<NumericVector>(data["time"]);
  auto gdat = as<IntegerVector>(data["counts"]);
  auto idat = as<IntegerVector>(data["indicators"]);
  int maxcount = as<int>(data["maxcount"]);
  double maxtime = as<double>(data["maxtime"]);
  auto eres = MAPEres<NumericVector,MatrixT>(
    NumericVector(n),
    NumericVector(n),
    en0, en1);
  double llf;
  auto dat = MAPGroupSample<NumericVector,IntegerVector,IntegerVector>(tdat, gdat, idat, maxtime, maxcount);
  
  auto options = EMOptions();
  options.poisson_eps = 1.0e-8;
  options.ufactor = 1.01;
  
  IntegerVector di(n);
  diag(D0, di);
  copy(D0, P0);
  copy(D1, P1);
  double qv = unif(P0, di, options.ufactor);
  scal(1.0/qv, P1);
  
  auto model = MAP<NumericVector, MatrixT, IntegerVector>(alpha, xi, D0, D1, P0, P1, di, qv);
  auto work = MAPWorkSpace1<MatrixT>(m, n, H0, H1);
  llf = estep(model, dat, eres, options, work);
  Rcout << "llf=" << llf << std::endl;
  Rcout << eres.eb << std::endl;
  Rcout << sum(eres.eb) << std::endl;
  Rcout << eres.ez << std::endl;
  Rcout << sum(eres.ez) << std::endl;
  // Rcout << eres.en0 << std::endl;
  // Rcout << eres.en1 << std::endl;
  mstep(eres, model, options);
  llf = estep(model, dat, eres, options, work);
  Rcout << "llf=" << llf << std::endl;
}

// [[Rcpp::export]]
void test_estep_group_dge(NumericVector alpha,
                          NumericVector xi,
                          S4 xD0,
                          S4 xD1,
                          S4 xP0,
                          S4 xP1,
                          S4 xH0,
                          S4 xH1,
                          S4 xen0,
                          S4 xen1,
                          List data) {
  using MatrixT = S4matrix<DenseMatrixT>;
  auto D0 = MatrixT(xD0);
  auto D1 = MatrixT(xD1);
  auto P0 = MatrixT(xP0);
  auto P1 = MatrixT(xP1);
  auto H0 = MatrixT(xH0);
  auto H1 = MatrixT(xH1);
  auto en0 = MatrixT(xen0);
  auto en1 = MatrixT(xen1);
  
  int n = alpha.length();
  int m = 5;
  auto tdat = as<NumericVector>(data["time"]);
  auto gdat = as<IntegerVector>(data["counts"]);
  auto idat = as<IntegerVector>(data["indicators"]);
  int maxcount = as<int>(data["maxcount"]);
  double maxtime = as<double>(data["maxtime"]);
  auto eres = MAPEres<NumericVector,MatrixT>(
    NumericVector(n),
    NumericVector(n),
    en0, en1);
  double llf;
  auto dat = MAPGroupSample<NumericVector,IntegerVector,IntegerVector>(tdat, gdat, idat, maxtime, maxcount);
  
  auto options = EMOptions();
  options.poisson_eps = 1.0e-8;
  options.ufactor = 1.01;
  
  IntegerVector di(n);
  diag(D0, di);
  copy(D0, P0);
  copy(D1, P1);
  double qv = unif(P0, di, options.ufactor);
  scal(1.0/qv, P1);
  
  auto model = MAP<NumericVector, MatrixT, IntegerVector>(alpha, xi, D0, D1, P0, P1, di, qv);
  auto work = MAPWorkSpace1<MatrixT>(m, n, H0, H1);
  llf = estep(model, dat, eres, options, work);
  Rcout << "llf=" << llf << std::endl;
  Rcout << eres.eb << std::endl;
  Rcout << sum(eres.eb) << std::endl;
  Rcout << eres.ez << std::endl;
  Rcout << sum(eres.ez) << std::endl;
  // Rcout << eres.en0 << std::endl;
  // Rcout << eres.en1 << std::endl;
  mstep(eres, model, options);
  llf = estep(model, dat, eres, options, work);
  Rcout << "llf=" << llf << std::endl;
  mstep(eres, model, options);
  llf = estep(model, dat, eres, options, work);
  Rcout << "llf=" << llf << std::endl;
}

/*** R
alpha <- c(0.4, 0.6)
xi <- c(1,1)
D0 <- rbind(
  c(-4, 2),
  c(1, -5)
)
D1 <- rbind(
  c(2, 0),
  c(1, 3)
)
P0 <- matrix(0, 2, 2)
P1 <- matrix(0, 2, 2)
H0 <- matrix(0, 2, 2)
H1 <- matrix(0, 2, 2)
en0 <- matrix(0, 2, 2)
en1 <- matrix(0, 2, 2)
dat <- list(time=c(1,2,1,3,4), counts=c(1,3,0,2,4), indicators=c(0,0,1,0,0), maxtime=4, maxcount=4)
test_estep_group(alpha, xi, D0, D1, P0, P1, H0, H1, en0, en1, dat)
P0 <- matrix(0, 2, 2)
P1 <- matrix(0, 2, 2)
H0 <- matrix(0, 2, 2)
H1 <- matrix(0, 2, 2)
en0 <- matrix(0, 2, 2)
en1 <- matrix(0, 2, 2)
dat <- list(time=c(1,2,1,3,4), counts=c(1,3,0,2,4), indicators=c(0,0,1,0,0), maxtime=4, maxcount=4)
test_estep_group(alpha, xi, D0, D1, P0, P1, H0, H1, en0, en1, dat)

# matclass <- "dgCMatrix"
# alpha <- c(0.4, 0.6)
# xi <- c(1,1)
# D0 <- rbind(
#   c(-4, 2),
#   c(1, -5)
# )
# D1 <- rbind(
#   c(2, 0),
#   c(1, 3)
# )
# P0 <- as(D0, matclass)
# H0 <- as(D0, matclass)
# en0 <- as(D0, matclass)
# P1 <- as(D1, matclass)
# H1 <- as(D1, matclass)
# en1 <- as(D1, matclass)
# dat <- list(time=c(1,2,1,3,4), counts=c(1,3,0,2,4), indicators=c(0,0,1,0,0), maxtime=4, maxcount=4)
# test_estep_group_csc(alpha, xi, as(D0, matclass), as(D1, matclass), P0, P1, H0, H1, en0, en1, dat)
# print(P0)
# print(P1)
# 
# matclass <- "dgRMatrix"
# alpha <- c(0.4, 0.6)
# xi <- c(1,1)
# D0 <- rbind(
#   c(-4, 2),
#   c(1, -5)
# )
# D1 <- rbind(
#   c(2, 0),
#   c(1, 3)
# )
# P0 <- as(D0, matclass)
# H0 <- as(D0, matclass)
# en0 <- as(D0, matclass)
# P1 <- as(D1, matclass)
# H1 <- as(D1, matclass)
# en1 <- as(D1, matclass)
# dat <- list(time=c(1,2,1,3,4), counts=c(1,3,0,2,4), indicators=c(0,0,1,0,0), maxtime=4, maxcount=4)
# test_estep_group_csr(alpha, xi, as(D0, matclass), as(D1, matclass), P0, P1, H0, H1, en0, en1, dat)
# print(P0)
# print(P1)
# 
# matclass <- "dgTMatrix"
# alpha <- c(0.4, 0.6)
# xi <- c(1,1)
# D0 <- rbind(
#   c(-4, 2),
#   c(1, -5)
# )
# D1 <- rbind(
#   c(2, 0),
#   c(1, 3)
# )
# P0 <- as(D0, matclass)
# H0 <- as(D0, matclass)
# en0 <- as(D0, matclass)
# P1 <- as(D1, matclass)
# H1 <- as(D1, matclass)
# en1 <- as(D1, matclass)
# dat <- list(time=c(1,2,1,3,4), counts=c(1,3,0,2,4), indicators=c(0,0,1,0,0), maxtime=4, maxcount=4)
# test_estep_group_coo(alpha, xi, as(D0, matclass), as(D1, matclass), P0, P1, H0, H1, en0, en1, dat)
# print(P0)
# print(P1)

matclass <- "dgeMatrix"
alpha <- c(0.4, 0.6)
xi <- c(1,1)
D0 <- rbind(
  c(-4, 2),
  c(1, -5)
)
D1 <- rbind(
  c(2, 0),
  c(1, 3)
)
P0 <- as(D0, matclass)
H0 <- as(D0, matclass)
en0 <- as(D0, matclass)
P1 <- as(D1, matclass)
H1 <- as(D1, matclass)
en1 <- as(D1, matclass)
dat <- list(time=c(1,2,1,3,4), counts=c(1,3,0,2,4), indicators=c(0,0,1,0,0), maxtime=4, maxcount=4)
test_estep_group_dge(alpha, xi, as(D0, matclass), as(D1, matclass), P0, P1, H0, H1, en0, en1, dat)
print(P0)
print(P1)
print(P0+P1)

matclass <- "dgeMatrix"
alpha <- c(0.5242189, 0.4757811)
xi <- c(1,1)
D0 <- rbind(
  c(-5, 0.9858063),
  c(0.03233593, -0.8333333)
)
D1 <- rbind(
  c(3.1061022, 0.9080915),
  c(0.1558961, 0.6451013)
)
P0 <- as(D0, matclass)
H0 <- as(D0, matclass)
en0 <- as(D0, matclass)
P1 <- as(D1, matclass)
H1 <- as(D1, matclass)
en1 <- as(D1, matclass)
dat <- list(time=c(1,1,1,1,1,1,1,1), counts=c(1,4,0,5,2,0,0,1), indicators=c(0,0,0,0,0), maxtime=1, maxcount=5)
test_estep_group_dge(alpha, xi, as(D0, matclass), as(D1, matclass), P0, P1, H0, H1, en0, en1, dat)
print(P0)
print(P1)
print(P0+P1)
*/
