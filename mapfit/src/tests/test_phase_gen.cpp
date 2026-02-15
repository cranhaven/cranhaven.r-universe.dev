#include <Rcpp.h>
using namespace Rcpp;

#include "../phase_gen.h"
#include "../emfit.h"

// #include "gperftools/profiler.h"
// 
// // [[Rcpp::export]]
// SEXP start_profiler(SEXP str) {
//   ProfilerStart(as<const char*>(str));
//   return R_NilValue;
// }
// 
// // [[Rcpp::export]]
// SEXP stop_profiler() {
//   ProfilerStop();
//   return R_NilValue;
// }

// [[Rcpp::export]]
void test_estep_wtime(NumericVector alpha,
                      NumericMatrix Q,
                      NumericVector xi,
                      List data) {
  int n = alpha.length();
  int m = 5;

  auto tdat = as<NumericVector>(data["time"]);
  auto wdat = as<NumericVector>(data["weights"]);
  double maxtime = as<double>(data["maxtime"]);
  NumericMatrix H(n,n);
  auto eres = GPHEres<NumericVector, NumericMatrix>(
    NumericVector(n),
    NumericVector(n),
    NumericVector(n),
    H);
  auto options = EMOptions();
  options.poisson_eps = 1.0e-8;
  options.ufactor = 1.01;
  double llf;
  auto dat = PHWeightSample<NumericVector,NumericVector>(tdat, wdat, maxtime);
  
  auto P = clone(Q);
  IntegerVector di(n);
  diag(Q, di);
  double qv = unif(P, di, 1.01);
  auto model = GPH<NumericVector, NumericMatrix, IntegerVector>(alpha, Q, P, xi, qv, di);

  auto work = GPHWorkSpace1(m, n);
  llf = estep(model, dat, eres, options, work);
  Rcout << llf << std::endl;
  Rcout << eres.eb << std::endl;
  Rcout << sum(eres.eb) << std::endl;
  Rcout << eres.ey << std::endl;
  Rcout << sum(eres.ey) << std::endl;
  Rcout << eres.ez << std::endl;
  Rcout << sum(eres.ez) << std::endl;
  Rcout << eres.en << std::endl;
  mstep(eres, model, options);
  llf = estep(model, dat, eres, options, work);
  Rcout << llf << std::endl;
}

// [[Rcpp::export]]
void test_estep_wtime_csc(NumericVector alpha,
                      S4 Q0,
                      NumericVector xi,
                      S4 P0,
                      S4 H0,
                      List data) {
  int n = alpha.length();
  int m = 5;

  auto Q = S4matrix<CSCMatrixT>(Q0);
  auto P = S4matrix<CSCMatrixT>(P0);
  auto H = S4matrix<CSCMatrixT>(H0);
  
  auto tdat = as<NumericVector>(data["time"]);
  auto wdat = as<NumericVector>(data["weights"]);
  double maxtime = as<double>(data["maxtime"]);

  auto eres = GPHEres<NumericVector, S4matrix<CSCMatrixT>>(
    NumericVector(n),
    NumericVector(n),
    NumericVector(n),
    H);
  auto options = EMOptions();
  options.poisson_eps = 1.0e-8;
  options.ufactor = 1.01;
  double llf;
  auto dat = PHWeightSample<NumericVector,NumericVector>(tdat, wdat, maxtime);

  IntegerVector di(n);
  diag(Q, di);
  double qv = unif(P, di, 1.01);
  auto model = GPH<NumericVector, S4matrix<CSCMatrixT>, IntegerVector>(alpha, Q, P, xi, qv, di);

  auto work = GPHWorkSpace1(m, n);
  llf = estep(model, dat, eres, options, work);
  Rcout << llf << std::endl;
  Rcout << eres.eb << std::endl;
  Rcout << sum(eres.eb) << std::endl;
  Rcout << eres.ey << std::endl;
  Rcout << sum(eres.ey) << std::endl;
  Rcout << eres.ez << std::endl;
  Rcout << sum(eres.ez) << std::endl;
  mstep(eres, model, options);
  Rcout << "2 x" << std::endl;
  llf = estep(model, dat, eres, options, work);
  Rcout << llf << std::endl;
}

// [[Rcpp::export]]
void test_estep_wtime_csr(NumericVector alpha,
                          S4 Q0,
                          NumericVector xi,
                          S4 P0,
                          S4 H0,
                          List data) {
  int n = alpha.length();
  int m = 5;
  
  auto Q = S4matrix<CSRMatrixT>(Q0);
  auto P = S4matrix<CSRMatrixT>(P0);
  auto H = S4matrix<CSRMatrixT>(H0);
  
  auto tdat = as<NumericVector>(data["time"]);
  auto wdat = as<NumericVector>(data["weights"]);
  double maxtime = as<double>(data["maxtime"]);
  
  auto eres = GPHEres<NumericVector, S4matrix<CSRMatrixT>>(
    NumericVector(n),
    NumericVector(n),
    NumericVector(n),
    H);
  auto options = EMOptions();
  options.poisson_eps = 1.0e-8;
  options.ufactor = 1.01;
  double llf;
  auto dat = PHWeightSample<NumericVector,NumericVector>(tdat, wdat, maxtime);
  
  IntegerVector di(n);
  diag(Q, di);
  double qv = unif(P, di, 1.01);
  auto model = GPH<NumericVector, S4matrix<CSRMatrixT>, IntegerVector>(alpha, Q, P, xi, qv, di);
  
  auto work = GPHWorkSpace1(m, n);
  llf = estep(model, dat, eres, options, work);
  Rcout << llf << std::endl;
  Rcout << eres.eb << std::endl;
  Rcout << sum(eres.eb) << std::endl;
  Rcout << eres.ey << std::endl;
  Rcout << sum(eres.ey) << std::endl;
  Rcout << eres.ez << std::endl;
  Rcout << sum(eres.ez) << std::endl;
  mstep(eres, model, options);
  Rcout << "2 x" << std::endl;
  llf = estep(model, dat, eres, options, work);
  Rcout << llf << std::endl;
}

// [[Rcpp::export]]
void test_estep_wtime_coo(NumericVector alpha,
                          S4 Q0,
                          NumericVector xi,
                          S4 P0,
                          S4 H0,
                          List data) {
  int n = alpha.length();
  int m = 5;
  
  auto Q = S4matrix<COOMatrixT>(Q0);
  auto P = S4matrix<COOMatrixT>(P0);
  auto H = S4matrix<COOMatrixT>(H0);
  
  auto tdat = as<NumericVector>(data["time"]);
  auto wdat = as<NumericVector>(data["weights"]);
  double maxtime = as<double>(data["maxtime"]);
  
  auto eres = GPHEres<NumericVector, S4matrix<COOMatrixT>>(
    NumericVector(n),
    NumericVector(n),
    NumericVector(n),
    H);
  auto options = EMOptions();
  options.poisson_eps = 1.0e-8;
  options.ufactor = 1.01;
  double llf;
  auto dat = PHWeightSample<NumericVector,NumericVector>(tdat, wdat, maxtime);
  
  IntegerVector di(n);
  diag(Q, di);
  double qv = unif(P, di, 1.01);
  auto model = GPH<NumericVector, S4matrix<COOMatrixT>, IntegerVector>(alpha, Q, P, xi, qv, di);
  
  auto work = GPHWorkSpace1(m, n);
  llf = estep(model, dat, eres, options, work);
  Rcout << llf << std::endl;
  Rcout << eres.eb << std::endl;
  Rcout << sum(eres.eb) << std::endl;
  Rcout << eres.ey << std::endl;
  Rcout << sum(eres.ey) << std::endl;
  Rcout << eres.ez << std::endl;
  Rcout << sum(eres.ez) << std::endl;
  mstep(eres, model, options);
  Rcout << "2 x" << std::endl;
  llf = estep(model, dat, eres, options, work);
  Rcout << llf << std::endl;
}

// [[Rcpp::export]]
void test_estep_group(NumericVector alpha,
                      NumericMatrix Q,
                      NumericVector xi,
                      NumericMatrix P,
                      NumericMatrix H,
                      List data) {
  int n = alpha.length();
  int m = 5;
  
  auto tdat = as<NumericVector>(data["time"]);
  auto gdat = as<IntegerVector>(data["counts"]);
  auto idat = as<IntegerVector>(data["indicators"]);
  int glast = as<int>(data["last"]);
  double maxtime = as<double>(data["maxtime"]);
  auto eres = GPHEres<NumericVector, NumericMatrix>(
    NumericVector(n),
    NumericVector(n),
    NumericVector(n),
    H);
  auto options = EMOptions();
  options.poisson_eps = 1.0e-8;
  options.ufactor = 1.01;
  double llf;
  auto dat = PHGroupSample<NumericVector,IntegerVector,IntegerVector>(tdat, gdat, idat, maxtime, glast);

  IntegerVector di(n);
  diag(Q, di);
  copy(Q, P);
  double qv = unif(P, di, 1.01);
  auto model = GPH<NumericVector, NumericMatrix, IntegerVector>(alpha, Q, P, xi, qv, di);
  
  auto work = GPHWorkSpace2(m, n);
  llf = estep(model, dat, eres, options, work);
  Rcout << llf << std::endl;
  Rcout << eres.eb << std::endl;
  Rcout << sum(eres.eb) << std::endl;
  Rcout << eres.ey << std::endl;
  Rcout << sum(eres.ey) << std::endl;
  Rcout << eres.ez << std::endl;
  Rcout << sum(eres.ez) << std::endl;
  Rcout << eres.en << std::endl;
  mstep(eres, model, options);
  llf = estep(model, dat, eres, options, work);
  Rcout << llf << std::endl;
}

// [[Rcpp::export]]
void test_estep_group_csc(NumericVector alpha,
                      S4 Q0,
                      NumericVector xi,
                      S4 P0,
                      S4 H0,
                      List data) {
  int n = alpha.length();
  int m = 5;
  
  using SparseT = S4matrix<CSCMatrixT>;
  auto Q = SparseT(Q0);
  auto P = SparseT(P0);
  auto H = SparseT(H0);
  
  auto tdat = as<NumericVector>(data["time"]);
  auto gdat = as<IntegerVector>(data["counts"]);
  auto idat = as<IntegerVector>(data["indicators"]);
  int glast = as<int>(data["last"]);
  double maxtime = as<double>(data["maxtime"]);
  auto eres = GPHEres<NumericVector, SparseT>(
    NumericVector(n),
    NumericVector(n),
    NumericVector(n),
    H);
  auto options = EMOptions();
  options.poisson_eps = 1.0e-8;
  options.ufactor = 1.01;
  double llf;
  auto dat = PHGroupSample<NumericVector,IntegerVector,IntegerVector>(tdat, gdat, idat, maxtime, glast);
  
  IntegerVector di(n);
  diag(Q, di);
  copy(Q, P);
  double qv = unif(P, di, 1.01);
  auto model = GPH<NumericVector, SparseT, IntegerVector>(alpha, Q, P, xi, qv, di);
  
  auto work = GPHWorkSpace2(m, n);
  llf = estep(model, dat, eres, options, work);
  Rcout << llf << std::endl;
  Rcout << eres.eb << std::endl;
  Rcout << sum(eres.eb) << std::endl;
  Rcout << eres.ey << std::endl;
  Rcout << sum(eres.ey) << std::endl;
  Rcout << eres.ez << std::endl;
  Rcout << sum(eres.ez) << std::endl;
  // Rcout << eres.en << std::endl;
  mstep(eres, model, options);
  llf = estep(model, dat, eres, options, work);
  Rcout << llf << std::endl;
}

// [[Rcpp::export]]
void test_estep_group_csr(NumericVector alpha,
                          S4 Q0,
                          NumericVector xi,
                          S4 P0,
                          S4 H0,
                          List data) {
  int n = alpha.length();
  int m = 5;
  
  using SparseT = S4matrix<CSRMatrixT>;
  auto Q = SparseT(Q0);
  auto P = SparseT(P0);
  auto H = SparseT(H0);
  
  auto tdat = as<NumericVector>(data["time"]);
  auto gdat = as<IntegerVector>(data["counts"]);
  auto idat = as<IntegerVector>(data["indicators"]);
  int glast = as<int>(data["last"]);
  double maxtime = as<double>(data["maxtime"]);
  auto eres = GPHEres<NumericVector, SparseT>(
    NumericVector(n),
    NumericVector(n),
    NumericVector(n),
    H);
  auto options = EMOptions();
  options.poisson_eps = 1.0e-8;
  options.ufactor = 1.01;
  double llf;
  auto dat = PHGroupSample<NumericVector,IntegerVector,IntegerVector>(tdat, gdat, idat, maxtime, glast);
  
  IntegerVector di(n);
  diag(Q, di);
  copy(Q, P);
  double qv = unif(P, di, 1.01);
  auto model = GPH<NumericVector, SparseT, IntegerVector>(alpha, Q, P, xi, qv, di);
  
  auto work = GPHWorkSpace2(m, n);
  llf = estep(model, dat, eres, options, work);
  Rcout << llf << std::endl;
  Rcout << eres.eb << std::endl;
  Rcout << sum(eres.eb) << std::endl;
  Rcout << eres.ey << std::endl;
  Rcout << sum(eres.ey) << std::endl;
  Rcout << eres.ez << std::endl;
  Rcout << sum(eres.ez) << std::endl;
  // Rcout << eres.en << std::endl;
  mstep(eres, model, options);
  llf = estep(model, dat, eres, options, work);
  Rcout << llf << std::endl;
}

// [[Rcpp::export]]
void test_estep_group_coo(NumericVector alpha,
                          S4 Q0,
                          NumericVector xi,
                          S4 P0,
                          S4 H0,
                          List data) {
  int n = alpha.length();
  int m = 5;
  
  using SparseT = S4matrix<COOMatrixT>;
  auto Q = SparseT(Q0);
  auto P = SparseT(P0);
  auto H = SparseT(H0);
  
  auto tdat = as<NumericVector>(data["time"]);
  auto gdat = as<IntegerVector>(data["counts"]);
  auto idat = as<IntegerVector>(data["indicators"]);
  int glast = as<int>(data["last"]);
  double maxtime = as<double>(data["maxtime"]);
  auto eres = GPHEres<NumericVector, SparseT>(
    NumericVector(n),
    NumericVector(n),
    NumericVector(n),
    H);
  auto options = EMOptions();
  options.poisson_eps = 1.0e-8;
  options.ufactor = 1.01;
  double llf;
  auto dat = PHGroupSample<NumericVector,IntegerVector,IntegerVector>(tdat, gdat, idat, maxtime, glast);
  
  IntegerVector di(n);
  diag(Q, di);
  copy(Q, P);
  double qv = unif(P, di, 1.01);
  auto model = GPH<NumericVector, SparseT, IntegerVector>(alpha, Q, P, xi, qv, di);
  
  auto work = GPHWorkSpace2(m, n);
  llf = estep(model, dat, eres, options, work);
  Rcout << llf << std::endl;
  Rcout << eres.eb << std::endl;
  Rcout << sum(eres.eb) << std::endl;
  Rcout << eres.ey << std::endl;
  Rcout << sum(eres.ey) << std::endl;
  Rcout << eres.ez << std::endl;
  Rcout << sum(eres.ez) << std::endl;
  // Rcout << eres.en << std::endl;
  mstep(eres, model, options);
  llf = estep(model, dat, eres, options, work);
  Rcout << llf << std::endl;
}

// [[Rcpp::export]]
void test_estep_group_poi(
    double omega,
    NumericVector alpha,
                      NumericMatrix Q,
                      NumericVector xi,
                      NumericMatrix P,
                      NumericMatrix H,
                      List data) {
  int n = alpha.length();
  int m = 5;
  
  auto tdat = as<NumericVector>(data["time"]);
  auto gdat = as<IntegerVector>(data["counts"]);
  auto idat = as<IntegerVector>(data["indicators"]);
  int glast = as<int>(data["last"]);
  double maxtime = as<double>(data["maxtime"]);
  auto eres = GPHEres<NumericVector, NumericMatrix>(
    NumericVector(n),
    NumericVector(n),
    NumericVector(n),
    H);
  auto options = EMOptions();
  options.poisson_eps = 1.0e-8;
  options.ufactor = 1.01;
  double llf;
  auto dat = PHGroupSample<NumericVector,IntegerVector,IntegerVector>(tdat, gdat, idat, maxtime, glast);
  
  IntegerVector di(n);
  diag(Q, di);
  copy(Q, P);
  double qv = unif(P, di, 1.01);
  auto gph = GPH<NumericVector, NumericMatrix, IntegerVector>(alpha, Q, P, xi, qv, di);
  auto model = GPHPoi<GPH<NumericVector, NumericMatrix, IntegerVector>>(gph, omega);
  
  auto work = GPHWorkSpace2(m, n);
  llf = estep(model, dat, eres, options, work);
  Rcout << llf << std::endl;
  Rcout << eres.eb << std::endl;
  Rcout << sum(eres.eb) << std::endl;
  Rcout << eres.ey << std::endl;
  Rcout << sum(eres.ey) << std::endl;
  Rcout << eres.ez << std::endl;
  Rcout << sum(eres.ez) << std::endl;
  Rcout << eres.en << std::endl;
  mstep(eres, model, options);
  llf = estep(model, dat, eres, options, work);
  Rcout << llf << std::endl;
}

/*** R
alpha <- c(0.2, 0.6, 0.2)
Q <- rbind(
  c(-2.0, 1.0, 0.0),
  c(2.0, -5.0, 1.0),
  c(3.0, 2.0, -8.0))
xi <- c(1.0, 2.0, 3.0)
dat <- list(time=c(1,2,1,3,4), weights=c(1,1,1,1,1), maxtime=4)
test_estep_wtime(alpha, Q, xi, dat)
print(alpha)
print(Q)
print(xi)

alpha <- c(0.2, 0.6, 0.2)
Q <- rbind(
  c(-2.0, 1.0, 0.0),
  c(2.0, -5.0, 1.0),
  c(3.0, 2.0, -8.0))
xi <- c(1.0, 2.0, 3.0)
dat <- list(time=c(1,2,1,3,4), weights=c(1,1,1,1,1), maxtime=4)
test_estep_wtime_csc(alpha, as(Q, "dgCMatrix"), xi, as(Q, "dgCMatrix"), as(Q, "dgCMatrix"), dat)

alpha <- c(0.2, 0.6, 0.2)
Q <- rbind(
  c(-2.0, 1.0, 0.0),
  c(2.0, -5.0, 1.0),
  c(3.0, 2.0, -8.0))
xi <- c(1.0, 2.0, 3.0)
dat <- list(time=c(1,2,1,3,4), weights=c(1,1,1,1,1), maxtime=4)
test_estep_wtime_csr(alpha, as(Q, "dgRMatrix"), xi, as(Q, "dgRMatrix"), as(Q, "dgRMatrix"), dat)

alpha <- c(0.2, 0.6, 0.2)
Q <- rbind(
  c(-2.0, 1.0, 0.0),
  c(2.0, -5.0, 1.0),
  c(3.0, 2.0, -8.0))
xi <- c(1.0, 2.0, 3.0)
dat <- list(time=c(1,2,1,3,4), weights=c(1,1,1,1,1), maxtime=4)
test_estep_wtime_coo(alpha, as(Q, "dgTMatrix"), xi, as(Q, "dgTMatrix"), as(Q, "dgTMatrix"), dat)

alpha <- c(0.2, 0.6, 0.2)
Q <- rbind(
  c(-2.0, 1.0, 0.0),
  c(2.0, -5.0, 1.0),
  c(3.0, 2.0, -8.0))
P <- matrix(0, 3, 3)
H <- matrix(0, 3, 3)
xi <- c(1.0, 2.0, 3.0)
dat <- list(time=c(1,2,1,3,4), counts=c(1,3,-1,2,4), indicators=c(0,0,0,1,0), last=10, maxtime=4)
test_estep_group(alpha, Q, xi, P, H, dat)
print(alpha)
print(Q)
print(xi)

alpha <- c(0.2, 0.6, 0.2)
Q0 <- rbind(
  c(-2.0, 1.0, 0.0),
  c(2.0, -5.0, 1.0),
  c(3.0, 2.0, -8.0))
Q <- as(Q0, "dgCMatrix")
P <- as(Q0, "dgCMatrix")
H <- as(Q0, "dgCMatrix")
xi <- c(1.0, 2.0, 3.0)
dat <- list(time=c(1,2,1,3,4), counts=c(1,3,-1,2,4), indicators=c(0,0,0,1,0), last=10, maxtime=4)
test_estep_group_csc(alpha, Q, xi, P, H, dat)
print(alpha)
print(Q)
print(xi)

alpha <- c(0.2, 0.6, 0.2)
Q0 <- rbind(
  c(-2.0, 1.0, 0.0),
  c(2.0, -5.0, 1.0),
  c(3.0, 2.0, -8.0))
Q <- as(Q0, "dgRMatrix")
P <- as(Q0, "dgRMatrix")
H <- as(Q0, "dgRMatrix")
xi <- c(1.0, 2.0, 3.0)
dat <- list(time=c(1,2,1,3,4), counts=c(1,3,-1,2,4), indicators=c(0,0,0,1,0), last=10, maxtime=4)
test_estep_group_csr(alpha, Q, xi, P, H, dat)
print(alpha)
print(Q)
print(xi)
  
alpha <- c(0.2, 0.6, 0.2)
Q0 <- rbind(
  c(-2.0, 1.0, 0.0),
  c(2.0, -5.0, 1.0),
  c(3.0, 2.0, -8.0))
Q <- as(Q0, "dgTMatrix")
P <- as(Q0, "dgTMatrix")
H <- as(Q0, "dgTMatrix")
xi <- c(1.0, 2.0, 3.0)
dat <- list(time=c(1,2,1,3,4), counts=c(1,3,-1,2,4), indicators=c(0,0,0,1,0), last=10, maxtime=4)
test_estep_group_coo(alpha, Q, xi, P, H, dat)
print(alpha)
print(Q)
print(xi)

alpha <- c(0.2, 0.6, 0.2)
Q <- rbind(
  c(-2.0, 1.0, 0.0),
  c(2.0, -5.0, 1.0),
  c(3.0, 2.0, -8.0))
omega <- 10
P <- matrix(0, 3, 3)
H <- matrix(0, 3, 3)
xi <- c(1.0, 2.0, 3.0)
dat <- list(time=c(1,2,1,3,4), counts=c(1,3,-1,2,4), indicators=c(0,0,0,1,0), last=10, maxtime=4)
test_estep_group_poi(omega, alpha, Q, xi, P, H, dat)
print(alpha)
print(Q)
print(xi)
*/
