#include <Rcpp.h>
using namespace Rcpp;

#include "../map_erhmm.h"

// [[Rcpp::export]]
void test_estep_time(NumericVector alpha,
                     NumericVector xi,
                     NumericVector rate,
                     IntegerVector shape,
                     NumericMatrix P,
                     NumericMatrix H,
                     List data) {
  int n = alpha.length();
  int m = 5;
  auto tdat = as<NumericVector>(data["time"]);
  double maxtime = as<double>(data["maxtime"]);
  auto eres = ErlangHMMEres<std::vector<double>,NumericMatrix>(
    std::vector<double>(n),
    std::vector<double>(n),
    std::vector<double>(n),
    H);
  double llf;
  auto dat = MAPTimeSample<NumericVector>(tdat, maxtime);
  auto model = ErlangHMM<NumericVector, IntegerVector, NumericMatrix>(alpha, xi, rate, shape, P);
  auto work = std::vector<double>(n);
  llf = estep(model, dat, eres, nullptr, work);
  Rcout << llf << std::endl;
  mstep(eres, model, work);
  llf = estep(model, dat, eres, nullptr, work);
  Rcout << llf << std::endl;
}

// [[Rcpp::export]]
void test_estep_time_s4(NumericVector alpha,
                     NumericVector xi,
                     NumericVector rate,
                     IntegerVector shape,
                     S4 P0,
                     S4 H0,
                     List data) {
  using MatrixT = S4matrix<DenseMatrixT>;
  auto P = MatrixT(P0);
  auto H = MatrixT(H0);
  int n = alpha.length();
  int m = 5;
  auto tdat = as<NumericVector>(data["time"]);
  double maxtime = as<double>(data["maxtime"]);
  auto eres = ErlangHMMEres<std::vector<double>,MatrixT>(
    std::vector<double>(n),
    std::vector<double>(n),
    std::vector<double>(n),
    H);
  double llf;
  auto dat = MAPTimeSample<NumericVector>(tdat, maxtime);
  auto model = ErlangHMM<NumericVector, IntegerVector, MatrixT>(alpha, xi, rate, shape, P);
  auto work = std::vector<double>(n);
  llf = estep(model, dat, eres, nullptr, work);
  Rcout << llf << std::endl;
  mstep(eres, model, work);
  llf = estep(model, dat, eres, nullptr, work);
  Rcout << llf << std::endl;
}

/*** R
alpha <- c(0.4, 0.6)
rate <- c(1.0, 2.0)
shape <- c(1, 2)
xi <- c(1,1)
P <- cbind(
  c(0.4, 0.6),
  c(0.2, 0.8)
)
H <- matrix(0, 2, 2)
dat <- list(time=c(1,2,1,3,4), maxtime=4)
test_estep_time(alpha, xi, rate, shape, P, H, dat)

alpha <- c(0.4, 0.6)
rate <- c(1.0, 2.0)
shape <- c(1, 2)
xi <- c(1,1)
P <- cbind(
  c(0.4, 0.6),
  c(0.2, 0.8)
)
H <- matrix(0, 2, 2)
dat <- list(time=c(1,2,1,3,4), maxtime=4)
test_estep_time_s4(alpha, xi, rate, shape, as(P, "dgeMatrix"), as(H, "dgeMatrix"), dat)
*/
