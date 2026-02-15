#include <Rcpp.h>
using namespace Rcpp;

#include "../phase_herlang.h"
#include "../emfit.h"

// [[Rcpp::export]]
void test_em_herlang_wtime(NumericVector alpha,
                      IntegerVector shape,
                      NumericVector rate,
                      List data) {
  int n = 2;
  int m = 5;
  HErlangWorkSpace1 work(m, n);
  auto tdat = as<NumericVector>(data["time"]);
  auto wdat = as<NumericVector>(data["weights"]);
  double maxtime = as<double>(data["maxtime"]);
  auto eres = HErlangEres<std::vector<double>>(std::vector<double>(n), std::vector<double>(n));
  auto dat = PHWeightSample<NumericVector,NumericVector>(tdat, wdat, maxtime);
  auto model = HErlang<NumericVector, IntegerVector>(alpha, shape, rate);
  auto options = EMOptions();
  options.maxiter = 10;
  options.atol = 0.0;
  options.rtol = 0.0;
  options.steps = 5;
  options.verbose = true;
  emfit(model, dat, options, eres, work);
}

// [[Rcpp::export]]
void test_em_herlang_group(NumericVector alpha,
                           IntegerVector shape,
                           NumericVector rate,
                           List data) {
  int n = 2;
  int m = 5;
  HErlangWorkSpace2 work(m, n);
  auto tdat = as<NumericVector>(data["time"]);
  auto gdat = as<IntegerVector>(data["counts"]);
  auto idat = as<IntegerVector>(data["indicators"]);
  int glast = as<int>(data["last"]);
  double maxtime = as<double>(data["maxtime"]);
  auto dat = PHGroupSample<NumericVector,IntegerVector,IntegerVector>(tdat, gdat, idat, maxtime, glast);
  auto eres = HErlangEres<std::vector<double>>(std::vector<double>(n), std::vector<double>(n));
  auto model = HErlang<NumericVector, IntegerVector>(alpha, shape, rate);
  auto options = EMOptions();
  options.maxiter = 10;
  options.atol = 0.0;
  options.rtol = 0.0;
  options.steps = 5;
  options.verbose = true;
  emfit(model, dat, options, eres, work);
}

/*** R
alpha <- c(0.4, 0.6)
rate <- c(1.0, 2.0)
shape <- c(1, 2)
dat <- list(time=c(1,2,1,3,4), weights=c(1,3,4,2,4), maxtime=4)
test_em_herlang_wtime(alpha, shape, rate, dat)

alpha <- c(0.4, 0.6)
rate <- c(1.0, 2.0)
shape <- c(1, 2)
dat <- list(time=c(1,2,1,3,4), counts=c(1,3,-1,2,4), indicators=c(0,0,0,1,0), last=10, maxtime=4)
test_em_herlang_group(alpha, shape, rate, dat)
*/
