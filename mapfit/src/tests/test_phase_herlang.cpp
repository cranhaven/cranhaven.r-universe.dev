#include <Rcpp.h>
using namespace Rcpp;

#include "../phase_herlang.h"

// [[Rcpp::export]]
void test_estep_wtime(NumericVector alpha,
                      IntegerVector shape,
                      NumericVector rate,
                      List data) {
  int n = 2;
  int m = 5;
  HErlangWorkSpace1 work(m, n);
  auto tdat = as<NumericVector>(data["time"]);
  auto wdat = as<NumericVector>(data["weights"]);
  double maxtime = 10.0;
  auto eres = HErlangEres<std::vector<double>>(std::vector<double>(n), std::vector<double>(n));
  double llf;
  auto dat = PHWeightSample<NumericVector,NumericVector>(tdat, wdat, maxtime);
  auto model = HErlang<NumericVector, IntegerVector>(alpha, shape, rate);
  llf = estep(model, dat, eres, nullptr, work);
  Rcout << llf << std::endl;
  mstep(eres, model, nullptr);
  llf = estep(model, dat, eres, nullptr, work);
  Rcout << llf << std::endl;
}

// [[Rcpp::export]]
void test_estep_group(NumericVector alpha,
                      IntegerVector shape,
                      NumericVector rate,
                      List data) {
  int n = 2;
  int m = 5;
  // NumericMatrix pool0(n, m);
  // NumericMatrix pool1(n, m);
  // NumericMatrix cool0(n, m+2);
  // NumericMatrix cool1(n, m+2);
  HErlangWorkSpace2 work(m, n);
  // for (int k=0; k<m; k++) {
  //   work.perl0[k] = Vec<double>(n, &pool0(0, k));
  //   work.perl1[k] = Vec<double>(n, &pool1(0, k));
  // }
  // for (int k=0; k<m+2; k++) {
  //   work.cerl0[k] = Vec<double>(n, &cool0(0, k));
  //   work.cerl1[k] = Vec<double>(n, &cool1(0, k));
  // }
  auto tdat = as<NumericVector>(data["time"]);
  auto gdat = as<IntegerVector>(data["counts"]);
  auto idat = as<IntegerVector>(data["indicators"]);
  int glast = as<int>(data["last"]);
  double maxtime = 10.0;
  auto eres = HErlangEres<std::vector<double>>(std::vector<double>(n), std::vector<double>(n));
  double llf;
  auto dat = PHGroupSample<NumericVector,IntegerVector,IntegerVector>(tdat, gdat, idat, maxtime, glast);
  auto model = HErlang<NumericVector, IntegerVector>(alpha, shape, rate);
  llf = estep(model, dat, eres, nullptr, work);
  Rcout << llf << std::endl;
  mstep(eres, model, nullptr);
  llf = estep(model, dat, eres, nullptr, work);
  Rcout << llf << std::endl;
}

// [[Rcpp::export]]
void test_estep_group_poi(NumericVector alpha,
                      IntegerVector shape,
                      NumericVector rate,
                      double omega,
                      List data) {
  int n = 2;
  int m = 5;
  // NumericMatrix pool0(n, m);
  // NumericMatrix pool1(n, m);
  // NumericMatrix cool0(n, m+2);
  // NumericMatrix cool1(n, m+2);
  HErlangWorkSpace2 work(m, n);
  // for (int k=0; k<m; k++) {
  //   work.perl0[k] = Vec<double>(n, &pool0(0, k));
  //   work.perl1[k] = Vec<double>(n, &pool1(0, k));
  // }
  // for (int k=0; k<m+2; k++) {
  //   work.cerl0[k] = Vec<double>(n, &cool0(0, k));
  //   work.cerl1[k] = Vec<double>(n, &cool1(0, k));
  // }
  auto tdat = as<NumericVector>(data["time"]);
  auto gdat = as<IntegerVector>(data["counts"]);
  auto idat = as<IntegerVector>(data["indicators"]);
  int glast = as<int>(data["last"]);
  double maxtime = 10.0;
  auto dat = PHGroupSample<NumericVector,IntegerVector,IntegerVector>(tdat, gdat, idat, maxtime, glast);
  auto eres = HErlangEres<std::vector<double>>(std::vector<double>(n), std::vector<double>(n));
  double llf;
  auto model = HErlangPoi<NumericVector, IntegerVector>(alpha, shape, rate, omega);
  llf = estep(model, dat, eres, nullptr, work);
  Rcout << llf << std::endl;
  mstep(eres, model, nullptr);
  llf = estep(model, dat, eres, nullptr, work);
  Rcout << llf << std::endl;
}
 
/*** R
alpha <- c(0.4, 0.6)
rate <- c(1.0, 2.0)
shape <- c(1, 2)
dat <- list(size=5, time=c(1,2,1,3,4), weights=c(1,3,4,2,4))
test_estep_wtime(alpha, shape, rate, dat)

alpha <- c(0.4, 0.6)
rate <- c(1.0, 2.0)
shape <- c(1, 2)
dat <- list(size=5, time=c(1,2,1,3,4), counts=c(1,3,-1,2,4), indicators=c(0,0,0,1,0))
dat <- c(dat, list(last=10))
test_estep_group(alpha, shape, rate, dat)

alpha <- c(0.4, 0.6)
rate <- c(1.0, 2.0)
shape <- c(1, 2)
dat <- list(size=5, time=c(1,2,1,3,4), counts=c(1,3,-1,2,4), indicators=c(0,0,0,1,0), last=10)
test_estep_group_poi(alpha, shape, rate, 10.0, dat)
print(dat)
*/
