#include <Rcpp.h>
using namespace Rcpp;

#include "phase_gen.h"
#include "phase_cf1.h"
#include "poisson.h"
#include "blas.h"

// [[Rcpp::export]]
NumericVector phase_dist_pdf(
    NumericVector dt,
    double maxtime,
    NumericVector alpha,
    S4 Q0,
    NumericVector xi,
    double eps,
    double ufactor,
    S4 P0) {
  using MatrixT = S4matrix<CSCMatrixT>;
  auto Q = MatrixT(Q0);
  auto P = MatrixT(P0);

  int m = dt.size();
  NumericVector y(m);

  int n = alpha.length();
  copy(Q, P);
  double qv = unif(P, ufactor);

  int right = poi::rightbound(qv*maxtime, eps);
  std::vector<double> prob(right+1);
  std::vector<double> x(n);
  std::vector<double> vf(n);
  std::vector<double> tmpv(n);

  copy(alpha, vf);
  for (int k=0; k<m; k++) {
    int right = poi::rightbound(qv*dt[k], eps);
    double weight = poi::pmf(qv*dt[k], 0, right, prob);

    copy(vf, x);
    fill(vf, 0.0);
    axpy(prob[0], x, vf);
    for (int u=1; u<=right; u++) {
      gemv(TRANS{}, 1.0, P, x, 0.0, tmpv);
      copy(tmpv, x);
      axpy(prob[u], x, vf);
    }
    scal(1.0/weight, vf);
    y[k] = dot(vf, xi);
  }

  return y;
}

// [[Rcpp::export]]
NumericVector phase_dist_ccdf(
    NumericVector dt,
    double maxtime,
    NumericVector alpha,
    S4 Q0,
    double eps,
    double ufactor,
    S4 P0) {
  using MatrixT = S4matrix<CSCMatrixT>;
  auto Q = MatrixT(Q0);
  auto P = MatrixT(P0);
  
  int m = dt.size();
  NumericVector y(m);
  
  int n = alpha.length();
  copy(Q, P);
  double qv = unif(P, ufactor);
  
  int right = poi::rightbound(qv*maxtime, eps);
  std::vector<double> prob(right+1);
  std::vector<double> x(n);
  std::vector<double> vf(n);
  std::vector<double> tmpv(n);
  
  copy(alpha, vf);
  for (int k=0; k<m; k++) {
    int right = poi::rightbound(qv*dt[k], eps);
    double weight = poi::pmf(qv*dt[k], 0, right, prob);
    
    copy(vf, x);
    fill(vf, 0.0);
    axpy(prob[0], x, vf);
    for (int u=1; u<=right; u++) {
      gemv(TRANS{}, 1.0, P, x, 0.0, tmpv);
      copy(tmpv, x);
      axpy(prob[u], x, vf);
    }
    scal(1.0/weight, vf);
    y[k] = asum(vf);
  }
  
  return y;
}

// [[Rcpp::export]]
int phase_cf1_sort(
    NumericVector alpha,
    NumericVector rate) {
  cf1sort(alpha, rate);
  return alpha.length();
}

/*** R
alpha <- c(0.2, 0.6, 0.2)
Q <- rbind(
  c(-2.0, 1.0, 0.0),
  c(2.0, -5.0, 1.0),
  c(3.0, 2.0, -8.0))
xi <- c(1.0, 2.0, 3.0)
time <- c(1,2,3,4,5,6)
dt <- c(time[1], diff(time))
eps <- 1.0e-8
ufactor <- 1.01
result <- phase_dist_pdf(
  dt, max(dt),
  alpha, as(Q, "dgCMatrix"), xi,
  eps, ufactor, as(Q, "dgCMatrix"))
print(result)
result <- phase_dist_ccdf(
  dt, max(dt),
  alpha, as(Q, "dgCMatrix"),
  eps, ufactor, as(Q, "dgCMatrix"))
print(result)

*/

