#include <Rcpp.h>
using namespace Rcpp;

#include "map_erhmm.h"
#include "emfit.h"

// [[Rcpp::export]]
List emfit_erhmm_time(
    NumericVector alpha,
    NumericVector xi,
    NumericVector rate,
    IntegerVector shape,
    S4 P0,
    List data,
    List options,
    S4 H0) {
  using MatrixT = S4matrix<DenseMatrixT>;
  auto P = MatrixT(P0);
  auto H = MatrixT(H0);
  
  auto maxiter = as<int>(options["maxiter"]);
  auto atol = as<double>(options["abstol"]);
  auto rtol = as<double>(options["reltol"]);
  auto verbose = as<bool>(options["em.verbose"]);
  auto steps = as<int>(options["steps"]);
  auto stationary = as<bool>(options["map.stationary"]);

  int n = alpha.length();
  auto model = ErlangHMM<NumericVector, IntegerVector, MatrixT>(alpha, xi, rate, shape, P);

  auto tdat = as<NumericVector>(data["intervals"]);
  double maxtime = as<double>(data["maxinterval"]);
  auto m = tdat.length();
  auto dat = MAPTimeSample<NumericVector>(tdat, maxtime);

  auto eres = ErlangHMMEres<NumericVector,MatrixT>(
    NumericVector(n),
    NumericVector(n),
    NumericVector(n),
    H);
  auto work = ErlangHMMWorkSpace1<MatrixT>(m, n);
  
  auto opts = EMOptions();
  opts.maxiter = maxiter;
  opts.atol = atol;
  opts.rtol = rtol;
  opts.steps = steps;
  opts.verbose = verbose;
  opts.stationary = stationary;

  emfit(model, dat, opts, eres, work);

  return List::create(
    Named("alpha") = alpha,
    Named("xi") = xi,
    Named("rate") = rate,
    Named("shape") = shape,
    Named("P") = P0,
    Named("iter") = opts.iter,
    Named("aerror") = opts.aerror,
    Named("rerror") = opts.rerror,
    Named("llf") = opts.llf,
    Named("convergence") = opts.status == Convergence);
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
dat <- list(intervals=c(1,2,1,3,4), maxinterval=4)

options <- list(maxiter=10, abstol=1.0e-3, reltol=1.0e-6,
                steps=1, em.verbose=TRUE, map.stationary=TRUE)
result <- emfit_erhmm_time(alpha, xi, rate, shape, as(P, "dgeMatrix"), dat, options, as(P, "dgeMatrix"))
print(result)
*/

