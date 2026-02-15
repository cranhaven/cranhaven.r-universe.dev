#include <Rcpp.h>
using namespace Rcpp;

#include "map_gen.h"
#include "emfit.h"

// [[Rcpp::export]]
List emfit_mapgen_group(
    NumericVector alpha,
    NumericVector xi,
    S4 D00,
    S4 D10,
    List data,
    List options,
    S4 P00,
    S4 P10,
    S4 H00,
    S4 H10,
    S4 en00,
    S4 en10) {
  using MatrixT = S4matrix<DenseMatrixT>;
  auto D0 = MatrixT(D00);
  auto D1 = MatrixT(D10);
  auto P0 = MatrixT(P00);
  auto P1 = MatrixT(P10);
  auto H0 = MatrixT(H00);
  auto H1 = MatrixT(H10);
  auto en0 = MatrixT(en00);
  auto en1 = MatrixT(en10);
  
  auto maxiter = as<int>(options["maxiter"]);
  auto atol = as<double>(options["abstol"]);
  auto rtol = as<double>(options["reltol"]);
  auto verbose = as<bool>(options["em.verbose"]);
  auto steps = as<int>(options["steps"]);
  auto ufactor = as<double>(options["uniform.factor"]);
  auto eps = as<double>(options["poisson.eps"]);
  auto stationary = as<bool>(options["map.stationary"]);

  int n = alpha.length();
  IntegerVector di(n);
  diag(D0, di);
  copy(D0, P0);
  copy(D1, P1);
  double qv = unif(P0, di, ufactor);
  scal(1.0/qv, P1);
  auto model = MAP<NumericVector,MatrixT,IntegerVector>(alpha, xi, D0, D1, P0, P1, di, qv);

  auto tdat = as<NumericVector>(data["intervals"]);
  auto gdat = as<IntegerVector>(data["counts"]);
  auto idat = as<IntegerVector>(data["instants"]);
  double maxtime = as<double>(data["maxinterval"]);
  int maxcount = as<int>(data["maxcount"]);
  auto m = tdat.length();
  auto dat = MAPGroupSample<NumericVector,IntegerVector,IntegerVector>(tdat, gdat, idat, maxtime, maxcount);
  
  auto eres = MAPEres<NumericVector,MatrixT>(
    NumericVector(n),
    NumericVector(n),
    en0, en1);
  auto work = MAPWorkSpace1<MatrixT>(m, n, H0, H1);
  
  auto opts = EMOptions();
  opts.maxiter = maxiter;
  opts.atol = atol;
  opts.rtol = rtol;
  opts.steps = steps;
  opts.verbose = verbose;
  opts.ufactor = ufactor;
  opts.poisson_eps = eps;
  opts.stationary = stationary;
  
  emfit(model, dat, opts, eres, work);

  return List::create(
    Named("alpha") = alpha,
    Named("xi") = xi,
    Named("D0") = D00,
    Named("D1") = D10,
    Named("iter") = opts.iter,
    Named("aerror") = opts.aerror,
    Named("rerror") = opts.rerror,
    Named("llf") = opts.llf,
    Named("convergence") = opts.status == Convergence);
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
dat <- list(intervals=c(1,2,1,3,4), counts=c(1,3,0,2,4), instants=c(0,0,0,0,0), maxinterval=4, maxcount=4)
options <- list(maxiter=10, abstol=1.0e-3, reltol=1.0e-6,
                steps=1, em.verbose=TRUE, uniform.factor=1.01,
                poisson.eps=1.0e-8, map.stationary=TRUE)
result <- emfit_mapgen_group(alpha, xi, as(D0, "dgeMatrix"), as(D1, "dgeMatrix"), dat, options,
                             as(P0, "dgeMatrix"), as(P1, "dgeMatrix"),
                             as(H0, "dgeMatrix"), as(H1, "dgeMatrix"),
                             as(en0, "dgeMatrix"), as(en1, "dgeMatrix"))
print(result)
*/

