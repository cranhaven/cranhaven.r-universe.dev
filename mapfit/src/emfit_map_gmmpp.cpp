#include <Rcpp.h>
using namespace Rcpp;

#include "map_gmmpp.h"
#include "emfit.h"

// [[Rcpp::export]]
List emfit_gmmpp_group(
    NumericVector alpha,
    NumericVector xi,
    S4 xD0,
    S4 xD1,
    List data,
    List options,
    S4 xP0,
    S4 xP1,
    S4 xen0,
    S4 xen1,
    S4 xG,
    S4 xPsiT1,
    S4 xPsiT2,
    S4 xPsiN1,
    S4 xPsiN2,
    S4 xtmpm) {
  using MatrixT = S4matrix<DenseMatrixT>;
  auto D0 = MatrixT(xD0);
  auto D1 = MatrixT(xD1);
  auto P0 = MatrixT(xP0);
  auto P1 = MatrixT(xP1);
  auto en0 = MatrixT(xen0);
  auto en1 = MatrixT(xen1);
  auto G = MatrixT(xG);
  auto PsiT1 = MatrixT(xPsiT1);
  auto PsiT2 = MatrixT(xPsiT2);
  auto PsiN1 = MatrixT(xPsiN1);
  auto PsiN2 = MatrixT(xPsiN2);
  auto tmpm = MatrixT(xtmpm);

  auto maxiter = as<int>(options["maxiter"]);
  auto atol = as<double>(options["abstol"]);
  auto rtol = as<double>(options["reltol"]);
  auto verbose = as<bool>(options["em.verbose"]);
  auto steps = as<int>(options["steps"]);
  auto ufactor = as<double>(options["uniform.factor"]);
  auto inte_divide = as<int>(options["inte.divide"]);
  auto inte_eps = as<double>(options["inte.eps"]);

  int n = alpha.length();
  IntegerVector di(n);
  diag(D0, di);
  copy(D0, P0);
  copy(D1, P1);
  double qv = unif(P0, di, ufactor);
  scal(1.0/qv, P1);
  auto map = MAP<NumericVector,MatrixT,IntegerVector>(alpha, xi, D0, D1, P0, P1, di, qv);
  auto model = GMMPP<MAP<NumericVector,MatrixT,IntegerVector>>(map);

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
  auto work = GMMPPWorkSpace<MatrixT>(G, PsiT1, PsiT2, PsiN1, PsiN2, tmpm);

  auto opts = EMOptions();
  opts.maxiter = maxiter;
  opts.atol = atol;
  opts.rtol = rtol;
  opts.steps = steps;
  opts.verbose = verbose;
  opts.ufactor = ufactor;
  opts.inte_divide = inte_divide;
  opts.inte_eps = inte_eps;

  emfit(model, dat, opts, eres, work);
  
  return List::create(
    Named("alpha") = alpha,
    Named("xi") = xi,
    Named("D0") = xD0,
    Named("D1") = xD1,
    Named("iter") = opts.iter,
    Named("aerror") = opts.aerror,
    Named("rerror") = opts.rerror,
    Named("llf") = opts.llf,
    Named("convergence") = opts.status == Convergence);
}

/*** R
alpha <- c(0.4, 0.6)
xi <- c(1,1)
D0 <- as(rbind(
  c(-10.0, 4.0),
  c(2.0, -4.0)), "dgeMatrix")
D1 <- as(rbind(
  c(6.0, 0.0),
  c(0.0, 2.0)), "dgeMatrix")
P0 <- as(matrix(0.0, 2, 2), "dgeMatrix")
P1 <- as(matrix(0.0, 2, 2), "dgeMatrix")
en0 <- as(matrix(0.0, 2, 2), "dgeMatrix")
en1 <- as(matrix(0.0, 2, 2), "dgeMatrix")
G <- as(matrix(0.0, 2, 2), "dgeMatrix")
Psi1T <- as(matrix(0.0, 2, 2), "dgeMatrix")
Psi2T <- as(matrix(0.0, 2, 2), "dgeMatrix")
Psi1N <- as(matrix(0.0, 2, 2), "dgeMatrix")
Psi2N <- as(matrix(0.0, 2, 2), "dgeMatrix")
tmpm <- as(matrix(0.0, 2, 2), "dgeMatrix")
dat <- list(intervals=c(1,2,1,1,1), counts=c(1,0,0,2,1), instants=c(0,0,0,0,0), maxinterval=2, maxcount=2)
options <- list(maxiter=10, abstol=1.0e-3, reltol=1.0e-6,
                steps=1, em.verbose=TRUE, uniform.factor=1.01,
                poisson.eps=1.0e-8, inte.divide=100, inte.eps=1.0e-8)
result <- emfit_gmmpp_group(alpha, xi, D0, D1, dat, options,
                             P0, P1, en0, en1, G, Psi1T, Psi2T, Psi1N, Psi2N, tmpm)
print(result)
*/

