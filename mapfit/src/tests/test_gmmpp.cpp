#include "../map_gmmpp.h"
#include "../emfit.h"

using namespace Rcpp;

// [[Rcpp::export]]
double test_xifunc(int n, double t, double u, double mi, double mj, double ri, double rj) {
  return xifunc0(n, t, u, mi, mj, ri, rj);
}

// [[Rcpp::export]]
double test_inte(int n, double t, double mi, double mj, double ri, double rj) {
  int divide = 10;
  std::vector<double> x(divide);
  std::vector<double> w(divide);
  std::vector<double> fx(divide);
  std::vector<double> fv(divide);

  gauss_inte::w(x, w, 1.0e-8);
  double s0 = gam_inte(n, t, mi, mj, ri, rj, x, w, fx, fv);
  double s1 = psi_inte(n, t, mi, mj, ri, rj, x, w, fx, fv);
  return s1;
}

// [[Rcpp::export]]
void test_makeG(int n, double t, NumericMatrix D0, NumericMatrix D1, NumericMatrix G) {
  int divide = 10;
  std::vector<double> x(divide);
  std::vector<double> w(divide);
  std::vector<double> fx(divide);
  std::vector<double> fv(divide);
  
  gauss_inte::w(x, w, 1.0e-8);
  makeG(n, t, D0, D1, G, x, w, fx, fv);
}

// [[Rcpp::export]]
void test_estep_group(NumericVector alpha,
                      NumericVector xi,
                      NumericMatrix D0,
                      NumericMatrix D1,
                      NumericMatrix P0,
                      NumericMatrix P1,
                      NumericMatrix en0,
                      NumericMatrix en1,
                      NumericMatrix G,
                      NumericMatrix PsiT1,
                      NumericMatrix PsiT2,
                      NumericMatrix PsiN1,
                      NumericMatrix PsiN2,
                      NumericMatrix tmpm,
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
  options.inte_divide = 50;
  options.inte_eps = 1.0e-8;
  
  IntegerVector di(n);
  diag(D0, di);
  copy(D0, P0);
  copy(D1, P1);
  double qv = unif(P0, di, options.ufactor);
  scal(1.0/qv, P1);
  auto map = MAP<NumericVector, NumericMatrix, IntegerVector>(alpha, xi, D0, D1, P0, P1, di, qv);
  auto model = GMMPP<MAP<NumericVector, NumericMatrix, IntegerVector>>(map);
  auto work = GMMPPWorkSpace<NumericMatrix>(G, PsiT1, PsiT2, PsiN1, PsiN2, tmpm);
  llf = estep(model, dat, eres, options, work);
  Rcout << "llf=" << llf << std::endl;
  Rcout << eres.eb << std::endl;
  Rcout << sum(eres.eb) << std::endl;
  Rcout << eres.ez << std::endl;
  Rcout << sum(eres.ez) << std::endl;
  Rcout << eres.en0 << std::endl;
  Rcout << eres.en1 << std::endl;
  mstep(eres, model, options);
  llf = estep(model, dat, eres, options, work);
  mstep(eres, model, options);
  Rcout << "llf=" << llf << std::endl;
  llf = estep(model, dat, eres, options, work);
  mstep(eres, model, options);
  Rcout << "llf=" << llf << std::endl;
}

/*** R
print(test_xifunc(1, 10, 1, 1.0, 2.0, 3.0, 1.0))
print(test_inte(1, 10, 1.0, 2.0, 3.0, 1.0))

D0 <- rbind(
  c(-10.0, 4.0),
  c(2.0, -4.0))
D1 <- rbind(
  c(6.0, 0.0),
  c(0.0, 2.0))
G <- matrix(0.0, 2, 2)

test_makeG(1, 10.0, D0, D1, G)
print(G)

alpha <- c(0.4, 0.6)
xi <- c(1,1)
D0 <- rbind(
  c(-10.0, 4.0),
  c(2.0, -4.0))
D1 <- rbind(
  c(6.0, 0.0),
  c(0.0, 2.0))
P0 <- matrix(0.0, 2, 2)
P1 <- matrix(0.0, 2, 2)
en0 <- matrix(0.0, 2, 2)
en1 <- matrix(0.0, 2, 2)
G <- matrix(0.0, 2, 2)
Psi1T <- matrix(0.0, 2, 2)
Psi2T <- matrix(0.0, 2, 2)
Psi1N <- matrix(0.0, 2, 2)
Psi2N <- matrix(0.0, 2, 2)
tmpm <- matrix(0.0, 2, 2)
dat <- list(time=c(1,2,1,1,1), counts=c(1,0,0,2,1), indicators=c(0,0,0,0,0), maxtime=4, maxcount=4)
test_estep_group(alpha, xi, D0, D1, P0, P1, en0, en1, G, Psi1T, Psi2T, Psi1N, Psi2N, tmpm, dat)


*/
