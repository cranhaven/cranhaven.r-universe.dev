#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
double PredPCpp(int x, int n, int nmax, double a, double b, double p0, double theta_t){

  double prob = 0.0;
  double eps = std::numeric_limits<double>::epsilon();
  double pxy;

  for (int y = 0; y < nmax - n + 1; y++) {
    pxy = (1.0 - R::pbeta(p0, a + y + x, b + nmax - y - x, 1, 0));
    if (pxy > theta_t || std::abs(pxy - theta_t) < eps) {
      prob += exp(
        R::lchoose(nmax - n, y) +
          R::lbeta(a + y + x, b + nmax - y - x) -
          R::lbeta(a + x, b + n - x)
      );
    }
  }
  return prob;
}
