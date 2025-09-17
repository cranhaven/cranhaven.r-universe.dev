#include "glm_gmrf.h"

using namespace Rcpp;


void mcstat2::glm::gmrf_approx(double* b, double* c, const double* x0,
  const double* y, int n, const mcstat2::glm::glmfamily family) {

  switch(family) {
    case poisson: // exponential link function
      for(int i=0; i<n; i++) {
        double d2 = - std::exp(x0[i]);
        double d1 = y[i] + d2;
        b[i] = d1 - d2 * x0[i];
        c[i] = d2>0 ? 0 : - d2;
      }
      break;
  }
}


double mcstat2::glm::ll(const double* y, const double* eta, const int n,
    const mcstat2::glm::glmfamily family) {

    double res = 0;

    switch(family) {
        case poisson:  // exponential link function
            for(int i=0; i<n; i++)
                res += y[i] * eta[i] - std::exp(eta[i]) - std::lgamma(y[i] + 1);
            break;
    }

    return res;
}


//
// Rcpp exports
//

// [[Rcpp::export]]
List test_gmrf_approx(NumericVector y, NumericVector x0) {

  int n = y.size();

  // initialize results
  NumericVector b(n);
  NumericVector c(n);

  //
  // get access to raw data
  //

  std::vector<double> y_v = Rcpp::as<std::vector<double> >(y);
  std::vector<double> x_v = Rcpp::as<std::vector<double> >(x0);
  std::vector<double> b_v = Rcpp::as<std::vector<double> >(b);
  std::vector<double> c_v = Rcpp::as<std::vector<double> >(c);
  double* b_data = &b_v[0];
  double* c_data = &c_v[0];
  double* y_data = &y_v[0];
  double* x_data = &x_v[0];

  // build approximation elements
  mcstat2::glm::gmrf_approx(b_data, c_data, x_data, y_data, n,
    mcstat2::glm::glmfamily::poisson);

  // extract approximation
  for(int i=0; i<n; i++) {
    b[i] = b_data[i];
    c[i] = c_data[i];
  }

  return List::create(Named("b") = b, Named("c") = c);
}


// [[Rcpp::export]]
NumericVector test_ll(NumericVector y, NumericVector lambda) {

	//
	// extract data
	//

	int n = y.size();

	std::vector<double> y_v = Rcpp::as<std::vector<double> >(y);
	std::vector<double> lambda_v = Rcpp::as<std::vector<double> >(lambda);

	double* y_data = &y_v[0];
	double* lambda_data = &lambda_v[0];

	// evaluate likelihood
	return wrap(mcstat2::glm::ll(y_data, lambda_data, n,
    mcstat2::glm::glmfamily::poisson));
}
