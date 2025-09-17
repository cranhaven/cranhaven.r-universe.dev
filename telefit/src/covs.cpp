#include "covs.h"

using namespace arma;

// build a matern covariance matrix in place
void maternCov(arma::mat & cov, const arma::mat & d, double scale, double range,
			   double smoothness, double nugget ) {

    double cst = std::pow(2.0, 1.0 - smoothness) / R::gammafn(smoothness);
	double cstInv = 1.0 / cst;

	if(cov.n_rows == cov.n_cols) {
		//  WARNING: this function assumes cov is a distance matrix---symmetric, and
		//			 with diag(cov)=0.  no checks are made.

		// compute elementwise correlations
		int n = cov.n_rows;
		for(int i=0; i<n; i++) {

			// diagonal
			cov.at(i,i) = cstInv;

			// off-diagonal
			for(int j=0; j<i; j++) {
				double v = d.at(i,j) / range;
                cov.at(i,j) = std::pow(v, smoothness) * R::bessel_k(v, smoothness, 1.0);
				cov.at(j,i) = cov.at(i,j);
			}
		}

	} else {

		// compute elementwise correlations for non-square matrices
		int n = cov.n_rows;
		int m = cov.n_cols;
		for(int i=0; i<n; i++) {
			for(int j=0; j<m; j++) {
				if(d.at(i,j) <= 1e-300) {
					// numerically zero, or possibly an error in distances
					cov.at(i,j) = cstInv;
				} else {
					double v = d.at(i,j) / range;
					cov.at(i,j) = std::pow(v, smoothness) * R::bessel_k(v, smoothness, 1.0);
				}
			}
		}

	}

	// scale to covariances
	cov = scale * cst * cov;

	// add nugget
	if(nugget!=0)
		cov.diag() += nugget;

}


// compute matern covariances (for a vector of distances) in place
void maternArray( arma::vec & d, double scale, double range,
				 double smoothness, double nugget ) {

	double cst = std::pow(2.0, 1.0 - smoothness) / R::gammafn(smoothness);
	double cstInv = 1.0 / cst;

	// compute elementwise correlations
	int n = d.size();
	for(int i=0; i<n; i++) {
		if(d.at(i)==0) {
			d.at(i) = cstInv;
		} else {
			double v = d.at(i) / range;
			d.at(i) = std::pow(v, smoothness) * R::bessel_k(v, smoothness, 1.0);
		}
	}

	// scale to covariances and add nugget
	d = scale * cst * d + nugget;

}



//
// R exports
//


// [[Rcpp::export]]
arma::mat r_maternCov(arma::mat dist, double scale, double range,
					 double smoothness, double nugget) {
	mat res = mat(dist.n_rows, dist.n_cols, fill::zeros);
	maternCov( res, dist, scale, range, smoothness, nugget );
	return res;
}


// [[Rcpp::export]]
arma::vec r_maternArray(arma::vec dist, double scale, double range,
					   double smoothness, double nugget) {
	maternArray( dist, scale, range, smoothness, nugget );
	return dist;
}
