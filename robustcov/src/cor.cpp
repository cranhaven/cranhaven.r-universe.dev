#include <R.h>
#include "cor.h"

using namespace Rcpp;
using namespace arma;
using namespace std;

// this is the megic number 1/(\Phi^{-1}(0.75))
const double invphiinv_75 =  1.482602; 

// --------------------
// Gnanadesikan-Kettenring estimator
// --------------------

double covGK(const vec & x, const vec & y){
	int n = x.n_elem;
	mat dummy_data(n,2);
	dummy_data.col(0) = x + y; // sum
	dummy_data.col(1) = x - y; // diff
	vec MADxy = MAD_cpp(dummy_data); // calculate the MAD estimation
	return ((MADxy(0)*MADxy(0)-MADxy(1)*MADxy(1))/4);
}


// --------------------
// Qn estimator for scale
// --------------------


double scaleQn(const vec & x){
	int n = x.n_elem;
	int k = R::choose(floor(n/2)+1, 2); // kth order statistics
	//int k = floor(R::choose(n,2)/4);
	//Rcout << k << endl;
	uvec upp_ind = arma::trimatu_ind( arma::size(n,n),1); // upper tri index
	arma::mat temp = x * arma::ones(1,n); // copy by column
	arma::vec dist = temp(upp_ind); // take only upper triangular part
	temp = temp.t(); // transpose, so copy by row
	dist -= temp(upp_ind); // calculate distance
	dist = arma::sort(arma::abs(dist)); // sort
	//Rcout << dist << endl;
	return(2.2219 * dist(k));// kth distance, the magic number d is 2.2219 according to Rousseeuw, P. J. and Croux, C. (1993). Alternatives to the median absolute deviation. J. Amer. Statist. Assoc. 88 1273–1283. 
}


// --------------------
// Qn estimator for covariance
// --------------------

double covQn(const vec & x, const vec & y){
	vec pPlus = x + y;
	vec pMinus = x - y; 
	double sigma_plus = scaleQn(pPlus);
	double sigma_minus = scaleQn(pMinus);
	return((sigma_plus * sigma_plus - sigma_minus * sigma_minus)/4);
	
}



// --------------------
// MAD estimator for scale, for individual vectors, useful when dimension is large to save memory
// --------------------

double scaleMAD(vec x){// make a copy, so that we can substract 
	x -= arma::median(x); 
	x = arma::abs(x);
	return(invphiinv_75 * arma::median(x));
}



/*
 * below adopted from ccaPP, all NA checks are removed, we will use R to do the dirty work
 */

// --------------------
// Spearman correlation
// --------------------

// barebones arma version of the Spearman correlation
double corSpearman(const vec& x, const vec& y) {
	const uword n = x.n_elem;
	// compute ranks
	vec ranksX = rank_cpp(x), ranksY = rank_cpp(y);
	// return Pearson correlation for ranks
	return as_scalar(cor(ranksX, ranksY));// original ccaPP has its own corPearson, chaned by cor in arma here
}

// version with parameter that determines whether consistent estimate at the
// normal model should be computed
//double corSpearman(const vec& x, const vec& y, const bool& consistent) {
//	double r = corSpearman(x, y);	// Spearman correlation
//	if(consistent) {
//		r = 2 * sin(M_PI * r / 6);	// consistent estimate at the normal model
//	}
//	return r;
//}

// -------------------
// Kendall correlation
// -------------------

// naive n^2 implementation
double naiveCorKendall(const vec& x, const vec& y, const uword& n) {
	// compute sum of signs for pairs of observations
	double sum = 0, xi, xj, yi, yj;
	sword signX, signY;				// signed integers
	uword nTieX = 0, nTieY = 0;		// unsigned integers
	for(uword j = 0; j < n; j++) {
		xj = x(j); yj = y(j);
		for(uword i = 0; i < j; i++) {
			// obtain sign and update number of ties for pair of x values
			xi = x(i);
			if(xi > xj) {
				signX = 1;
			} else if(xi < xj) {
				signX = -1;
			} else {
				signX = 0;
				nTieX++;
			}
			// obtain sign and update number of ties for pair of y values
			yi = y(i);
			if(yi > yj) {
				signY = 1;
			} else if(yi < yj) {
				signY = -1;
			} else {
				signY = 0;
				nTieY++;
			}
			// update sum for numerator
			sum += signX * signY;
		}
	}
	// ties need to be subtracted from number of pairs for denominator
	uword nPairs = n * (n-1) / 2;
	double dX = nPairs - nTieX, dY = nPairs - nTieY;
	// return Kendall correlation
	return sum / (sqrt(dX) * sqrt(dY));
}

// barebones arma version of the Kendall correlation
double corKendall(const vec& x, const vec& y) {
	const uword n = x.n_elem;
	// call naive version for small n and fast version for larger n
	double r;
	if(n < 30) {
		r = naiveCorKendall(x, y, n);
	} else {
		r = fastCorKendall(x, y, n);
	}
	return r;
}

// version with parameter that determines whether consistent estimate at the
// normal model should be computed
//double corKendall(const vec& x, const vec& y, const bool& consistent) {
//	double r = corKendall(x, y);	// Kendall correlation
//	if(consistent) {
//		r = sin(M_PI * r / 2);		// consistent estimate at the normal model
//	}
//	return r;
//}


// barebones arma version of the quadrant correlation
double corQuadrant(const vec& x, const vec& y) {
	const uword n = x.n_elem;
	// count number of observations in 1./3. and 2./4. quadrants, respectively
	double medX = arma::median(x), medY = arma::median(y);
	sword onethree = 0, twofour = 0;
	for(uword i = 0; i < n; i++) {
		// this should be numerically stable
		double xi = x(i), yi = y(i);
	    if(((xi > medX) && (yi > medY)) || ((xi < medX) && (yi < medY))) {
	    	onethree++;
	    } else if(((xi > medX) && (yi < medY)) || ((xi < medX) && (yi > medY))) {
	    	twofour++;
	    }
	}
	// return quadrant correlation
	return ((double) (onethree - twofour)) / ((double) (onethree + twofour));
}

// version with parameter that determines whether consistent estimate at the
// normal model should be computed
//double corQuadrant(const vec& x, const vec& y, const bool& consistent) {
//	double r = corQuadrant(x, y);	// quadrant correlation
//	if(consistent) {
//		r = sin(M_PI * r / 2);		// consistent estimate at the normal model
//	}
//	return r;
//}
