#include <R.h>
#include "utils.h"

using namespace Rcpp;
using namespace arma;
using namespace std;
// [[Rcpp::depends(RcppArmadillo)]]


// this is the megic number 1/(\Phi^{-1}(0.75))
const double invphiinv_75 =  1.482602; 
// calculate MAD in matrix form
arma::vec MAD_cpp(arma::mat data)
{ // we are gonna modify this object, so make a copy
	arma::rowvec med = arma::median(data);
	data.each_row() -= med;		  //minus the median at each row
	data = invphiinv_75 * abs(data);			  // take absolute value
	return (trans(median(data))); // take median again
}

// calculate the rank of data, adopted from package ccaPP ver 0.33, autor Andreas Alfons
arma::vec rank_cpp(const vec &x)
{
	const uword n = x.n_elem;
	uword i, j, k;
	// compute order of observations
	uvec ord = arma::sort_index(x); // different from ccaPP since arma implement sort_index()
	// compute ranks (break ties by taking average)
	vec ranks(n);
	for (i = 0; i < n; i = j + 1)
	{
		j = i;
		// check if there is a series of equal values
		while ((j < n - 1) && (x(ord(j)) == x(ord(j + 1))))
		{
			j++;
		}
		// break ties by average rank, otherwise this gives just the rank
		for (k = i; k <= j; k++)
		{
			ranks(ord(k)) = (i + j + 2) / 2.0;
		}
	}
	// return ranks
	return ranks;
}

