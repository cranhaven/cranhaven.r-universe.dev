//######## GWASinlps package C++ functions #########//

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

//[[Rcpp::export(rng = false)]]
arma::vec arma_cor( arma::mat X, arma::vec y )
{
	int n = X.n_rows;
	int p = X.n_cols;
	double sumy = sum(y);
	double sum2y = sum(y % y);
	double denomy = sqrt(n * sum2y - pow(sumy,2));

	arma::vec out(p);
	for(int i=0; i<p; i++)
	{
	double sumxi = sum(X.col(i));
	double sum2xi = sum(X.col(i) % X.col(i));
	double denomxi = sqrt(n * sum2xi - pow(sumxi,2));

	out(i) = ( n * sum(X.col(i) % y)  - sumxi * sumy ) / ( denomxi * denomy );
	}
	return(out);
}

//[[Rcpp::export(rng = false)]]
int looprun(CharacterVector varsselected, CharacterVector varsleft, int max_nocollect, double m, int nskip)
{
  int out;
  if( (varsselected.size() < m+1) && (varsleft.size() > 0) && (max_nocollect < nskip) )
  out=1; else out=0;
  return(out);
}


