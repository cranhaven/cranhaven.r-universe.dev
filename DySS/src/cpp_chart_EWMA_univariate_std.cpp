#include <Rcpp.h>
using namespace Rcpp;
using std::max;
using std::pow;

//' @keywords internal
//' @noRd
//[[Rcpp::export]]
NumericMatrix chart_EWMA_univariate_std(
    NumericMatrix eeij,
    IntegerMatrix ttij,
    IntegerVector nobs,
    const double lambda,
    const double ll){
  
  const int nind=ttij.nrow();
  const int nmaxobs=ttij.ncol();
  const double one_lambda=1.0-lambda;
  int ii,jj;
  NumericMatrix EEij(nind,nmaxobs);
  std::fill(EEij.begin(),EEij.end(),NumericVector::get_na());
  
  for(ii=0;ii<nind;ii++){
    EEij(ii,0)=lambda*eeij(ii,0);
    if(EEij(ii,0)>=ll)continue;
    for(jj=1;jj<nobs(ii);jj++){
      EEij(ii,jj)=one_lambda*EEij(ii,jj-1)+lambda*eeij(ii,jj);
      if(EEij(ii,jj)>=ll)break;
    }
  }
  return(EEij);
}

