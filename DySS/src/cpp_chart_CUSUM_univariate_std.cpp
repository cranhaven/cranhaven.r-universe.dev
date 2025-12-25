#include <Rcpp.h>
using namespace Rcpp;
using std::max;

//' @keywords internal
//' @noRd
//[[Rcpp::export]]

NumericMatrix chart_CUSUM_univariate_std(
    NumericMatrix eeij,
    IntegerMatrix ttij,
    IntegerVector nobs,
    const double kk,
    const double ll){
  
  const int nind=ttij.nrow();
  const int nmaxobs=ttij.ncol();
  int ii,jj;
  NumericMatrix CCij(nind,nmaxobs);
  std::fill(CCij.begin(),CCij.end(),NumericVector::get_na());
  
  for(ii=0;ii<nind;ii++){
    CCij(ii,0)=std::max(0.0,eeij(ii,0)-kk);
    if(CCij(ii,0)>=ll)continue;
    for(jj=1;jj<nobs(ii);jj++){
      CCij(ii,jj)=std::max(0.0,CCij(ii,jj-1)+eeij(ii,jj)-kk);
      if(CCij(ii,jj)>=ll)break;
    }
  }
  return(CCij);
}


