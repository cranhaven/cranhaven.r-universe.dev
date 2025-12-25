#include <Rcpp.h>
using namespace Rcpp;
//' @keywords internal
//' @noRd
//[[Rcpp::export]]
List eva_calculate_signal_times_omit(
    NumericMatrix EE,IntegerMatrix tt,
    IntegerVector nobs,NumericVector imputetime,
    double limit){
  int nind=EE.nrow();
  int ii,jj;
  IntegerVector signal_times(nind);
  LogicalVector signals(nind);
  std::fill(signal_times.begin(),signal_times.end(),IntegerVector::get_na());
  std::fill(signals.begin(),signals.end(),false);
  
  for(ii=0;ii<nind;ii++){
    signals(ii)=false;
    for(jj=0;jj<nobs(ii);jj++){
      if(EE(ii,jj)>limit){
        signal_times(ii)=tt(ii,jj);
        signals(ii)=true;
        break;
      }
    }
  }
  
  return List::create(Rcpp::Named("signal_times")=signal_times,
                      Rcpp::Named("signals")=signals);
}

