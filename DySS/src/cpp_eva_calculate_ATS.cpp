#include <Rcpp.h>
using namespace Rcpp;
//' @keywords internal
//' @noRd
//[[Rcpp::export]]
double eva_calculate_ATS_omit(
    NumericMatrix EE,IntegerMatrix tt,
    IntegerVector nobs,NumericVector imputetime,
    double limit){
  int nind=EE.nrow();
  int ii,jj;
  double ATS;
  int sumtime,sumsignal;
  bool signal=false;
  sumtime=0;
  sumsignal=0;
  for(ii=0;ii<nind;ii++){
    signal=false;
    for(jj=0;jj<nobs(ii);jj++){
      if(EE(ii,jj)>limit){
        sumtime=sumtime+tt(ii,jj);
        signal=true;
        break;
      }
    }
    if(signal)sumsignal=sumsignal+1;
  }
  ATS=double(sumtime)/double(sumsignal);
  return ATS;
}

//' @keywords internal
//' @noRd
//[[Rcpp::export]]
double eva_calculate_ATS_impute(
    NumericMatrix EE,IntegerMatrix tt,
    IntegerVector nobs,NumericVector imputetime,
    double limit){
  int nind=EE.nrow();
  int ii,jj;
  double ATS;
  int sumtime,sumsignal;
  bool signal=false;
  sumtime=0;
  sumsignal=0;
  for(ii=0;ii<nind;ii++){
    signal=false;
    for(jj=0;jj<nobs(ii);jj++){
      if(EE(ii,jj)>limit){
        sumtime=sumtime+tt(ii,jj);
        signal=true;
        break;
      }
    }
    if(!signal)sumtime=sumtime+imputetime(ii);
  }
  ATS=double(sumtime)/double(nind);
  return ATS;
}
