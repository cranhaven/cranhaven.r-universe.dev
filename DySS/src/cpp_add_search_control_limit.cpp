#include <Rcpp.h>
using namespace Rcpp;

//' @keywords internal
//' @noRd
//[[Rcpp::export]]
double add_search_control_limit_omit(
    NumericMatrix EE,IntegerMatrix tt,IntegerVector nobs,
    NumericVector imputetime,double ATS_nominal,double ATS_tol,
    double limit_lower,double limit_step,double limit_upper,double limit_tol){
  int nind=EE.nrow();
  int ii,jj;
  double limit,ATS_actual=ATS_nominal-10000.0;
  int sumtime,sumsignal;
  bool signal=false,found=false;
  while(limit_upper-limit_lower>limit_tol){
    if(!found){
      limit=limit_lower+limit_step;
    }else{
      limit=(limit_lower+limit_upper)/2.0;
    }
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
    ATS_actual=double(sumtime)/double(sumsignal);
    if(std::abs(ATS_actual-ATS_nominal)<ATS_tol)break;
    if(ATS_actual>ATS_nominal){
      found=true;
      limit_upper=limit;
    }else{
      limit_lower=limit;
    }
  }
  return limit;
}

//' @keywords internal
//' @noRd
//[[Rcpp::export]]
double add_search_control_limit_impute(
    NumericMatrix EE,IntegerMatrix tt,IntegerVector nobs,
    NumericVector imputetime,double ATS_nominal,double ATS_tol,
    double limit_lower,double limit_step,double limit_upper,double limit_tol){
  int nind=EE.nrow();
  int ii,jj;
  double limit,ATS_actual;
  int sumtime,sumsignal;
  bool signal=false,found=false;
  while(limit_upper-limit_lower>limit_tol){
    if(!found){
      limit=limit_lower+limit_step;
    }else{
      limit=(limit_lower+limit_upper)/2.0;
    }
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
    ATS_actual=double(sumtime)/double(nind);
    if(std::abs(ATS_actual-ATS_nominal)<ATS_tol)break;
    if(ATS_actual>ATS_nominal){
      found=true;
      limit_upper=limit;
    }else{
      limit_lower=limit;
    }
  }
  return limit;
}
