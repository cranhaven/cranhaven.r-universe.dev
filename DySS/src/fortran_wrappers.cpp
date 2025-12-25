/* export for testing and use in examples */
#include "R_ext/RS.h" /* for e.g., `F77_CALL` macro */
#include "RcppArmadillo.h"

extern "C" {
  void F77_NAME(f90_addarray)(
       double*, int*);
  
  void F77_NAME(f90_add)(
       double*, double*, double*);
  
  void F77_NAME(f90_local_const_mean_est_mult)(
       double*, int*, int*,
       int*, int*, int*, int*, int*, double*);
  
  void F77_NAME(f90_local_const_var_est_mult)(
       double*, int*, int*,
       int*, int*, int*, int*, int*, double*);
  
  void F77_NAME(f90_local_const_cov_est_mult)(
       double*, int*, int*,
       int*, int*, int*, int*, int*, double*);
  
  void F77_NAME(f90_mchart_simultaneous_cusum_both)(
       double*, int*, int*, int*, int*,
       double*, double*, double*, double*, double*);
  
  void F77_NAME(f90_mchart_simultaneous_cusum_upward)(
       double*, int*, int*, int*, int*,
       double*, double*, double*, double*);

  void F77_NAME(f90_mchart_simultaneous_ewma_both)(
       double*, int*, int*, int*, int*,
       double*, double*, double*, double*);

  void F77_NAME(f90_mchart_simultaneous_ewma_upward)(
       double*, int*, int*, int*, int*,
       double*, double*, double*, double*);
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(rng = false)]]
Rcpp::NumericVector f90addarray_wrap
  (Rcpp::NumericVector AA, int ndim){
  F77_CALL(f90_addarray)(AA.begin(), &ndim);
  return AA;
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(rng = false)]]
double f90_add_wrap
  (double a, double b, double c){
  F77_CALL(f90_add)(&a, &b, &c);
  return c;
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(rng = false)]]
Rcpp::NumericVector f90_local_const_mean_est_mult_wrap
  (Rcpp::NumericVector yyij,Rcpp::IntegerVector ttij,Rcpp::IntegerVector nobs,
   int nind,int nmaxobs,int ndim,int ntimepoints,int hh,Rcpp::NumericVector mu_est){
  F77_CALL(f90_local_const_mean_est_mult)(yyij.begin(),ttij.begin(),nobs.begin(),
           &nind,&nmaxobs,&ndim,&ntimepoints,&hh,mu_est.begin());
  return mu_est;
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(rng = false)]]
Rcpp::NumericVector f90_local_const_var_est_mult_wrap
  (Rcpp::NumericVector eps,Rcpp::IntegerVector ttij,Rcpp::IntegerVector nobs,
   int nind,int nmaxobs,int ndim,int ntimepoints,int hh,Rcpp::NumericVector var_est){
  F77_CALL(f90_local_const_var_est_mult)(eps.begin(),ttij.begin(),nobs.begin(),
           &nind,&nmaxobs,&ndim,&ntimepoints,&hh,var_est.begin());
  return var_est;
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(rng = false)]]
Rcpp::NumericVector f90_local_const_cov_est_mult_wrap
  (Rcpp::NumericVector eps,Rcpp::IntegerVector ttij,Rcpp::IntegerVector nobs,
   int nind,int nmaxobs,int ndim,int ntimepoints,int hh,Rcpp::NumericVector varcov_est){
  F77_CALL(f90_local_const_cov_est_mult)(eps.begin(),ttij.begin(),nobs.begin(),
           &nind,&nmaxobs,&ndim,&ntimepoints,&hh,varcov_est.begin());
  return varcov_est;
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(rng = false)]]
Rcpp::List f90_mchart_simultaneous_CUSUM_both_wrap
  (Rcpp::NumericVector eeijk,Rcpp::IntegerVector nobs,int nind,int nmaxobs,int ndim,
   double allowance,double limit,
   Rcpp::NumericVector CCij,Rcpp::NumericVector SSijk_upward,Rcpp::NumericVector SSijk_downward){
  F77_CALL(f90_mchart_simultaneous_cusum_both)(
      eeijk.begin(),nobs.begin(),&nind,&nmaxobs,&ndim,&allowance,&limit,
      CCij.begin(),SSijk_upward.begin(),SSijk_downward.begin());
  Rcpp::List result(3);
  result(0)=CCij;
  result(1)=SSijk_upward;
  result(2)=SSijk_downward;
  return(result);
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(rng = false)]]
Rcpp::List f90_mchart_simultaneous_CUSUM_upward_wrap
  (Rcpp::NumericVector eeijk,Rcpp::IntegerVector nobs,int nind,int nmaxobs,int ndim,
   double allowance,double limit,
   Rcpp::NumericVector CCij,Rcpp::NumericVector SSijk_upward){
  F77_CALL(f90_mchart_simultaneous_cusum_upward)(
      eeijk.begin(),nobs.begin(),&nind,&nmaxobs,&ndim,&allowance,&limit,
      CCij.begin(),SSijk_upward.begin());
  Rcpp::List result(2);
  result(0)=CCij;
  result(1)=SSijk_upward;
  return(result);
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(rng = false)]]
Rcpp::List f90_mchart_simultaneous_EWMA_both_wrap
  (Rcpp::NumericVector eeijk,Rcpp::IntegerVector nobs,int nind,int nmaxobs,int ndim,
   double lambda,double limit,
   Rcpp::NumericVector CCij,Rcpp::NumericVector SSijk){
  F77_CALL(f90_mchart_simultaneous_ewma_both)(
      eeijk.begin(),nobs.begin(),&nind,&nmaxobs,&ndim,&lambda,&limit,
      CCij.begin(),SSijk.begin());
  Rcpp::List result(2);
  result(0)=CCij;
  result(1)=SSijk;
  return(result);
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(rng = false)]]
Rcpp::List f90_mchart_simultaneous_EWMA_upward_wrap
  (Rcpp::NumericVector eeijk,Rcpp::IntegerVector nobs,int nind,int nmaxobs,int ndim,
   double lambda,double limit,
   Rcpp::NumericVector CCij,Rcpp::NumericVector SSijk){
  F77_CALL(f90_mchart_simultaneous_ewma_upward)(
      eeijk.begin(),nobs.begin(),&nind,&nmaxobs,&ndim,&lambda,&limit,
      CCij.begin(),SSijk.begin());
  Rcpp::List result(2);
  result(0)=CCij;
  result(1)=SSijk;
  return(result);
}
