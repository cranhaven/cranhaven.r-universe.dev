#include <Rcpp.h>
#include <R.h>
#include <string>
#include "covid19_model.h"
#include "seir_model.h"



Rcpp::List covid19_model_interface(
    Rcpp::NumericVector input_pop_N,
    Rcpp::NumericMatrix input_dist_mat,
    Rcpp::IntegerVector input_E_pops,
    Rcpp::List input_tw,
    Rcpp::IntegerVector input_realz_seeds,
    Rcpp::NumericVector input_census_area,
    double stoch_sd,
    int trans_type,
    double dd_trans_monod_k,
    double frac_beta_asym,
    double frac_beta_hosp,
    double delta,
    double recov_a,
    double recov_p,
    double recov_s,
    double recov_home,
    double recov_icu1,
    double recov_icu2,
    double asym_rate,
    double sym_to_icu_rate);

Rcpp::List seir_model_interface(
    Rcpp::NumericVector input_pop_N,
    Rcpp::NumericMatrix input_dist_mat,
    Rcpp::IntegerVector input_E_pops,
    Rcpp::List input_tw,
    Rcpp::IntegerVector input_realz_seeds,
    Rcpp::NumericVector input_census_area,
    double stoch_sd,
    int trans_type,
    double dd_trans_monod_k,
    double birth,
    double incubate,
    double recov);
