#pragma once
// src/lap_internal.h
#include <Rcpp.h>
#include "lap_utils.h"
#include <string>

// preparation
Rcpp::List prepare_cost_matrix_impl(Rcpp::NumericMatrix cost, bool maximize);

// single-solution solvers (internal)
Rcpp::List solve_bruteforce_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_hungarian_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_munkres_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_ssp_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_csflow_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_auction_impl(Rcpp::NumericMatrix cost, bool maximize, double eps_in);
Rcpp::List solve_auction_scaled_impl(Rcpp::NumericMatrix cost, bool maximize, std::string schedule);
Rcpp::List solve_auction_gauss_seidel_impl(Rcpp::NumericMatrix cost, bool maximize, double eps_in);
Rcpp::List solve_hk01_impl(Rcpp::NumericMatrix cost, bool maximize);

// specialized solvers
Rcpp::List solve_line_metric_impl(const Rcpp::NumericVector& x,
                                  const Rcpp::NumericVector& y,
                                  const std::string& cost,
                                  bool maximize);
Rcpp::List solve_ssap_bucket_impl(Rcpp::NumericMatrix cost, bool maximize);

// certification and feasibility witnesses
Rcpp::List certify_dense_impl(Rcpp::NumericMatrix cost, Rcpp::IntegerVector match,
                              Rcpp::NumericVector u, Rcpp::NumericVector v,
                              bool maximize, double tol, std::string arithmetic,
                              Rcpp::Nullable<Rcpp::List> exact);
Rcpp::List certify_lazy_impl(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                             SEXP distance,
                             Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                             double max_distance, Rcpp::List calipers,
                             Rcpp::IntegerVector match, Rcpp::NumericVector u,
                             Rcpp::NumericVector v, bool maximize, double tol, std::string arithmetic,
                             Rcpp::Nullable<Rcpp::List> exact);
Rcpp::List scan_reduced_costs_impl(Rcpp::NumericMatrix cost, Rcpp::NumericVector u,
                                   Rcpp::NumericVector v, double tol);
Rcpp::List hall_witness_dense_impl(Rcpp::NumericMatrix cost);
Rcpp::List hall_witness_lazy_impl(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                                  SEXP distance,
                                  Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                                  double max_distance, Rcpp::List calipers);

// k-best solvers (internal)
Rcpp::List solve_murty_impl(Rcpp::NumericMatrix cost, int k, bool maximize, std::string single_method);
Rcpp::List solve_kbest_lawler_impl(Rcpp::NumericMatrix cost, int k, std::string method_base, bool maximize);
Rcpp::List solve_cycle_cancel_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_gabow_tarjan_impl(Rcpp::NumericMatrix cost, bool maximize);
