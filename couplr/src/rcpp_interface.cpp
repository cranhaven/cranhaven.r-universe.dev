// src/rcpp_interface.cpp
// Exports for 4-mode pixel morphing + LAP solvers

// [[Rcpp::plugins(cpp17)]]
#include <Rcpp.h>
#include <limits>
#include <string>
#include <algorithm>
#include <cmath>
#include "../core/lap_internal.h"
#include "../core/lap_utils_rcpp.h"
#include "../gabow_tarjan/utils_gabow_tarjan.h"

using namespace Rcpp;

// =======================
// Forward decl for greedy matching (implemented in solvers/greedy_matching.cpp)
extern Rcpp::List greedy_matching_impl(Rcpp::NumericMatrix cost_matrix, bool maximize, std::string strategy);

// =======================
// Forward decls for LAP solvers (no [[Rcpp::export]] here)
Rcpp::List solve_cycle_cancel_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_gabow_tarjan_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_lapmod_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_bottleneck_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_csa_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_sinkhorn_impl(Rcpp::NumericMatrix cost, double lambda, double tol,
                               int max_iter, Rcpp::Nullable<Rcpp::NumericVector> r_weights,
                               Rcpp::Nullable<Rcpp::NumericVector> c_weights);
Rcpp::IntegerVector sinkhorn_round_impl(Rcpp::NumericMatrix P);
Rcpp::List solve_ramshaw_tarjan_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_push_relabel_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_jv_duals_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_jv_duals_lazy_impl(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                                    SEXP metric, Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                                    double max_distance, Rcpp::List calipers,
                                    bool maximize);
Rcpp::List solve_network_simplex_rcpp(const Rcpp::NumericMatrix& cost_matrix);
// =======================
Rcpp::List prepare_cost_matrix_impl(NumericMatrix cost, bool maximize);
Rcpp::List probe_cost_matrix_impl(SEXP cost);
Rcpp::List solve_bruteforce_impl(NumericMatrix cost, bool maximize);
Rcpp::List solve_auction_lazy_impl(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                                   SEXP metric, Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                                   double max_distance, Rcpp::List calipers,
                                   bool maximize,
                                   Rcpp::Nullable<double> eps);
Rcpp::List solve_murty_impl(Rcpp::NumericMatrix cost, int k, bool maximize, std::string single_method);
Rcpp::List solve_auction_impl(Rcpp::NumericMatrix cost, bool maximize, double eps_in);
Rcpp::List solve_auction_scaled_impl(Rcpp::NumericMatrix cost, bool maximize, std::string schedule);
Rcpp::List solve_auction_scaled_impl(Rcpp::NumericMatrix cost, bool maximize,
                                     double initial_epsilon_factor,
                                     double alpha,
                                     double final_epsilon);
Rcpp::List solve_auction_gauss_seidel_impl(Rcpp::NumericMatrix cost, bool maximize, double eps_in);
Rcpp::List solve_ssp_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_hungarian_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_munkres_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_csflow_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_kbest_lawler_impl(Rcpp::NumericMatrix cost, int k, std::string method_base, bool maximize);
Rcpp::List solve_hk01_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_line_metric_impl(const Rcpp::NumericVector& x,
                                  const Rcpp::NumericVector& y,
                                  const std::string& cost,
                                  bool maximize);
Rcpp::List solve_ssap_bucket_impl(Rcpp::NumericMatrix cost, bool maximize);

// =======================
// Forward decls for the flow model (implemented in flow/flow_rcpp.cpp)
Rcpp::List flow_solve_impl(int n_nodes, Rcpp::NumericVector supply,
                           Rcpp::IntegerVector tail, Rcpp::IntegerVector head,
                           Rcpp::NumericVector lower, Rcpp::NumericVector upper,
                           Rcpp::NumericVector cost,
                           Rcpp::NumericVector warm_flow,
                           Rcpp::NumericVector warm_potential,
                           double time_limit, double tol, double relax_eps,
                           double max_augmentations, bool return_potentials);
Rcpp::List flow_certify_impl(int n_nodes, Rcpp::NumericVector supply,
                             Rcpp::IntegerVector tail, Rcpp::IntegerVector head,
                             Rcpp::NumericVector lower, Rcpp::NumericVector upper,
                             Rcpp::NumericVector cost, Rcpp::NumericVector flow,
                             Rcpp::NumericVector potential, double tol,
                             std::string arithmetic,
                             Rcpp::Nullable<Rcpp::NumericMatrix> exact_potential);
Rcpp::List flow_design_implicit_impl(Rcpp::NumericMatrix left_mat,
                                     Rcpp::NumericMatrix right_mat,
                                     SEXP distance,
                                     Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                                     double max_distance, Rcpp::List calipers,
                                     std::string design,
                                     double first, double second,
                                     double keep_per_row, double width, double tol,
                                     double max_rounds, bool certify);
Rcpp::List replace_lazy_impl(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                             SEXP distance,
                             Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                             double max_distance, Rcpp::List calipers,
                             double per_row);
SEXP pricing_session_new_impl(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                              SEXP distance,
                              Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                              double max_distance, Rcpp::List calipers);
Rcpp::List pricing_session_seed_impl(SEXP session, double width);
Rcpp::List pricing_session_price_exact_impl(SEXP session, Rcpp::NumericMatrix u,
                                            Rcpp::NumericMatrix v, double offset,
                                            double keep_per_row);
Rcpp::List flow_lagrangian_step_impl(int n_nodes, Rcpp::NumericVector supply,
                                     Rcpp::IntegerVector tail, Rcpp::IntegerVector head,
                                     Rcpp::NumericVector lower, Rcpp::NumericVector upper,
                                     Rcpp::NumericVector cost, Rcpp::NumericVector flow,
                                     Rcpp::NumericVector potential,
                                     Rcpp::IntegerVector pair_arc,
                                     Rcpp::IntegerVector pair_left,
                                     Rcpp::IntegerVector pair_right,
                                     Rcpp::IntegerVector left_node,
                                     Rcpp::IntegerVector right_node,
                                     Rcpp::NumericMatrix u, Rcpp::NumericMatrix w,
                                     Rcpp::NumericVector b, Rcpp::NumericVector lambda);
Rcpp::NumericVector exact_dot_impl(Rcpp::NumericVector x, Rcpp::NumericVector y);
int exact_compare_impl(Rcpp::NumericVector e, Rcpp::NumericVector f);
double exact_round_impl(Rcpp::NumericVector e, std::string direction);
double exact_ceil_quotient_impl(Rcpp::NumericVector e, double add, double divisor);
Rcpp::List exact_moment_rows_impl(Rcpp::NumericMatrix u, Rcpp::NumericMatrix w,
                                  Rcpp::NumericVector b, Rcpp::IntegerVector left,
                                  Rcpp::IntegerVector right, Rcpp::NumericVector lambda);
Rcpp::NumericVector pricing_session_cost_impl(SEXP session, Rcpp::IntegerVector i,
                                              Rcpp::IntegerVector j);
Rcpp::List pricing_session_range_impl(SEXP session);
double pricing_session_evaluated_impl(SEXP session);
double implicit_seed_width_impl(double ncol);
Rcpp::List flow_compile_full_match_impl(Rcpp::NumericMatrix cost,
                                        double min_controls, double max_controls);
Rcpp::List flow_compile_couples_impl(std::string design, double n_rows,
                                     double n_cols, double ratio);
Rcpp::List flow_trace_assignment_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List flow_trace_push_relabel_impl(Rcpp::NumericMatrix cost, bool maximize);

// Forward decls for the edge-generation loop (implemented in
// flow/flow_implicit_rcpp.cpp)
Rcpp::List implicit_dense_impl(Rcpp::NumericMatrix cost, bool maximize,
                               double keep_per_row, double width, double tol,
                               double max_rounds, bool certify);
Rcpp::List implicit_lazy_impl(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                              SEXP distance,
                              Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                              double max_distance, Rcpp::List calipers,
                              bool maximize,
                              double keep_per_row, double width, double tol,
                              double max_rounds, bool certify);

// Forward decl for the design path (implemented in flow/flow_path_rcpp.cpp)
Rcpp::List match_path_lazy_impl(Rcpp::NumericMatrix left_mat,
                                Rcpp::NumericMatrix right_mat,
                                SEXP distance,
                                Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                                Rcpp::NumericVector values, Rcpp::List calipers,
                                bool maximize,
                                double keep_per_row, double width, double tol,
                                double max_rounds, bool certify);

// =======================
// Pixel morphing core (implemented in morph_pixel_level.cpp)
// =======================
extern Rcpp::List analyze_color_overlap(const Rcpp::NumericVector& pixelsA,
                                        const Rcpp::NumericVector& pixelsB,
                                        int H, int W,
                                        int quantize_bits);

// REMOVED: extract_patches - No longer needed with square tiling implementation

// REMOVED: compute_color_match_assignment - R does the assignment!
// REMOVED: compute_color_walk_assignment - R does the assignment!

extern Rcpp::NumericMatrix compute_pixel_cost(const Rcpp::NumericVector& pixelsA,
                                              const Rcpp::NumericVector& pixelsB,
                                              int H, int W,
                                              double alpha, double beta);

extern Rcpp::NumericVector downscale_image(const Rcpp::NumericVector& pixels,
                                           int H, int W, int H_new, int W_new);

extern Rcpp::IntegerVector upscale_assignment(const Rcpp::IntegerVector& assignment,
                                              int H_orig, int W_orig,
                                              int H_scaled, int W_scaled);

extern Rcpp::List morph_pixel_level_impl(const Rcpp::NumericVector& pixelsA,
                                         const Rcpp::NumericVector& pixelsB,
                                         const Rcpp::IntegerVector& assignment,
                                         int H, int W,
                                         int n_frames);

extern Rcpp::List color_palette_info(const Rcpp::NumericVector& pixelsA,
                                     const Rcpp::NumericVector& pixelsB,
                                     int H, int W,
                                     int quantize_bits);

extern Rcpp::NumericMatrix spatial_cost_matrix(const Rcpp::IntegerVector& idxA,
                                               const Rcpp::IntegerVector& idxB,
                                               int H, int W);

// =======================
// LAP Solver Exports
// =======================

// [[Rcpp::export]]
Rcpp::List lap_prepare_cost_matrix(NumericMatrix cost, bool maximize) {
  return prepare_cost_matrix_impl(cost, maximize);
}

// Takes SEXP rather than NumericMatrix so an integer cost matrix is read in
// place; a NumericMatrix parameter would coerce it and allocate the copy this
// probe exists to avoid.
// [[Rcpp::export]]
Rcpp::List lap_probe_cost_matrix(SEXP cost) {
  return probe_cost_matrix_impl(cost);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_bruteforce(NumericMatrix cost, bool maximize) {
  return solve_bruteforce_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_jv(NumericMatrix cost, bool maximize) {
  return solve_jv_duals_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List cpp_lap_solve_jv_lazy(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                                 SEXP metric, Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                                 double max_distance, Rcpp::List calipers,
                                 bool maximize) {
  return solve_jv_duals_lazy_impl(left_mat, right_mat, metric, inv_cov, max_distance,
                                  calipers, maximize);
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_lazy_pair_distances(Rcpp::NumericMatrix left_mat,
                                            Rcpp::NumericMatrix right_mat,
                                            SEXP metric,
                                            Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                                            Rcpp::IntegerVector rows,
                                            Rcpp::IntegerVector cols) {
  return lazy_pair_distances_impl(left_mat, right_mat, metric, inv_cov, rows, cols);
}

// [[Rcpp::export]]
double cpp_lazy_distance_sd(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                            SEXP metric,
                            Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov) {
  return lazy_distance_sd_impl(left_mat, right_mat, metric, inv_cov);
}

// [[Rcpp::export]]
Rcpp::List cpp_lap_solve_auction_lazy(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                                      SEXP metric, Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                                      double max_distance, Rcpp::List calipers,
                                      bool maximize,
                                      Rcpp::Nullable<double> eps = R_NilValue) {
  return solve_auction_lazy_impl(left_mat, right_mat, metric, inv_cov, max_distance,
                                 calipers, maximize, eps);
}

// [[Rcpp::export]]
Rcpp::List lap_certify_dense(Rcpp::NumericMatrix cost, Rcpp::IntegerVector match,
                             Rcpp::NumericVector u, Rcpp::NumericVector v,
                             bool maximize, double tol,
                             std::string arithmetic,
                             Rcpp::Nullable<Rcpp::List> exact = R_NilValue) {
  return certify_dense_impl(cost, match, u, v, maximize, tol, arithmetic, exact);
}

// [[Rcpp::export]]
Rcpp::List lap_certify_lazy(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                            SEXP distance,
                            Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                            double max_distance, Rcpp::List calipers,
                            Rcpp::IntegerVector match, Rcpp::NumericVector u,
                            Rcpp::NumericVector v, bool maximize, double tol,
                            std::string arithmetic,
                            Rcpp::Nullable<Rcpp::List> exact = R_NilValue) {
  return certify_lazy_impl(left_mat, right_mat, distance, inv_cov, max_distance,
                           calipers, match, u, v, maximize, tol, arithmetic,
                           exact);
}

// [[Rcpp::export]]
Rcpp::List lap_scan_reduced_costs(Rcpp::NumericMatrix cost, Rcpp::NumericVector u,
                                  Rcpp::NumericVector v, double tol) {
  return scan_reduced_costs_impl(cost, u, v, tol);
}

// [[Rcpp::export]]
Rcpp::List lap_hall_witness_dense(Rcpp::NumericMatrix cost) {
  return hall_witness_dense_impl(cost);
}

// [[Rcpp::export]]
Rcpp::List lap_hall_witness_lazy(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                                 SEXP distance,
                                 Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                                 double max_distance, Rcpp::List calipers) {
  return hall_witness_lazy_impl(left_mat, right_mat, distance, inv_cov,
                                max_distance, calipers);
}

// [[Rcpp::export]]
Rcpp::List lap_kbest_murty(Rcpp::NumericMatrix cost, int k, bool maximize,
                           std::string single_method = "jv") {
  return solve_murty_impl(cost, k, maximize, single_method);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_auction(Rcpp::NumericMatrix cost, bool maximize,
                             Rcpp::Nullable<double> eps = R_NilValue) {
  double eps_in = std::numeric_limits<double>::quiet_NaN();
  if (eps.isNotNull()) eps_in = Rcpp::as<double>(eps.get());
  return solve_auction_impl(cost, maximize, eps_in);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_auction_scaled(Rcpp::NumericMatrix cost, bool maximize,
                                    std::string schedule = "alpha7") {
  std::transform(schedule.begin(), schedule.end(), schedule.begin(),
                 [](unsigned char c){ return static_cast<char>(std::tolower(c)); });
  if (schedule != "alpha7" && schedule != "pow2" && schedule != "halves") {
    LAP_ERROR("Invalid schedule: '%s'. Use: 'alpha7', 'pow2', 'halves'.", schedule.c_str());
  }
  return solve_auction_scaled_impl(cost, maximize, schedule);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_auction_scaled_params(Rcpp::NumericMatrix cost, bool maximize,
                                           double initial_epsilon_factor = 1.0,
                                           double alpha = 7.0,
                                           Rcpp::Nullable<double> final_epsilon = R_NilValue) {
  double fe = -1.0;
  if (final_epsilon.isNotNull()) fe = Rcpp::as<double>(final_epsilon.get());
  return solve_auction_scaled_impl(cost, maximize, initial_epsilon_factor, alpha, fe);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_auction_gs(Rcpp::NumericMatrix cost, bool maximize,
                                Rcpp::Nullable<double> eps = R_NilValue) {
  double eps_in = std::numeric_limits<double>::quiet_NaN();
  if (eps.isNotNull()) eps_in = Rcpp::as<double>(eps.get());
  return solve_auction_gauss_seidel_impl(cost, maximize, eps_in);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_ssp(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_ssp_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_hungarian(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_hungarian_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_munkres(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_munkres_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_csflow(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_csflow_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_kbest_lawler(Rcpp::NumericMatrix cost, int k,
                            std::string method_base = "jv", bool maximize = false) {
  return solve_kbest_lawler_impl(cost, k, method_base, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_hk01(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_hk01_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_line_metric_cpp(Rcpp::NumericVector x,
                                     Rcpp::NumericVector y,
                                     std::string cost = "L1",
                                     bool maximize = false) {
  return solve_line_metric_impl(x, y, cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_ssap_bucket(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_ssap_bucket_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_gabow_tarjan(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_gabow_tarjan_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_lapmod(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_lapmod_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_bottleneck(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_bottleneck_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_csa(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_csa_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_sinkhorn(Rcpp::NumericMatrix cost, double lambda = 10.0,
                              double tol = 1e-9, int max_iter = 1000,
                              Rcpp::Nullable<Rcpp::NumericVector> r_weights = R_NilValue,
                              Rcpp::Nullable<Rcpp::NumericVector> c_weights = R_NilValue) {
  return solve_sinkhorn_impl(cost, lambda, tol, max_iter, r_weights, c_weights);
}

// [[Rcpp::export]]
Rcpp::IntegerVector sinkhorn_round(Rcpp::NumericMatrix P) {
  return sinkhorn_round_impl(P);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_ramshaw_tarjan(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_ramshaw_tarjan_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_push_relabel(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_push_relabel_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_network_simplex(Rcpp::NumericMatrix cost) {
  return solve_network_simplex_rcpp(cost);
}

// =======================
// Flow model exports
// =======================

// [[Rcpp::export]]
Rcpp::List lap_flow_solve(int n_nodes, Rcpp::NumericVector supply,
                          Rcpp::IntegerVector tail, Rcpp::IntegerVector head,
                          Rcpp::NumericVector lower, Rcpp::NumericVector upper,
                          Rcpp::NumericVector cost,
                          Rcpp::NumericVector warm_flow,
                          Rcpp::NumericVector warm_potential,
                          double time_limit, double tol = 1e-12,
                          double relax_eps = 1e-18, double max_augmentations = 0.0,
                          bool return_potentials = true) {
  return flow_solve_impl(n_nodes, supply, tail, head, lower, upper, cost,
                         warm_flow, warm_potential, time_limit, tol, relax_eps,
                         max_augmentations, return_potentials);
}

// [[Rcpp::export]]
Rcpp::List lap_flow_certify(int n_nodes, Rcpp::NumericVector supply,
                            Rcpp::IntegerVector tail, Rcpp::IntegerVector head,
                            Rcpp::NumericVector lower, Rcpp::NumericVector upper,
                            Rcpp::NumericVector cost, Rcpp::NumericVector flow,
                            Rcpp::NumericVector potential, double tol,
                            std::string arithmetic = "auto",
                            Rcpp::Nullable<Rcpp::NumericMatrix> exact_potential = R_NilValue) {
  return flow_certify_impl(n_nodes, supply, tail, head, lower, upper, cost, flow,
                           potential, tol, arithmetic, exact_potential);
}

// [[Rcpp::export]]
Rcpp::List lap_flow_compile_full_match(Rcpp::NumericMatrix cost,
                                       double min_controls, double max_controls) {
  return flow_compile_full_match_impl(cost, min_controls, max_controls);
}

// [[Rcpp::export]]
SEXP lap_pricing_session(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                         SEXP distance,
                         Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                         double max_distance, Rcpp::List calipers) {
  return pricing_session_new_impl(left_mat, right_mat, distance, inv_cov,
                                  max_distance, calipers);
}

// [[Rcpp::export]]
Rcpp::List lap_pricing_seed(SEXP session, double width) {
  return pricing_session_seed_impl(session, width);
}

// [[Rcpp::export]]
Rcpp::List lap_pricing_price_exact(SEXP session, Rcpp::NumericMatrix u,
                                   Rcpp::NumericMatrix v, double offset,
                                   double keep_per_row) {
  return pricing_session_price_exact_impl(session, u, v, offset, keep_per_row);
}

// [[Rcpp::export]]
Rcpp::List lap_flow_lagrangian_step(int n_nodes, Rcpp::NumericVector supply,
                                    Rcpp::IntegerVector tail, Rcpp::IntegerVector head,
                                    Rcpp::NumericVector lower, Rcpp::NumericVector upper,
                                    Rcpp::NumericVector cost, Rcpp::NumericVector flow,
                                    Rcpp::NumericVector potential,
                                    Rcpp::IntegerVector pair_arc,
                                    Rcpp::IntegerVector pair_left,
                                    Rcpp::IntegerVector pair_right,
                                    Rcpp::IntegerVector left_node,
                                    Rcpp::IntegerVector right_node,
                                    Rcpp::NumericMatrix u, Rcpp::NumericMatrix w,
                                    Rcpp::NumericVector b, Rcpp::NumericVector lambda) {
  return flow_lagrangian_step_impl(n_nodes, supply, tail, head, lower, upper, cost,
                                   flow, potential, pair_arc, pair_left, pair_right,
                                   left_node, right_node, u, w, b, lambda);
}

// [[Rcpp::export]]
Rcpp::NumericVector lap_exact_dot(Rcpp::NumericVector x, Rcpp::NumericVector y) {
  return exact_dot_impl(x, y);
}

// [[Rcpp::export]]
int lap_exact_compare(Rcpp::NumericVector e, Rcpp::NumericVector f) {
  return exact_compare_impl(e, f);
}

// [[Rcpp::export]]
double lap_exact_round(Rcpp::NumericVector e, std::string direction) {
  return exact_round_impl(e, direction);
}

// [[Rcpp::export]]
double lap_exact_ceil_quotient(Rcpp::NumericVector e, double add, double divisor) {
  return exact_ceil_quotient_impl(e, add, divisor);
}

// [[Rcpp::export]]
Rcpp::List lap_exact_moment_rows(Rcpp::NumericMatrix u, Rcpp::NumericMatrix w,
                                 Rcpp::NumericVector b, Rcpp::IntegerVector left,
                                 Rcpp::IntegerVector right, Rcpp::NumericVector lambda) {
  return exact_moment_rows_impl(u, w, b, left, right, lambda);
}

// [[Rcpp::export]]
Rcpp::NumericVector lap_pricing_cost(SEXP session, Rcpp::IntegerVector i,
                                     Rcpp::IntegerVector j) {
  return pricing_session_cost_impl(session, i, j);
}

// [[Rcpp::export]]
Rcpp::List lap_pricing_range(SEXP session) {
  return pricing_session_range_impl(session);
}

// [[Rcpp::export]]
double lap_pricing_evaluated(SEXP session) {
  return pricing_session_evaluated_impl(session);
}

// [[Rcpp::export]]
double lap_implicit_seed_width(double ncol) {
  return implicit_seed_width_impl(ncol);
}

// [[Rcpp::export]]
Rcpp::List lap_replace_lazy(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                            SEXP distance,
                            Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                            double max_distance, Rcpp::List calipers,
                            double per_row) {
  return replace_lazy_impl(left_mat, right_mat, distance, inv_cov, max_distance,
                           calipers, per_row);
}

// [[Rcpp::export]]
Rcpp::List lap_design_implicit(Rcpp::NumericMatrix left_mat,
                               Rcpp::NumericMatrix right_mat,
                               SEXP distance,
                               Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                               double max_distance, Rcpp::List calipers,
                               std::string design,
                               double first = 0.0, double second = 0.0,
                               double keep_per_row = 5.0, double width = 0.0,
                               double tol = 1e-9, double max_rounds = 60.0,
                               bool certify = true) {
  return flow_design_implicit_impl(left_mat, right_mat, distance, inv_cov,
                                   max_distance, calipers, design, first,
                                   second, keep_per_row, width, tol, max_rounds,
                                   certify);
}

// [[Rcpp::export]]
Rcpp::List lap_flow_compile_couples(std::string design, double n_rows,
                                    double n_cols, double ratio = 1.0) {
  return flow_compile_couples_impl(design, n_rows, n_cols, ratio);
}

// [[Rcpp::export]]
Rcpp::List lap_flow_trace_assignment(Rcpp::NumericMatrix cost,
                                     bool maximize = false) {
  return flow_trace_assignment_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_flow_trace_push_relabel(Rcpp::NumericMatrix cost,
                                       bool maximize = false) {
  return flow_trace_push_relabel_impl(cost, maximize);
}

// =======================
// Edge generation exports
// =======================
// The assignment over a complete implicit graph, solved by generating the pairs
// it turns out to need. The caller states the cost source and the search knobs;
// the loop keeps the problem, the candidate set and every round on the C++ side
// and crosses once.

// [[Rcpp::export]]
Rcpp::List lap_implicit_dense(Rcpp::NumericMatrix cost, bool maximize = false,
                              double keep_per_row = 5.0, double width = 5.0,
                              double tol = 1e-9, double max_rounds = 60.0,
                              bool certify = true) {
  return implicit_dense_impl(cost, maximize, keep_per_row, width, tol, max_rounds,
                             certify);
}

// [[Rcpp::export]]
Rcpp::List lap_implicit_lazy(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                             SEXP distance,
                             Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                             double max_distance, Rcpp::List calipers,
                             bool maximize = false,
                             double keep_per_row = 5.0, double width = 5.0,
                             double tol = 1e-9, double max_rounds = 60.0,
                             bool certify = true) {
  return implicit_lazy_impl(left_mat, right_mat, distance, inv_cov, max_distance,
                            calipers, maximize, keep_per_row, width, tol,
                            max_rounds, certify);
}

// [[Rcpp::export]]
Rcpp::List lap_match_path_lazy(Rcpp::NumericMatrix left_mat,
                               Rcpp::NumericMatrix right_mat,
                               SEXP distance,
                               Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                               Rcpp::NumericVector values, Rcpp::List calipers,
                               bool maximize = false,
                               double keep_per_row = 5.0, double width = 5.0,
                               double tol = 1e-9, double max_rounds = 60.0,
                               bool certify = true) {
  return match_path_lazy_impl(left_mat, right_mat, distance, inv_cov, values,
                              calipers, maximize, keep_per_row, width, tol,
                              max_rounds, certify);
}

// =======================
// Greedy matching exports (implemented in solvers/greedy_matching.cpp)
// =======================

// One front door: strategy selects the greedy variant. The per-strategy
// implementations live in solvers/greedy_matching.cpp (lap::greedy_matching_*)
// and are reached through the lap::greedy_matching dispatcher.
// [[Rcpp::export]]
Rcpp::List greedy_matching(Rcpp::NumericMatrix cost_matrix, bool maximize = false,
                          std::string strategy = "row_best") {
  return greedy_matching_impl(cost_matrix, maximize, strategy);
}

// =======================
// Pixel morphing exports
// =======================

// [[Rcpp::export]]
Rcpp::List analyze_color_overlap_cpp(Rcpp::NumericVector pixelsA,
                                     Rcpp::NumericVector pixelsB,
                                     int H, int W,
                                     int quantize_bits = 5) {
  const int N = H * W;
  const int expected = N * 3;
  if (pixelsA.size() != expected || pixelsB.size() != expected)
    LAP_ERROR("pixelsA and pixelsB must be H*W*3.");
  return analyze_color_overlap(pixelsA, pixelsB, H, W, quantize_bits);
}

// REMOVED: extract_patches_cpp - No longer needed with square tiling implementation

// REMOVED: compute_color_match_assignment_cpp - R does the assignment!
// REMOVED: compute_color_walk_assignment_cpp - R does the assignment!

// [[Rcpp::export]]
Rcpp::NumericMatrix compute_pixel_cost_cpp(const Rcpp::NumericVector& pixelsA,
                                           const Rcpp::NumericVector& pixelsB,
                                           int H, int W,
                                           double alpha, double beta) {
  const int N = H * W;
  const int expected = N * 3;
  if (pixelsA.size() != expected || pixelsB.size() != expected)
    LAP_ERROR("pixelsA and pixelsB must be H*W*3.");
  return compute_pixel_cost(pixelsA, pixelsB, H, W, alpha, beta);
}

// [[Rcpp::export]]
Rcpp::NumericVector downscale_image_cpp(Rcpp::NumericVector pixels,
                                        int H, int W, int H_new, int W_new) {
  const int N = H * W;
  const int expected = N * 3;
  if (pixels.size() != expected)
    LAP_ERROR("pixels must be H*W*3.");
  return downscale_image(pixels, H, W, H_new, W_new);
}

// [[Rcpp::export]]
Rcpp::IntegerVector upscale_assignment_cpp(Rcpp::IntegerVector assignment,
                                           int H_orig, int W_orig,
                                           int H_scaled, int W_scaled) {
  const int N_scaled = H_scaled * W_scaled;
  if (assignment.size() != N_scaled)
    LAP_ERROR("assignment must have H_scaled*W_scaled elements.");
  return upscale_assignment(assignment, H_orig, W_orig, H_scaled, W_scaled);
}

// [[Rcpp::export]]
Rcpp::List morph_pixel_level_cpp(Rcpp::NumericVector pixelsA,
                                 Rcpp::NumericVector pixelsB,
                                 Rcpp::IntegerVector assignment,
                                 int H, int W,
                                 int n_frames) {
  const int N = H * W;
  const int expected = N * 3;
  if (pixelsA.size() != expected || pixelsB.size() != expected)
    LAP_ERROR("pixelsA and pixelsB must be H*W*3.");
  if (assignment.size() != N)
    LAP_ERROR("assignment must have H*W elements.");
  for (int i = 0; i < N; ++i) {
    if (assignment[i] < 0 || assignment[i] >= N) assignment[i] = i;
  }
  return morph_pixel_level_impl(pixelsA, pixelsB, assignment, H, W, n_frames);
}

// [[Rcpp::export]]
Rcpp::List color_palette_info_cpp(Rcpp::NumericVector pixelsA,
                                  Rcpp::NumericVector pixelsB,
                                  int H, int W,
                                  int quantize_bits = 5) {
  const int N = H * W;
  const int expected = N * 3;
  if (pixelsA.size() != expected || pixelsB.size() != expected)
    LAP_ERROR("pixelsA and pixelsB must be H*W*3.");
  return color_palette_info(pixelsA, pixelsB, H, W, quantize_bits);
}

// [[Rcpp::export]]
Rcpp::NumericMatrix spatial_cost_matrix_cpp(Rcpp::IntegerVector idxA,
                                            Rcpp::IntegerVector idxB,
                                            int H, int W) {
  return spatial_cost_matrix(idxA, idxB, H, W);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_cycle_cancel(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_cycle_cancel_impl(cost, maximize);
}


// =======================
// Dense-scan successive shortest path export
// =======================

// Forward declaration for production solver
extern Rcpp::List sap_dense_solve_impl(Rcpp::NumericMatrix cost_r, bool maximize);

// [[Rcpp::export]]
Rcpp::List sap_dense_solve(Rcpp::NumericMatrix cost_r) {
    return sap_dense_solve_impl(cost_r, false);
}

// =======================
// Gabow-Tarjan test helpers
// =======================

namespace {

CostMatrix gt_cost_from_r(const Rcpp::NumericMatrix& cost) {
  const int n = cost.nrow();
  const int m = cost.ncol();
  CostMatrix out(n, std::vector<long long>(m, BIG_INT));
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      const double val = cost(i, j);
      out[i][j] = R_finite(val) ? static_cast<long long>(std::llround(val)) : BIG_INT;
    }
  }
  return out;
}

MatchVec gt_match_from_r(const Rcpp::IntegerVector& match) {
  MatchVec out(match.size(), NIL);
  for (int i = 0; i < match.size(); ++i) {
    if (match[i] != NA_INTEGER && match[i] > 0) {
      out[i] = match[i] - 1;
    }
  }
  return out;
}

DualVec gt_duals_from_r(const Rcpp::NumericVector& duals) {
  DualVec out(duals.size(), 0);
  for (int i = 0; i < duals.size(); ++i) {
    out[i] = static_cast<long long>(std::llround(duals[i]));
  }
  return out;
}

Rcpp::IntegerVector gt_match_to_r(const MatchVec& match) {
  Rcpp::IntegerVector out(match.size());
  for (int i = 0; i < static_cast<int>(match.size()); ++i) {
    out[i] = match[i] == NIL ? 0 : match[i] + 1;
  }
  return out;
}

Rcpp::NumericVector gt_duals_to_r(const DualVec& duals) {
  Rcpp::NumericVector out(duals.size());
  for (int i = 0; i < static_cast<int>(duals.size()); ++i) {
    out[i] = static_cast<double>(duals[i]);
  }
  return out;
}

// Column view of a row-side matching, which is what the solver reads.
MatchVec gt_col_match_of(const MatchVec& row_match, int m) {
  MatchVec out(m, NIL);
  for (int i = 0; i < static_cast<int>(row_match.size()); ++i) {
    const int j = row_match[i];
    if (j >= 0 && j < m) out[j] = i;
  }
  return out;
}

Rcpp::List gt_paths_to_r(const std::vector<std::vector<std::pair<int, int>>>& paths) {
  Rcpp::List out(paths.size());
  for (int p = 0; p < static_cast<int>(paths.size()); ++p) {
    Rcpp::IntegerMatrix mat(paths[p].size(), 2);
    for (int e = 0; e < static_cast<int>(paths[p].size()); ++e) {
      mat(e, 0) = paths[p][e].first + 1;
      mat(e, 1) = paths[p][e].second + 1;
    }
    out[p] = mat;
  }
  return out;
}

} // namespace

// [[Rcpp::export]]
long long gt_cost_length(long long c_ij, bool in_matching) {
  return cost_length(c_ij, in_matching);
}

// [[Rcpp::export]]
bool gt_is_eligible(long long c_ij, bool in_matching, long long y_u, long long y_v) {
  return is_eligible(c_ij, in_matching, y_u, y_v);
}

// [[Rcpp::export]]
bool gt_check_one_feasible(Rcpp::NumericMatrix cost,
                           Rcpp::IntegerVector row_match,
                           Rcpp::IntegerVector col_match,
                           Rcpp::NumericVector y_u,
                           Rcpp::NumericVector y_v) {
  (void)row_match;  // the matching is read from the column side
  return check_one_feasible(gt_cost_from_r(cost),
                            gt_match_from_r(col_match),
                            gt_duals_from_r(y_u),
                            gt_duals_from_r(y_v));
}

// [[Rcpp::export]]
Rcpp::List gt_build_equality_graph(Rcpp::NumericMatrix cost,
                                   Rcpp::IntegerVector row_match,
                                   Rcpp::NumericVector y_u,
                                   Rcpp::NumericVector y_v) {
  auto graph = build_equality_graph(gt_cost_from_r(cost),
                                    gt_col_match_of(gt_match_from_r(row_match),
                                                    cost.ncol()),
                                    gt_duals_from_r(y_u),
                                    gt_duals_from_r(y_v));
  Rcpp::List out(graph.size());
  for (int i = 0; i < static_cast<int>(graph.size()); ++i) {
    Rcpp::IntegerVector row(graph[i].size());
    for (int k = 0; k < static_cast<int>(graph[i].size()); ++k) {
      row[k] = graph[i][k] + 1;
    }
    out[i] = row;
  }
  return out;
}

// [[Rcpp::export]]
Rcpp::List gt_augment_along_path(Rcpp::IntegerMatrix edges,
                                 Rcpp::IntegerVector row_match,
                                 Rcpp::IntegerVector col_match) {
  std::vector<std::pair<int, int>> path;
  path.reserve(edges.nrow());
  for (int r = 0; r < edges.nrow(); ++r) {
    path.emplace_back(edges(r, 0) - 1, edges(r, 1) - 1);
  }
  MatchVec rows = gt_match_from_r(row_match);
  MatchVec cols = gt_match_from_r(col_match);
  augment_along_path(path, rows, cols);
  return Rcpp::List::create(
    Rcpp::Named("row_match") = gt_match_to_r(rows),
    Rcpp::Named("col_match") = gt_match_to_r(cols)
  );
}

// [[Rcpp::export]]
Rcpp::List gt_find_maximal_augmenting_paths(Rcpp::List eq_graph,
                                            Rcpp::IntegerVector row_match,
                                            Rcpp::IntegerVector col_match) {
  std::vector<std::vector<int>> graph(eq_graph.size());
  for (int i = 0; i < eq_graph.size(); ++i) {
    Rcpp::IntegerVector row = eq_graph[i];
    graph[i].reserve(row.size());
    for (int j : row) {
      graph[i].push_back(j - 1);
    }
  }
  (void)row_match;  // the search reads the matching from the column side
  auto paths = find_maximal_augmenting_paths(graph, gt_match_from_r(col_match));
  return gt_paths_to_r(paths);
}

// [[Rcpp::export]]
Rcpp::List gt_hungarian_step_one_feasible(Rcpp::NumericMatrix cost,
                                          Rcpp::IntegerVector row_match,
                                          Rcpp::IntegerVector col_match,
                                          Rcpp::NumericVector y_u,
                                          Rcpp::NumericVector y_v) {
  MatchVec rows = gt_match_from_r(row_match);
  MatchVec cols = gt_match_from_r(col_match);
  DualVec yu = gt_duals_from_r(y_u);
  DualVec yv = gt_duals_from_r(y_v);
  bool found = hungarian_step_one_feasible(gt_cost_from_r(cost), rows, cols, yu, yv);
  return Rcpp::List::create(
    Rcpp::Named("found") = found,
    Rcpp::Named("row_match") = gt_match_to_r(rows),
    Rcpp::Named("col_match") = gt_match_to_r(cols),
    Rcpp::Named("y_u") = gt_duals_to_r(yu),
    Rcpp::Named("y_v") = gt_duals_to_r(yv)
  );
}

// [[Rcpp::export]]
Rcpp::List gt_match_gt(Rcpp::NumericMatrix cost,
                       Rcpp::Nullable<Rcpp::IntegerVector> row_match = R_NilValue,
                       Rcpp::Nullable<Rcpp::IntegerVector> col_match = R_NilValue,
                       Rcpp::Nullable<Rcpp::NumericVector> y_u = R_NilValue,
                       Rcpp::Nullable<Rcpp::NumericVector> y_v = R_NilValue,
                       int max_iters = 1000,
                       bool check_feasible = false) {
  const int n = cost.nrow();
  const int m = cost.ncol();
  MatchVec rows = row_match.isNotNull()
    ? gt_match_from_r(Rcpp::IntegerVector(row_match))
    : MatchVec(n, NIL);
  MatchVec cols = col_match.isNotNull()
    ? gt_match_from_r(Rcpp::IntegerVector(col_match))
    : MatchVec(m, NIL);
  DualVec yu = y_u.isNotNull()
    ? gt_duals_from_r(Rcpp::NumericVector(y_u))
    : DualVec(n, 0);
  DualVec yv = y_v.isNotNull()
    ? gt_duals_from_r(Rcpp::NumericVector(y_v))
    : DualVec(m, 0);
  match_gt(gt_cost_from_r(cost), rows, cols, yu, yv, max_iters, check_feasible);
  return Rcpp::List::create(
    Rcpp::Named("row_match") = gt_match_to_r(rows),
    Rcpp::Named("col_match") = gt_match_to_r(cols),
    Rcpp::Named("y_u") = gt_duals_to_r(yu),
    Rcpp::Named("y_v") = gt_duals_to_r(yv)
  );
}

// [[Rcpp::export]]
Rcpp::List scale_match_cpp(Rcpp::NumericMatrix cost,
                           Rcpp::Nullable<Rcpp::IntegerVector> row_match = R_NilValue,
                           Rcpp::Nullable<Rcpp::IntegerVector> col_match = R_NilValue,
                           Rcpp::Nullable<Rcpp::NumericVector> y_u = R_NilValue,
                           Rcpp::Nullable<Rcpp::NumericVector> y_v = R_NilValue) {
  const int n = cost.nrow();
  const int m = cost.ncol();
  MatchVec rows = row_match.isNotNull()
    ? gt_match_from_r(Rcpp::IntegerVector(row_match))
    : MatchVec(n, NIL);
  MatchVec cols = col_match.isNotNull()
    ? gt_match_from_r(Rcpp::IntegerVector(col_match))
    : MatchVec(m, NIL);
  DualVec yu = y_u.isNotNull()
    ? gt_duals_from_r(Rcpp::NumericVector(y_u))
    : DualVec(n, 0);
  DualVec yv = y_v.isNotNull()
    ? gt_duals_from_r(Rcpp::NumericVector(y_v))
    : DualVec(m, 0);
  scale_match(gt_cost_from_r(cost), rows, cols, yu, yv);
  return Rcpp::List::create(
    Rcpp::Named("row_match") = gt_match_to_r(rows),
    Rcpp::Named("col_match") = gt_match_to_r(cols),
    Rcpp::Named("y_u") = gt_duals_to_r(yu),
    Rcpp::Named("y_v") = gt_duals_to_r(yv)
  );
}

