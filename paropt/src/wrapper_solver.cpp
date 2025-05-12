#include "solver.hpp"

// [[Rcpp::export]]
Rcpp::List wrapper_solver(vd &init_state, vd &par_times, vi &param_idx_cuts,
                          vd &parameter_vec, vd &state_measured,
                          vi &state_idx_cuts, vd &integration_times,
                          double reltol, vd &absolute_tolerances,
                          Rcpp::XPtr<OS> fct, int solvertype,
                          Rcpp::XPtr<error_calc_fct> ecf,
                          Rcpp::XPtr<spline_fct> sf, Rcpp::XPtr<JAC> jf) {

  // add parameter to struct
  time_state_information tsi;

  tsi.init_state = init_state;
  tsi.par_times = par_times;
  tsi.param_idx_cuts = param_idx_cuts;
  tsi.state_measured = state_measured;
  tsi.state_idx_cut = state_idx_cuts;
  tsi.integration_times = integration_times;
  tsi.reltol = reltol;
  tsi.absolute_tolerances = absolute_tolerances;
  tsi.ecf = *ecf;
  tsi.sf = *sf;
  tsi.jf = *jf;
  OS ode = *fct;

  // define solver
  solver_ptr_save save_fct = nullptr;
  if (solvertype == 1) {
    save_fct = solver_bdf_save;
  } else if (solvertype == 2) {
    save_fct = solver_adams_save;
  } else if (solvertype == 3) {
    save_fct = solver_bdf_save_with_jac;
  }
  Rcpp::NumericMatrix df(integration_times.size(), init_state.size());

  if (save_fct == nullptr) Rcpp::stop("Undefined solvertype found!");

  double error = save_fct(parameter_vec, ode, tsi, df);
  Rcpp::List L = Rcpp::List::create(error, df);
  return L;
}
