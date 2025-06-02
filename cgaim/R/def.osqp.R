def.osqp <- function(qp_pars){
  osqpsets <- formals(osqp::osqpSettings)
  qp_pars <- qp_pars[names(qp_pars) %in% names(osqpsets)]
  # adaptive_rho = FALSE allows the algorithm to converge
  #   to a feasible solution.
  #   See (https://github.com/oxfordcontrol/osqp/issues/151)
  def_settings <- list(verbose = FALSE, adaptive_rho = FALSE,
    eps_abs = osqpsets$eps_abs)
  c(qp_pars, def_settings[!names(def_settings) %in% names(qp_pars)])
}