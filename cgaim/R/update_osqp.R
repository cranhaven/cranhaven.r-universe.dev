################################################################################
#
# Wrapper for osqp alpha update
#
################################################################################

update_osqp <- function(Dmat, dvec, Cmat, bvec, qp_pars){
  res <- osqp::solve_osqp(P = Dmat, q = -dvec, A = Cmat, 
    l = bvec, u = rep(Inf, nrow(Cmat)), pars = qp_pars)
  sol <- res$x
  list(alpha = sol, active = (res$y != 0))
}