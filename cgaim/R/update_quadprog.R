################################################################################
#
# Wrapper for quadprog alpha update
#
################################################################################

update_quadprog <- function(Dmat, dvec, Cmat, bvec, qp_pars){
  res <- quadprog::solve.QP(Dmat, dvec, t(Cmat), bvec = bvec)
  active <- rep(FALSE, length(bvec))
  active[res$iact] <- TRUE
  list(alpha = res$solution, active = active)
}