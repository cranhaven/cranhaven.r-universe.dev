################################################################################
#
# Wrapper for coneproj alpha update
#
################################################################################

update_coneproj <- function(Dmat, dvec, Cmat, bvec, qp_pars){
  # Fit
  res <- coneproj::qprog(q = Dmat, c = dvec, amat = Cmat, b = bvec,
    msg = qp_pars$msg)
  # Use all.equal for "near equality" due to potential precision errors
  isactive <- mapply(all.equal, Cmat %*% res$thetahat - bvec, 0)
  list(alpha = drop(res$thetahat), active = which(isactive))
}