################################################################################
#
#  Setup the parameters for the smoothign part of the algorithm
#
################################################################################

smooth.setup <- function(mf, data, method, control)
{
  method <- match.arg(method, c("scam", "cgam", "scar"))
  fcons <- unlist(sapply(mf, "attr", "fcons"))
  # Match arguments
  methodfun <- switch(method,
    scam = ifelse(length(fcons) > 0, scam::scam, mgcv::gam),
    cgam = cgam::cgam,
    scar = scar::scar)
  methodargs <- methods::formalArgs(methodfun)
  m <- match(methodargs, names(control), 0L)
  control <- control[m]
  # Create formula and check controls (method dependent)
  method_setup <- sprintf("%s.setup", method)
  smooth_comp <- do.call(method_setup, list(mf = mf, control = control))
  # Extract covariates
  mt <- attr(mf, "terms")
  gind <- attr(mt, "specials")$g
  smooth_comp$Xcov <- stats::get_all_vars(
    stats::delete.response(mt[-(gind - 1)]), data)
  # Output
  return(smooth_comp)
}


