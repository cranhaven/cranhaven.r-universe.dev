#' Get quantile limits of a distribution
#'
#' Get the quantile limits of a distribution, depending on the predicted parameters.
#' @keywords internal
quants <- function(fam_name, pred_params) {

  if (!is.distreg.fam(fam_name))
    stop("Quants only obtainable for distreg families.")

  # Obtain the right functions
  qfun <- fam_fun_getter(fam_name, "q")

  # Evaluate cdf at 0.001 and 0.999 quantile
  lower <- apply(pred_params, 1, function(x) qfun(0.001, par = as.list(x)))
  upper <- apply(pred_params, 1, function(x) qfun(0.999, par = as.list(x)))

  # Return results
  return(list(lower = lower, upper = upper))
}
