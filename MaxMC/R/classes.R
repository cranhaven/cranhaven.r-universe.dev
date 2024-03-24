#' S3 class \code{mmc}.
#'
#' @exportClass mmc
#

#' S3 class \code{mmc} object generating function
#'
#' @param call An object of class \code{call}. Original call to \code{mmc}
#' @param seed An integer vector. Value of \code{.Random.seed} at the start of
#'  \code{mmc} call.
#' @param lmc An object of class \code{mc}. If \code{par} is specified, it returns an object
#' of class \code{mc} corresponding the Local Monte Carlo test.
#' @param opt_result An object returning the optimization results.
#' @param opt_trace An object returning the optimization results.
#'
#' @inheritParams mmc
#' @inheritParams pvalue
#'
#' @example /inst/examples/return_mmc_example.R
#'
#' @keywords internal
#'
return_mmc <- function(S0, y, statistic, dgp, est, lower, upper, N, type,
                       method, alpha, control, call, seed, lmc, opt_result, opt_trace) {
    # Extract the p-value from the optimization results
    if (method == "GenSA") {
        pval <- abs(opt_result$value)
    } else if (method == "pso") {
        pval <- abs(opt_result$value)
    } else if (method == "GA") {
        pval <- opt_result@fitnessValue
    } else if (method == "gridSearch") {
        pval <- abs(opt_result$minfun)
    }

    opt_trace<- stats::na.omit(opt_trace)

    out <- list(S0 = S0, pval = pval, data = y, statistic = statistic,
              dgp = dgp, est = est, lower = lower, upper = upper, N = N, type = type,
              method = method, control = control, call = call, seed = seed,
              lmc = lmc, opt_result = opt_result, opt_trace = opt_trace)
    # If alpha is specified, test for rejection at specified level
    if (!is.null(alpha)){
      rejection <- out$pval <= alpha
      names(rejection) <- scales::percent(alpha)
      out$rejection <- rejection
      }
    class(out) <- "mmc"
    return(out)
}

#' S3 class \code{mc}.
#'
#' @exportClass mc
#

#' S3 class \code{mc} object generating function
#' @param pval An atomic vector. Monte Carlo p-value of \code{statistic}
#'
#' @inheritParams mc
#' @inheritParams return_mmc
#' @inheritParams pvalue
#'
#' @example /inst/examples/return_mc_example.R
#'
#' @keywords internal
#'
return_mc <- function(S0, y, statistic, dgp, N, type, call, seed, pval) {
  out <- list(S0 = S0, pval= pval, data = y, statistic = statistic, dgp = dgp,
              N = N, type = type, call = call, seed = seed)
  class(out) <- "mc"
  return(out)
}
