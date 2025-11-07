#' Generic Method For Fitting a model
#'
#' Generic method for fitting a model.
#' @param gpModel a model.
#' @param ... additional arguments.
#' @return A fitted model
#' @seealso \code{\link{fit.GPPM}}
#' @export
fit <- function(gpModel, ...) {
  UseMethod("fit")
}

#' Fit a Gaussian process panel model
#'
#' This function is used to fit a Gaussian process panel model,
#' which has been specified fit using \code{\link{gppm}}.
#'
#' @param gpModel object of class GPPM. The Gaussian process panel model to be fitted.
#'
#' @param init string or named numeric vector. Used to specify the starting values for the parameters. Can either be the string 'random' (default) or a numeric vector startVal of starting values. Which value belongs to which parameter is determined by the names attribute of startVal. See also the example.
#'
#' @param useOptimizer boolean. Should the optimizer be used or not? For false the (possibly random) starting values are returned as the maximum likelihood estimates.
#'
#' @param verbose boolean. Print diagnostic output?
#'
#' @param hessian boolean. Compute the hessian at the maximum likelihood estimate?
#'
#' @param ... additional arguments (currently not used).
#'
#' @return A fitted Gaussian process panel model, which is an object of class 'GPPM'.
#' @seealso Functions to extract from a fitted GPPM:
#' @examples
#' \donttest{
#' # regular usage
#' data("demoLGCM")
#' lgcm <- gppm(
#'   "muI+muS*t", "varI+covIS*(t+t#)+varS*t*t#+(t==t#)*sigma",
#'   demoLGCM, "ID", "y"
#' )
#' lgcmFit <- fit(lgcm)
#'
#' # starting values as ML results
#' startVals <- c(10, 1, 10, 3, 10, 1)
#' names(startVals) <- pars(lgcm)
#' lgcmFakeFit <- fit(lgcm, init = startVals, useOptimizer = FALSE)
#' stopifnot(identical(startVals, coef(lgcmFakeFit)))
#' }
#' @export
fit.GPPM <- function(gpModel, init = "random", useOptimizer = TRUE, verbose = FALSE, hessian = TRUE, ...) {
  if (useOptimizer) {
    iter <- 10000
    algorithm <- "LBFGS" # default
  } else {
    iter <- 0
    algorithm <- "Newton" # because for some reason LBFSGS still does an iteration even if iter==0
  }
  if (!init[1] == "random") {
    validate_simulate(gpModel, init)
    init <- as.list(init)
  }
  if (verbose) {
    outf <- eval
  } else {
    outf <- utils::capture.output
  }
  gpModel$stanOut <- NULL
  theOut <- outf(tryCatch(
    {
      gpModel$stanOut <- rstan::optimizing(gpModel$stanModel, gpModel$dataForStan,
                                           hessian = hessian, iter = iter, init = init,
                                           algorithm = algorithm, as_vector = FALSE)
    },
    error = function(cond) {
      if (verbose) {
        stop(cond)
      }
    }
  ))
  if (!verbose && is.null(gpModel$stanOut)) {
    parseErrorOutFit(theOut)
    for (i in seq_along(theOut)) {
      cat(paste0(theOut[i], "\n"))
    }
    stop("Stan error. See above")
  }
  gpModel$fitRes <- extractFitRes(gpModel$stanOut, gpModel$parsedModel, gpModel$dataForStan[c("nPer", "nTime", "maxTime", "Y")])
  gpModel
}

parseErrorOutFit <- function(errorOut) {
  if (any(grepl("m is not symmetric", errorOut))) {
    stop("Specified covariance function is not symmetric")
  }
  if (any(grepl("m is not positive definite", errorOut))) {
    stop("Specified covariance function is not positive definite")
  }
}
