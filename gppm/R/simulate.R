#' Simulate from a Gaussian process panel model
#'
#' This function is used to simulate from a Gaussian process panel model,
#' which has been specified using \code{\link{gppm}}.
#'
#' @param object object of class GPPM. The Gaussian process panel model from which to simulate.
#'
#' @param parameterValues numeric vector. Used to specify the values for the parameters.  Which value belongs to which parameter is determined by the names attribute of parameterValues. See also the example.
#'
#' @param seed numeric. Random seed to be used.
#'
#' @param nsim integer. Number of data sets to generate.
#'
#' @param ... additional parameters (currently not used).
#'
#' @param verbose boolean. Print diagnostic output?
#' @return A simulated data set, which is an object of class 'LongData'. If \code{nsim>1} a list of \code{nsim} simulated data sets.
#' @examples
#' \donttest{
#' data("demoLGCM")
#' lgcm <- gppm(
#'   "muI+muS*t", "varI+covIS*(t+t#)+varS*t*t#+(t==t#)*sigma",
#'   demoLGCM, "ID", "y"
#' )
#'
#' parameterValues <- c(10, -1, 0, 10, 0, 0.1)
#' names(parameterValues) <- c("muI", "muS", "varI", "varS", "covIS", "sigma")
#' simData <- simulate(lgcm, parameterValues = parameterValues)
#' }
#' @export
simulate.GPPM <- function(object, nsim = 1, seed = NULL, parameterValues = NULL, verbose = FALSE, ...) {
  if (is.null(parameterValues)) {
    parameterValues <- coef(object)
  }
  validate_simulate(object, parameterValues)

  ## set seed
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    runif(1)
  }
  if (is.null(seed)) {
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  } else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }


  ## core
  object <- fit(object, useOptimizer = FALSE, init = parameterValues, verbose = verbose, hessian = FALSE)
  meansAndCovs <- fitted(object)
  IDs <- meansAndCovs$ID
  simData <- getData(object)
  idCol <- attr(simData, "ID")
  dvCol <- attr(simData, "DV")
  attr(simData, "preds") <- preds(object)
  res <- rep(list(simData), nsim)
  for (j in 1:nsim) {
    simData[, dvCol] <- NA
    for (i in seq_len(length(IDs))) {
      cMu <- meansAndCovs$mean[[i]]
      cCov <- meansAndCovs$cov[[i]]
      simulated <- MASS::mvrnorm(mu = cMu, Sigma = cCov)
      simData[simData[, idCol] == IDs[i], dvCol] <- simulated
    }
    res[[j]] <- simData
  }
  if (j == 1) {
    res <- res[[1]]
  }
  res
}

validate_simulate <- function(gpModel, parameterValues) {
  stopifnot(class(gpModel) == "GPPM")
  stopifnot(is.numeric(parameterValues))

  allParas <- pars(gpModel)
  specified <- names(parameterValues)
  specifiedInModel <- specified %in% allParas
  if (!(all(specifiedInModel))) {
    stop(sprintf("The parameters %s were specified but are not in the model", paste(specified[!specifiedInModel], collapse = ",")))
  }
  modelInSpecified <- allParas %in% specified
  if (!(all(modelInSpecified))) {
    stop(sprintf("The parameters %s are in the model but were not specified", paste(allParas[!modelInSpecified], collapse = ",")))
  }
}
