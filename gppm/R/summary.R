new_ModelSpecification <- function(meanFormula, covFormula, nPars, params, nPreds, preds) {
  structure(
    list(
      meanFormula = meanFormula,
      covFormula = covFormula,
      nPars = nPars,
      params = params,
      nPreds = nPreds,
      preds = preds
    ),
    class = "ModelSpecification"
  )
}

new_ModelFit <- function(AIC, BIC, logLik) {
  structure(
    list(
      AIC = AIC,
      BIC = BIC,
      logLik = logLik
    ),
    class = "ModelFit"
  )
}

new_DataStats <- function(nPer, maxTime, nTime) {
  structure(
    list(
      nPer = nPer, # number of persons
      maxTime = maxTime, # maximum number of time points
      nTime = nTime # number of time points per persons (vector)
    ),
    class = "DataStats"
  )
}

new_summaryGPPM <- function(modelSpecification, parameterEstimates, modelFit, dataStats) {
  structure(
    list(
      modelSpecification = modelSpecification,
      parameterEstimates = parameterEstimates, # output from parameterEsts()
      modelFit = modelFit,
      dataStats = dataStats
    ),
    class = "summary.GPPM"
  )
}



#' Summarizing GPPM
#'
#' This function is used to summarize a GPPM.
#' \code{summary} method for class 'GPPM'.
#' @param object object of class GPPM.
#'
#' @param ... additional parameters (currently not used).
#'
#' @return An object of class "summary.GPPM", which is a list with 4 entries:
#'  \itemize{
#'   \item \code{modelSpecification} an object of class 'ModelSpecification' describing the model as a list with the following entries
#'   \itemize{
#'       \item \code{meanFormula} formula for the mean function; output of \code{\link{meanFun}}
#'       \item \code{covFormula} formula for the covariance function; output of \code{\link{covFun}}
#'       \item \code{nPars} number of parameters; output of \code{\link{nPars}}
#'       \item \code{params} parameter names; output of \code{\link{pars}}
#'       \item \code{nPreds} number of predictors; output of \code{\link{nPreds}}
#'       \item \code{preds} predictors names; output of \code{\link{preds}}
#'   }
#'   \item \code{parameterEstimates} a data frame containing a summary of the parameter estimates; output of \code{\link{parEsts}}
#'   \item \code{modelfit} An object of class "ModelFit" describing the modelfit using a list with the following entries
#'   \itemize{
#'       \item \code{AIC} AIC of the model; output of \code{\link{AIC}}
#'       \item \code{BIC} BIC of the model; output of \code{\link{BIC}}
#'       \item \code{logLik} log-likelihood of the model; output of \code{\link{logLik}}
#'   }
#'   \item \code{dataStats} An object of class "DataStats" describing the data set using a list with the following entries
#'      \itemize{
#'       \item \code{nPer} number of persons; output of \code{\link{nPers}}
#'       \item \code{maxTime} maximum number of observations per person; output of \code{\link{maxNObs}}
#'       \item \code{nTime} number of observations for each person; output of \code{\link{nObs}}
#'   }
#' }
#' @method summary GPPM
#' @export
summary.GPPM <- function(object, ...) {
  modelSpecification <- new_ModelSpecification(meanFun(object), covFun(object), nPars(object), pars(object), nPreds(object), preds(object))
  dataStats <- new_DataStats(nPers(object), maxNObs(object), nObs(object))
  if (isFitted(object)) {
    parameterEstimates <- parEsts(object)
    modelfit <- new_ModelFit(AIC(object), BIC(object), logLik(object))
  } else {
    parameterEstimates <- modelfit <- NULL
  }
  new_summaryGPPM(modelSpecification, parameterEstimates, modelfit, dataStats)
}

roundForPrint <- function(x) {
  # constants
  rf <- function(x) {
    round(x, 2)
  }

  if (is.data.frame(x)) {
    sel <- vapply(x, is.numeric, FUN.VALUE = TRUE)
    x[, sel] <- rf(x[, sel])
  } else {
    x <- rf(x)
  }
  x
}

#' @describeIn summary.GPPM Printing a summary.GPPM object
#' @param x output of \code{\link{fit.GPPM}}
#' @export
print.summary.GPPM <- function(x, ...) {
  cat("Summary of Gaussian process panel model \n \n")
  cat("Model Specification:", "\n")
  cat(sprintf("\t Mean Formula: \t\t %s \n", x$modelSpecification$meanFormula))
  cat(sprintf("\t Covariance Formula: \t %s \n", x$modelSpecification$covFormula))
  cat(sprintf("\t Parameters: \t\t %s (%d)\n", paste(x$modelSpecification$params, collapse = ","), x$modelSpecification$nPars))
  cat(sprintf("\t Predictors: \t\t %s (%d)\n", paste(x$modelSpecification$preds, collapse = ","), x$modelSpecification$nPreds))
  cat("\n")
  if (!is.null(x$parameterEstimates)) {
    cat("Parameter Estimates:", "\n")
    print(roundForPrint(x$parameterEstimates), row.names = FALSE)
    cat("\n")
  }
  if (!is.null(x$modelFit)) {
    cat("Model Fit:", "\n")
    cat(sprintf("\t AIC: \t\t\t %s \n", roundForPrint(x$modelFit$AIC)))
    cat(sprintf("\t BIC: \t\t\t %s \n", roundForPrint(x$modelFit$BIC)))
    cat(sprintf("\t Log Likelihood: \t %s \n", roundForPrint(x$modelFit$logLik)))
    cat("\n")
  }
  cat("Data Set Characteristica:", "\n")
  cat(sprintf("\t Number of Persons: \t\t %s \n", x$dataStats$nPer))
  cat(sprintf("\t Maximum Number of Observations: \t %s \n", x$dataStats$maxTime))
  cat(sprintf("\t Number of Observations per Person: see nObs()"))
}
