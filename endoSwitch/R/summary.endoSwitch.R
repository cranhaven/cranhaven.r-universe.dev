#' Summarize the endogenous switching regression results.
#'
#' @param object Estimated endogenous switching regression model.
#' @param ... Other elements.
#'
#' @return A list containing the key regression results.
#'
#' @export
#'
summary.endoSwitch <- function(object, ...){
  if(!inherits(object, "endoSwitch"))
    stop("'summary.endoSwitch' called on a non-'endoSwitch' object")

  coefEst <- object$MLE.Results$estimate
  coefSD <- sqrt(-diag(solve(object$MLE.Results$hessian)))
  tStat <- coefEst/coefSD
  pValue <- 2*stats::pnorm(-abs(tStat))

  results <- cbind("Estimate" = round(coefEst, 4), 'Std. error' = round(coefSD, 4),
                   't value' = round(tStat, 4), 'Pr(> t)' = round(pValue, 4))
  distPar <- object$distPar
  distPar <- cbind(distPar, 2*stats::pnorm(-abs(distPar[,3])))

  results[(nrow(results) - 3):nrow(results), ] <- distPar
  row.names(results)[(nrow(results) - 3):nrow(results)] <- c('Outcome.0.Sigma', 'Outcome.1.Sigma',
                                                             'Outcome.0.Rho', 'Outcome.1.Rho')
  treatEffect <- object$treatEffect
  if(!is.data.frame(treatEffect)){treatEffect <- 'Treatment effects are not calculated. Use treatEffect = T to calculate them'}

  summary <- list(maxinType = object$MLE.Results$type,
                  iterations = object$MLE.Results$iterations,
                  returnCode = object$MLE.Results$code,
                  returnMessage = object$MLE.Results$message,
                  loglik = object$MLE.Results$maximum,
                  constrains = object$MLE.Results$constraints,
                  estimate = results,
                  treatmentEffect = treatEffect)
  class(summary) <- "summary.endoSwitch"
  summary
}

# print.summary.endoSwitch <- function(x, print = T){
#   coefEst <- object$MLE.Results$estimate
#   coefSD <- sqrt(-diag(solve(object$MLE.Results$hessian)))
#   tStat <- coefEst/coefSD
#   pValue <- 2*pnorm(-abs(tStat))
#
#   results <- cbind("Estimate" = round(coefEst, 4), 'Std. error' = round(coefSD, 4),
#                    't value' = round(tStat, 4), 'Pr(> t)' = round(pValue, 4))
#
#   distPar <- object$distPar
#   distPar <- cbind(distPar, 2*pnorm(-abs(distPar[,3])))
#
#   results[(nrow(results) - 3):nrow(results), ] <- distPar
#   row.names(results)[(nrow(results) - 3):nrow(results)] <- c('Outcome.0.Sigma', 'Outcome.1.Sigma',
#                                                              'Outcome.0.Rho', 'Outcome.1.Rho')
#
#   cat('--------------------------------------------\n')
#   cat('Maximum Likelihood Estimation\n')
#   cat('Log-Likelihood:', x$MLE.Results$maximum, '\n')
#   cat('Coefficient estimates:\n')
#   results
# }
