#' Add predictions to summary.FluMoDL objects
#'
#' This function uses the data available in a \link[=fitFluMoDL]{FluMoDL} object
#' to generate predictions (in the form of \code{\link{crosspred}} objects) for a
#' \code{\link{summary.FluMoDL}} object.
#'
#' @param s An object of class \code{\link{summary.FluMoDL}} (normally holding BLUP or
#'   pooled estimates, i.e. \code{s$type} will equal \code{"blup"} or \code{"pooled"})
#'   for which predictions will be generated.
#'
#' @param m An object of class \link[=fitFluMoDL]{FluMoDL}, which provides the original
#'   (untransformed) predictor data to create the predictions.
#'
#' @details Creating a \link[dlnm:crossbasis]{cross-basis matrix} (to use as covariate
#'   in a Distributed-Lag Nonlinear Model) transforms and scales the original predictor.
#'   Interpreting the model coefficients requires revisiting the cross-basis matrix and
#'   backtransforming to the original predictor, in order to generate predicted effect
#'   estimates for specific values of predictor and lag.
#'
#'   For this reason, \code{\link{summary.FluMoDL}} objects created from a
#'   \link[=metaFluMoDL]{multivariate meta-analysis}, containing pooled or BLUP coefficients,
#'   do not contain predictions (their \code{$pred} element is \code{NULL})
#'   because they have no reference to an original predictor.
#'   This is what \code{addPredictions()} does: it uses the cross-basis matrices from a
#'   FluMoDL object \code{m} to calculate predictions with the coefficients in
#'   the \code{\link{summary.FluMoDL}} object \code{s}. It provides the necessary
#'   "context" in which to interpret the model coefficients.
#'
#' @return The function returns the \code{\link{summary.FluMoDL}} object \code{s},
#'   with predictions added (as element \code{$pred})
#'
#' @seealso \code{\link{summary.FluMoDL}}, \code{\link{fitFluMoDL}},
#'   \code{\link[dlnm]{crosspred}}
#'
#' @export
addPredictions <- function(s, m) {
  if (!inherits(s, "summary.FluMoDL")) stop("argument `s` should be of class 'summary.FluMoDL'")
  if (!inherits(m, "FluMoDL")) stop("argument `m` should be of class 'FluMoDL'")
  par <- c("proxyH1", "proxyH3", "proxyB")
  if (hasRSV(s) && hasRSV(m)) par <- c(par, "proxyRSV")
  s$pred <- list()
  for (p in par) {
    s$pred[[p]] <- crosspred(m$basis[[p]], coef=s$coef[[p]], vcov=s$vcov[[p]], model.link="log",
        at=as.numeric(names(m$pred[[p]]$allRRfit)), bylag=0.2, cen=0, cumul=TRUE)
  }
  return(s)
}

