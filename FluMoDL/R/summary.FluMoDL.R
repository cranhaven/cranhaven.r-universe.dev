#' Summary method for FluMoDL objects
#'
#' This function creates a summarized version of a 'FluMoDL' object. It contains
#' the sets of coefficients and variance-covariance matrices for the incidence
#' proxy terms (for influenza, and for RSV if provided), and the predictions for these terms.
#'
#' @param object An object of class 'FluMoDL'
#' @param ... Further arguments passed to or from other methods.
#'
#' @return An object of class 'summary.FluMoDL'. This is a list containing the following elements:
#'   \describe{
#'     \item{$type}{A string describing the meaning of the coefficients. Defaults to
#'     "summary", meaning a first-stage model summary. Alternatively, "blup" means
#'     Best Unbiased Linear Predictor (BLUP) coefficients, and "pooled" refers to coefficients
#'     pooled in the course of a multivariate meta-analysis. See \code{\link{metaFluMoDL}}.}
#'
#'     \item{$description}{A string with an additional description. For objects created
#'     with \code{summary.FluMoDL()} it is an empty string, but see \code{\link{metaFluMoDL}}.}
#'
#'     \item{$coef}{A list of numeric vectors, with names 'proxyH1', 'proxyH3' and 'proxyB'
#'     (and 'proxyRSV' if provided in the function arguments), containing the model
#'     coefficients for these terms.}
#'
#'     \item{$vcov}{A list of variance-covariance matrices, with names 'proxyH1', 'proxyH3'
#'     and 'proxyB' (and 'proxyRSV' if provided in the function arguments), for the respective
#'     model coefficients.}
#'
#'     \item{$pred}{A list with names 'proxyH1', 'proxyH3' and 'proxyB' (and 'proxyRSV'
#'     if provided in the function arguments), containing
#'     predictions (in the form of \code{\link[dlnm]{crosspred}} objects) for each exposure.
#'     These can be plotted in both the exposure-response and lag-response dimensions, see
#'     \code{\link[dlnm]{crosspred}}, \code{\link[dlnm]{plot.crosspred}} and the example below.}
#'   }
#'
#' @details These summaries can be used to run a \code{\link[=metaFluMoDL]{multivariate meta-analysis}} and calculate
#' pooled effect estimates and BLUP (Best Unbiased Linear Predictor) estimates
#' for influenza (and RSV if provided).
#'
#' @examples
#' data(greece) # Use example surveillance data from Greece
#' m <- with(greece, fitFluMoDL(deaths = daily$deaths,
#'     temp = daily$temp, dates = daily$date,
#'     proxyH1 = weekly$ILI * weekly$ppH1,
#'     proxyH3 = weekly$ILI * weekly$ppH3,
#'     proxyB = weekly$ILI * weekly$ppB,
#'     yearweek = weekly$yearweek))
#' summ <- summary(m)
#' summ
#'
#' # Plot the association between A(H1N1)pdm09 activity and mortality:
#' plot(summ$pred$proxyH1, "overall")
#'
#' @export
summary.FluMoDL <- function(object, ...) {
  if (is.null(object$pred)) stop("No 'pred' element found; object is corrupted.")
  res <- list(
    type = "summary",
    description = "",
    coef = lapply(object$pred[names(object$pred)[grep("proxy", names(object$pred))]], coef),
    vcov = lapply(object$pred[names(object$pred)[grep("proxy", names(object$pred))]], vcov),
    pred = object$pred[grep("proxy", names(object$pred))])
  class(res) <- "summary.FluMoDL"
  return(res)
}



#' @export
print.summary.FluMoDL <- function(x, ...) {
  cat("\n** FluMoDL model summary **\n\n")
  if (length(x$type)==1) {
    if (x$type=="summary") {
      cat("Object is a first-stage model summary.\n")
    } else if (x$type=="blup") {
      cat("Object contains BLUP estimates (Best Unbiased Linear Predictor).\n")
    } else if (x$type=="pooled") {
      cat("Object contains pooled estimates from random-effects multivariate meta-analysis.\n")
    }
  }
  cat(sprintf("Description: ", x$description))
  if (!is.null(x$pred)) {
    cat("Object includes predictions objects (of class 'crosspred').\n")
    mid <- ceiling(length(x$pred$proxyH1$allfit)/2)
    cat(sprintf("Relative Risk for an indicative influenza incidence proxy of %s: (95%% CI)\n",
                names(x$pred$proxyH1$allfit)[mid]))
    cat(sprintf("Influenza A(H1N1)pdm09 = %.3f (%.3f - %.3f)\n",
                exp(x$pred$proxyH1$allfit[mid]),
                x$pred$proxyH1$allRRlow[mid], x$pred$proxyH1$allRRhigh[mid]))
    cat(sprintf("Influenza A(H3N2)      = %.3f (%.3f - %.3f)\n",
                exp(x$pred$proxyH3$allfit[mid]),
                x$pred$proxyH3$allRRlow[mid], x$pred$proxyH3$allRRhigh[mid]))
    cat(sprintf("Influenza B            = %.3f (%.3f - %.3f)\n",
                exp(x$pred$proxyB$allfit[mid]),
                x$pred$proxyB$allRRlow[mid], x$pred$proxyB$allRRhigh[mid]))
    if (!is.null(x$pred$proxyRSV)) {
      cat("Object contains a term for RSV (Respiratory Syncytial Virus)\n")
      cat(sprintf("RR for an indicative RSV incidence proxy of %s = %.3f (%.3f - %.3f)\n",
                  names(x$pred$proxyRSV$allfit)[mid],
                  exp(x$pred$proxyRSV$allfit[mid]),
                  x$pred$proxyRSV$allRRlow[mid],
                  x$pred$proxyRSV$allRRhigh[mid]))
    }

  } else {
    cat("Object does NOT include 'crosspred' objects.\n")
    if (!is.null(x$pred$proxyRSV)) {
      cat("Object contains a term for RSV (Respiratory Syncytial Virus)\n")
    }
    cat("Summary of coefficients per incidence proxy:\n")
    print(do.call("cbind", x$coef))
  }
}
