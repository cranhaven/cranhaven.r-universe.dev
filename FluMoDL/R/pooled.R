#' Get pooled effect estimates from metaFluMoDL object
#'
#' This function returns the pooled effect estimates for all incidence proxy terms
#' (three for influenza, and optionally for RSV) from a \code{\link{metaFluMoDL}}
#' object. It returns a \code{\link{summary.FluMoDL}} object that can be
#' further used in analyses.
#'
#' @param m An object of class \code{\link{metaFluMoDL}}, holding the results of a
#'   random-effects multivariate meta-analysis of \code{\link{summary.FluMoDL}}
#'   first-stage model summaries
#'
#' @return An object of class \code{\link{summary.FluMoDL}}, holding the pooled
#'   coefficients and variance-covariance matrices for the three influenza incidence
#'   proxies (four if \code{\link[=hasRSV]{hasRSV(m)}} is \code{TRUE}). The returned
#'   object contains the string "pooled" in its \code{$type} element,
#'   to distinguish it from \code{\link[=summary.FluMoDL]{first-stage model summaries}}
#'   or \code{\link[=metaFluMoDL]{BLUP summaries}} (Best Linear Unbiased Predictor).
#'   The returned \code{\link{summary.FluMoDL}} object also has no \code{$pred} element,
#'   as it is not associated with a particular dataset and cross-basis matrices
#'   (which is a prerequisite to create \code{\link[dlnm]{crosspred}} objects).
#'
#' @seealso \code{\link{summary.FluMoDL}}, \code{\link{metaFluMoDL}}
#'
#' @references \itemize{
#'  \item Gasparrini A, Armstrong B, Kenward MG. Multivariate meta-analysis for non-linear
#'   and other multi-parameter associations.
#'   \href{https://onlinelibrary.wiley.com/doi/full/10.1002/sim.5471}{Stat Med} 2012;31(29):3821–39.
#' }
#'
#' @export
pooled <- function(m) {
  if (!inherits(m, "metaFluMoDL")) stop("argument should be of class 'metaFluMoDL'")
  par <- paste0("proxy", c("H1","H3","B"))
  if (hasRSV(m)) par <- c(par, "proxyRSV")
  res <- list(
    type = "pooled",
    description = "",
    coef = lapply(par, function(p) {
      tmp <- coef(unclass(m)[[p]])
      names(tmp) <- gsub(".(Intercept)", "", names(tmp), fixed=TRUE)
      tmp
    }),
    vcov = lapply(par, function(p) {
      tmp <- vcov(unclass(m)[[p]])
      rownames(tmp) <- gsub(".(Intercept)", "", rownames(tmp), fixed=TRUE)
      colnames(tmp) <- gsub(".(Intercept)", "", colnames(tmp), fixed=TRUE)
      tmp
    }),
    pred = NULL
  )
  names(res$coef) <- par
  names(res$vcov) <- par
  class(res) <- "summary.FluMoDL"
  res
}

