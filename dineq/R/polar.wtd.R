#' @title Polarization index
#'
#' @description Returns the (possibly weighted) polarization index for a vector. The Wolfson
#' index of bipolarization is used.
#'
#' A bipolarized (income) distribution has fewer observations in the middle and more in lower
#' and/or higher part of the distribution. The regular measures of inequality (like the gini
#' coefficient) does not give information about the polarization of the distribution. This
#' Polarization index computes the level of bipolarization of the distribution. The concept is
#' closely related to the Lorenz curve and therefore the scalar measure is also related to the
#' Gini coefficient. A lower number means a lower level of polarization.
#'
#' Extension of the polar.aff function in affluence-index package. Option of weighting the index
#' is included.
#'
#' @param x a numeric vector.
#' @param weights an optional vector of weights of x to be used in the computation of the
#' Polarization index. Should be NULL or a numeric vector.
#'
#' @return The value of the Wolfson polarization index.
#'
#' @examples
#' #calculate Polarization Index using Mexican Income data set
#' data(mex_inc_2008)
#'
#' #unweighted Polarization Index:
#' polar.wtd(mex_inc_2008$income)
#'
#' #weighted Polarization Index:
#' polar.wtd(x=mex_inc_2008$income, weights=mex_inc_2008$factor)
#'
#' @source
#'  Wolny-Dominiak, A. and A. Saczewska-Piotrowska (2017). affluenceIndex: Affluence Indices.
#'  R package version 1.0. https://CRAN.R-project.org/package=affluenceIndex
#'
#' @references
#' Wolfson M. (1994) When inequalities diverge, \emph{The American Economic Review}, 84, p. 353-358.
#'
#' Schmidt, A. (2002) Statistical Measurement of Income Polarization. A Cross-National, \emph{Berlin 10th
#' International conference on panel data.}
#'
#' @importFrom stats weights
#'
#' @export

polar.wtd <- function (x,weights=NULL)
  {
  if (is.null(weights)){

    missing <- !(is.na(x))
    x <- x[missing]

    gini <- gini.wtd(x)
    Xmedian <- stats::median(x)
    Xmean <- mean(x)
    t <- 0.5 - sum((x)[x<=Xmedian])/sum(x)
    polarization <- 2 * (2 * t - gini)* (Xmean/Xmedian)

  } else {

  missing <- !(is.na(x) | is.na(weights))
  x <- x[missing]
  weights <- weights[missing]
  if (!all(weights>=0)) stop("At least one weight is negative", call.=FALSE)
  if (all(weights == 0)) stop("All weights are zero", call.=FALSE)

  gini <- gini.wtd(x,weights)
  Xmedian <- unname(Hmisc::wtd.quantile(x, weights, probs=0.5))
  Xmean <- stats::weighted.mean(x,weights)
  t <- 0.5 - sum((x*weights)[x<=Xmedian])/sum(x*weights)
  polarization <- 2 * (2 * t - gini)* (Xmean/Xmedian)
  }
  return(polarization)
}

