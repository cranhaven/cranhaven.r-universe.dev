#' @title Recentered influence function (RIF)
#'
#' @description Returns the (optional weighted) recentered influence function of a distributional
#' statistic.
#'
#' @details The RIF can be used as input for a RIF regression approach. RIF regressions are mostly
#' used to estimate the marginal effect of covariates on distributional statistics of income or
#' wealth.
#'
#' The RIF is calculated by adding the distributional statistic (quantile, gini or variance) to
#' the influence function. RIF is a numeric vector where each element corresponds to a particular
#' individual’s influence on the distributional statistic.
#'
#' @param x a numeric vector for which the recentered influence function is computed.
#' @param weights an optional vector of weights of x to be used in the computation of the
#' recentered influence function. Should be NULL or a numeric vector.
#' @param method the distribution statistic for which the recentered influence function is
#' estimated. Options are "quantile", "gini" and "variance". Default is "quantile".
#' @param quantile quantile to be used when method "quantile" is selected. Must be a numeric
#' between 0 and 1. Default is 0.5 (median). Only a single quantile can be selected.
#' @param kernel a character giving the smoothing kernel to be used in method "quantile". Options
#' are "gaussian", "rectangular", "triangular", "epanechnikov", "biweight", "cosine"
#' or "optcosine". Default is "gaussian".
#'
#' @return A numeric vector of the recentered influence function of the selected distributional
#' statistic.
#'
#' @seealso \code{\link{rifr}}
#'
#' @examples
#' data(mex_inc_2008)
#'
#' #Recentered influence funtion of 20th quantile
#' rif_q20 <- rif(x=mex_inc_2008$income, weights=mex_inc_2008$factor, method="quantile",
#' quantile=0.2)
#'
#' #Recentered influence funtion of the gini coefficient
#' rif_gini <- rif(x=mex_inc_2008$income, weights=mex_inc_2008$factor, method="gini")
#'
#' @references
#' Firpo, S., N. Fortin and T. Lemieux (2009) Unconditional quantile regressions. \emph{Econometrica},
#' 77(3), p. 953-973.
#'
#' Heckley G, U.-G. Gerdtham U-G and G. Kjellsson (2016) A general method for decomposing the
#' causes of socioeconomic inequality in health. \emph{Journal of Health Economics},48, p. 89–106.
#'
#' Pereira, J. and A. Galego (2016) The drivers of wage inequality across Europe, a recentered
#' influence function regression approach, \emph{10th Annual Meeting of the Portuguese Economic
#' Journal}, University of Evora.
#'
#' @export


rif <- function(x, weights=NULL, method="quantile", quantile=0.5, kernel="gaussian"){

  if (is.null(weights)){
    weights <- rep(1, length(x))}
  weights[is.na(weights)] <- 0
  if (!all(weights>=0)) stop("At least one weight is negative", call.=FALSE)
  if (all(weights == 0)) stop("All weights are zero", call.=FALSE)

  if (method=="quantile"){
    df <- data.frame(x = as.numeric(x), w = as.numeric(weights))
    df <- df[stats::complete.cases(df), ,drop = FALSE]
    q <- Hmisc::wtd.quantile(df[,"x"], weights=df[, "w"], probs=quantile, na.rm=TRUE)
    dq <- stats::density(df[,"x"],kernel=kernel, weights=df[, "w"]/sum(df[, "w"]), from=q,to=q,n=1)$y
    RIF <- q + ((quantile - ifelse(x<q,1,0))/dq)
  }

  if (method=="gini"){
    weights <- weights/sum(weights, na.rm=TRUE)
    Xmean <- stats::weighted.mean(x,weights, na.rm=TRUE)
    Xgini <- gini.wtd(x, weights=weights)

    ord <- order(x)
    x <- x[ord]
    weights <- weights[ord]
    p <- cumsum(weights)
    GL <- cumsum(weights * x)

    B2 <- (1-Xgini)*x/Xmean
    C2 <- -2 * Xmean^(-1)*(x * (1 - p) + GL)
    RIF_t <- 1+B2+C2
    RIF <- RIF_t[order(ord)]
    weights <- weights[order(ord)]
  }

  if (method=="variance"){
    weights <- weights/sum(weights, na.rm=TRUE)
    Xmean <- stats::weighted.mean(x,weights, na.rm=TRUE)
    RIF <- (x - Xmean)^2
  }

  RIF[weights==0] <- NA
  return(RIF)
}
