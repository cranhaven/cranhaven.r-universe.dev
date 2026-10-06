#' @title Inference of recentered influence function regression (RIF regression)
#'
#' @description Inference of a RIF Regression using a bootstrap method.
#'
#' @details RIF Regressions can be used to estimate the marginal effects of covariates on
#' distributional statistics (such as quantiles, gini and variance). It is based on the recentered
#' influence function of a statistic. The transformed RIF is used as the dependent variable in an
#' ordinary least squares regression. RIF regressions are mostly used to estimate the marginal
#' effect of covariates on distributional statistics of income or wealth.
#'
#' The standard errors, confidence intervals and Z- and P-values are calculated by using a
#' standard bootstrap method (from boot package).
#'
#' @param formula an object of class "formula" (or one that can be coerced to that class):
#' a symbolic description of the model to be fitted in the RIF regression.
#' @param data a data frame containing the variables and weights of the model.
#' @param weights an optional vector of weights of x to be used in the computation of the
#' recentered influence function. Should be NULL or a numeric vector. Should be inside selected
#' data frame in the function and between quotation marks.
#' @param method the distribution statistic for which the recentered influence function is
#' estimated. Options are "quantile", "gini" and "variance". Default is "quantile".
#' @param quantile quantile to be used when method "quantile" is selected. Must be a numeric between
#' 0 and 1. Default is 0.5 (median). Only a single quantile can be used.
#' @param kernel a character giving the smoothing kernel to be used in method "quantile". Options
#' are "gaussian", "rectangular", "triangular", "epanechnikov", "biweight", "cosine"
#' or "optcosine". Default is "gaussian".
#' @param Nboot the number of bootstrap replicates. Default is 100.
#' @param confidence significance level for estimation of the confidence interval of the
#' fitted model. Default is 0.95.
#'
#' @return A data frame containing the results of the RIF regression.
#'     \item{Coef}{estimated coefficients of the original (non bootstrapped) RIF regression}
#'     \item{lower}{lower bound of confidence interval of estimated coefficient}
#'     \item{upper}{upper bound of confidence interval of estimated coefficient}
#'     \item{SE}{standard error}
#'     \item{Z Value}{Z value}
#'     \item{P Value}{P value}
#'     \item{Signif}{Significance codes of P: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1}
#'
#' @seealso \code{\link{rif}}
#'          \code{\link{rifr}}
#'
#' @examples
#' data(mex_inc_2008)
#'
#' #Recentered influence funtion of 20th quantile
#' rifr_q <- rifrSE(income~hh_structure+education, data=mex_inc_2008, weights="factor",
#' method="quantile", quantile=0.2, kernel="gaussian", Nboot=100, confidence=0.95)
#'
#' #Recentered influence funtion of the gini coefficient
#' rifr_gini <- rifrSE(income~hh_structure+education, data=mex_inc_2008, weights="factor",
#' method="gini", Nboot=100, confidence=0.95)
#'
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


rifrSE <- function(formula, data, weights=NULL, method="quantile", quantile=0.5, kernel="gaussian",
                     Nboot=100, confidence=0.95){

  data <- data.frame(data)
  if (is.null(weights)){
    data[,"weights"] <- rep(1, nrow(data))
  } else {
    data[,"weights"] <- data[,weights]
  }
  data$weights[is.na(data$weights)] <- 0
  weights_p <- NULL
  weights_p <- data[,"weights"]/sum(data[,"weights"])
  data$weights_p <- weights_p
  if (!all(data[,"weights"]>=0)) stop("At least one weight is negative", call.=FALSE)
  if (all(data[,"weights"] == 0)) stop("All weights are zero", call.=FALSE)

  Xname <- all.vars(formula)[1]
  formula_rif <- stats::update.formula(formula,Xrif~.)

  data[,"Xrif"] <- rif(x=data[,Xname],weights=data[,"weights"],method=method, quantile=quantile, kernel=kernel)


  # function for input boot
  regfunctie <- function(data, indices) {
    d <- data[indices,]
    stats::lm(formula_rif, data=d, weights=weights_p)$coef
  }

  # bootstrapping with N replications
  results <- boot::boot(data=data, statistic=regfunctie,
                  R=Nboot)

  # results of bootstrap
  conf_int <- NULL
  for (j in 1:ncol(results$t)){
    conf_temp <- boot::norm.ci(results, conf=confidence, index=j)
    conf_int <- rbind(conf_int,conf_temp)
  }
  SEs = sapply(data.frame(results$t), stats::sd)
  Coefs = as.numeric(results$t0)
  Z = Coefs / SEs
  P = 2*stats::pnorm(-abs(Z))
  Signif <- stats::symnum(P, corr = FALSE, na = FALSE,
                          cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                          symbols = c("***", "**", "*", ".", " "))

  output <- data.frame(Coefs, conf_int[,2], conf_int[,3], SEs, Z, P, Signif)
  rownames(output) <- names(results$t0)
  colnames(output)=c("Coef","Lower","Upper","Std.Error","Z value", "P Value", "Signif")

  return(output)
}
