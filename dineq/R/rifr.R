#' @title Recentered influence function regression (RIF Regression)
#'
#' @description Recentered influence function regression of a distributional statistic.
#'
#' @details RIF Regressions can be used to estimate the marginal effects of covariates on
#' distributional statistics (such as quantiles, gini and variance). It is based on the recentered
#' influence function of a statistic. The transformed RIF is used as the dependent variable in an
#' ordinary least squares regression. RIF regressions are mostly used to estimate the marginal
#' effect of covariates on distributional statistics of income or wealth.
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
#' 0 and 1. Default is 0.5 (median). Multiple quantiles can be used.
#' @param kernel a character giving the smoothing kernel to be used in method "quantile". Options
#' are "gaussian", "rectangular", "triangular", "epanechnikov", "biweight", "cosine"
#' or "optcosine". Default is "gaussian".
#'
#' @return A list containing the results of the RIF regression.
#'    \item{coefficients  }{the coefficient estimates.}
#'    \item{SE}{the coefficient standard error.}
#'    \item{t}{the coefficient t-value.}
#'    \item{p}{the coefficient p-value.}
#'    \item{adjusted_r2   }{the adjusted r-squares.}
#'
#' @seealso \code{\link{rif}}
#'          \code{\link{rifrSE}}
#'
#' @examples
#' data(mex_inc_2008)
#'
#' #Recentered influence funtion of each decile
#' rifr_q <- rifr(income~hh_structure+education, data=mex_inc_2008, weights="factor",
#' method="quantile", quantile=seq(0.1,0.9,0.1), kernel="gaussian")
#'
#' #Recentered influence funtion of the gini coefficient
#' rifr_gini <- rifr(income~hh_structure+education, data=mex_inc_2008, weights="factor",
#' method="gini")
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


rifr <- function(formula, data, weights=NULL, method="quantile", quantile=0.5, kernel="gaussian"){

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

  if (method=="quantile"){
    coef_output <- NULL
    se_output <- NULL
    t_output <- NULL
    p_output <- NULL
    r2_output <- NULL

    for (i in 1:length(quantile)){
      data[,"Xrif"] <- rif(x=data[,Xname],weights=data[,"weights"],method=method, quantile=quantile[i], kernel=kernel)
      reg <- stats::lm(formula=formula_rif, weights=weights_p, data=data)
      coef_output_quantile <- summary(reg)$coefficients[,1]
      coef_output <- rbind(coef_output,coef_output_quantile)
      se_output_quantile <- summary(reg)$coefficients[,2]
      se_output <- rbind(se_output,se_output_quantile)
      t_output_quantile <- summary(reg)$coefficients[,3]
      t_output <- rbind(t_output,t_output_quantile)
      p_output_quantile <- summary(reg)$coefficients[,4]
      p_output <- rbind(p_output,p_output_quantile)
      r2_output_quantile <- summary(reg)$adj.r.squared
      r2_output <- rbind(r2_output,r2_output_quantile)
    }

    coef_output <- t(coef_output)
    se_output <- t(se_output)
    p_output <- t(p_output)
    t_output <- t(t_output)
    colnames(coef_output) <- paste("q",quantile, sep="_")
    colnames(se_output) <- paste("q",quantile, sep="_")
    colnames(t_output) <- paste("q",quantile, sep="_")
    colnames(p_output) <- paste("q",quantile, sep="_")
    rownames(r2_output) <- paste("q", quantile, sep="_")
    output <- list(Coef=coef_output,SE=se_output,t=t_output,p=p_output, adjusted_r2=r2_output)

  }  else {
    data[,"Xrif"] <- rif(x=data[,Xname],weights=data[,"weights"],method=method)
    reg <- stats::lm(formula=formula_rif, weights=weights_p, data=data)
    coef_output <-summary(reg)$coefficients
    colnames(coef_output) <- c("Coef", "SE", "t", "p")
    r2_output <- summary(reg)$adj.r.squared
    output <- list(coefficients=coef_output, adjusted_r2=r2_output)
  }

  return(output)

  }

