#' @title Regression-based decomposition of inequality
#'
#' @description Decomposition of (income) inequality into multiple characteristics. A
#' regression-based decomposition method is used.
#'
#' @details This function uses a multivariate regression-based decomposition method. Multiple
#' variables can be added to the function in order to calculate the contribution of each
#' individual variable (including the residual) to the inequality. For instance socio-economic,
#' demographic and geographic characteristics (such as age, household composition, gender, region,
#' education) of the household or the individual can be added.
#'
#' This decomposition can be used on a broad range of inequality measure, like Gini, Theil,
#' mean log deviation, Atkinson index and variance of log income.
#'
#' It uses a logarithmic transformation of the values of the dependent variable. Therefore it
#' cannot handle negative or zero values. Those are excluded from the computation in this function.
#'
#' The main difference with the decomposition of the mean log deviation or Gini coefficient is that
#' multiple characteristics can be analyzed at the same time. While the other decomposition functions
#' only analyze one characteristic at the same time.
#'
#' @param formula an object of class "formula" (or one that can be coerced to that class):
#' a symbolic description of the model to be fitted in the ordinary least squares regression.
#' @param weights an optional vector of weights to be used in the fitting process. Should be
#' NULL or a numeric vector. Should be inside selected data frame in the function and between
#' quotation marks.
#' @param data a data frame containing the variables in the model.
#'
#' @return a list with the results of the decomposition, containing the following components:
#'    \item{inequality_measures}{the values of 4 inequality measures: gini, mean log deviation,
#'    theil and variance of log income}
#'    \item{decomposition_inequality}{the (relative) decomposition of the inequality into the
#'    different variables}
#'    \item{regression_results}{results of the ols regression which is used to make the
#'    decomposition of inequality}
#'    \item{note}{number of zero or negative observations. The function uses a logarithmic
#'    transformation of x as input for the regression. Therefore these observations are deleted
#'    from the analysis}
#'
#' @seealso \code{\link{dineq_change_rb}}
#'
#' @examples
#' #Decomposition of the income inequality into 4 variables using Mexican Income data set:
#' data(mex_inc_2008)
#' inequality_decomp <- dineq_rb(income~hh_structure+education+domicile_size+age_cat,
#' weights="factor", data=mex_inc_2008)
#'
#' #selection of the output: decomposition of the inequality into the contribution of the
#' #different variables and residual (adds up to 100 percent)
#' inequality_decomp["decomposition_inequality"]
#'
#' @references
#' Fields, G. S. (2003). ‘Accounting for income inequality and its change:
#' a new method, with application to the distribution of earnings in the United States’,
#' \emph{Research in Labor Economics}, 22, p. 1–38.
#'
#' Brewer M., and L. Wren-Lewis (2016) Accounting for Changes in Income Inequality:
#' Decomposition Analyses for the UK, 1978–2008. \emph{Oxford Bulletin of economics and
#' statistics}, 78 (3), p. 289-322,
#'
#' @export

dineq_rb <- function(formula, weights=NULL, data){

  data <- data.frame(data)
  if (is.null(weights)){
    data[,"weights"] <- rep(1, nrow(data))
    weights <-  "weights"
  } else {
    data[,"weights"] <- data[,weights]
  }

  variables <- c(all.vars(formula),"weights")
  y_name <- variables[1]
  note= paste(sum(  data[,y_name] <= 0, na.rm=TRUE  ), "negative or zero x's deleted (unweighted)")
  if (!all(data[,"weights"]>=0, na.rm=TRUE)) stop("At least one weight is negative", call.=FALSE)
  if (all(data[,"weights"] == 0, na.rm=TRUE)) stop("All weights are zero", call.=FALSE)

  df<- (data[variables])
  df <- df[which(df[,y_name]>0),]
  df <- df[stats::complete.cases(df), ,drop = FALSE]

  #inequality measures
  gini <- gini.wtd(df[,y_name], df[,"weights"])
  mld <- mld.wtd(df[,y_name], df[,"weights"])
  theil <- theil.wtd(df[,y_name], df[,"weights"])
  df[,y_name] <- log(df[,y_name])
  variance_logincome <- Hmisc::wtd.var(df[,y_name],weights=df[,"weights"])

  df[, "weights_tot"] <- df[, "weights"]
  df[, "weights"] <- df[, "weights"]/sum(df[, "weights"])

  #regression
  regression <- stats::lm(formula, weights=weights, data=df )
  summary_regression <- summary(regression)

  #decomposition of log income
  prediction <- as.data.frame(stats::predict.lm(regression, df, type="terms"))
  prediction[,"residual"] <- stats::resid(regression)

  #calculate relative decomposition of inequality
  correlations <- sapply(prediction, function(x) boot::corr(d=cbind(x, df[,y_name]),w=df[,"weights"]))
  variance_x <- sapply(prediction, function(x) Hmisc::wtd.var(x,weights=df[,"weights_tot"]))
  covvariance <- correlations * sqrt(variance_x*variance_logincome)
  decomposition_inequality <- covvariance/variance_logincome

  return(list(inequality_measures=c(gini=gini,mld=mld,theil=theil,variance_logincome=variance_logincome),
              decomposition_inequality=decomposition_inequality,
              regression_results=summary_regression,
              note=note))
}
