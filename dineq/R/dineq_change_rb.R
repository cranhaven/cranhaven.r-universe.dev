#' @title Decomposition of the change in inequality
#'
#' @description Decomposition of the change in (income) inequality into multiple characteristics,
#' divided by a price and a quantity effect.
#'
#' @details This function uses a multivariate regression-based decomposition method. Multiple
#' characteristics can be added to the function in order to calculate the contribution of each
#' individual variable (including the residual) to the change of the inequality. For instance
#' socio-economic,  demographic and geographic characteristics (such as age, household composition,
#' gender, region, education) of the household or the individual can be added.
#'
#' The change decomposition is divided into a price and a quantity effect for each characteristic.
#' The quantity effect is caused by changes in the relative size of subgroups (for instance: a higher
#' percentage of elderly households). The price effect is caused by a change in the influence of the
#' characteristic on the dependent variable (for instance a higher income for the elderly
#' households).
#'
#' It uses a logarithmic transformation of the values of the dependent variable. Therefore it
#' cannot handle negative or zero values. Those are excluded from the computation in this function.
#'
#' The decomposition can only be used on the variance of log income.
#'
#' The main difference with the decomposition of the change of the mean log deviation is that
#' multiple characteristics can be analyzed at the same time. While the decomposition function
#' only analyze one characteristic at the same time.
#'
#' The function uses two datasets for both years to compare. Pay attention that characteristics
#' should be the same (although can be named differently) and in the same order in the formula.
#'
#' @param formula1 an object of class "formula" (or one that can be coerced to that class)
#' for the first year/dataset: a symbolic description of the model to be fitted in the ordinary
#' least squares regression.
#' @param weights1 an optional vector of weights to be used in the fitting process. Should be
#' NULL or a numeric vector. Should be inside selected data frame in the function and between
#' quotation marks.
#' @param data1 a data frame containing the variables for the first year/dataset in the model.
#'
#' @param formula2 an object of class "formula" (or one that can be coerced to that class)
#' for the first year/dataset: a symbolic description of the model to be fitted in the ordinary
#' least squares regression.
#' @param weights2 an optional vector of weights to be used in the fitting process. Should be
#' NULL or a numeric vector. Should be inside selected data frame in the function and between
#' quotation marks.
#' @param data2 a data frame containing the variables for the first year/dataset in the model.
#'
#' @return a list with the results of the decomposition and the parts used for the
#' decomposition, containing the following components:
#'    \item{attention}{optional note on the difference in the input.}
#'    \item{variance_logincome}{the values of the variance of log income of both years/datasets and
#'    difference between both.}
#'    \item{decomposition_inequality}{the (relative) decomposition of the inequality of both
#'    years/datasets into the different variables. See function 'rb_decomp'.}
#'    \item{decomposition_change_absolute}{decomposition of the change in the variance
#'    of log income into the different variables and residual split into price and quantity
#'    effects. Adds up to the absolute change in variance of log income.}
#'    \item{decomposition_change_relative}{decomposition of the change in the variance of log
#'    income into the different variables and residual split into price and quantity effects.
#'    Adds up to 100 percent.}
#'    \item{notes}{number of zero or negative observations in both data sets/years. The function
#'    uses a logarithmic transformation of x as input for the regression. Therefore these
#'    observations are deleted from the analysis}
#'
#' @seealso \code{\link{dineq_rb}}
#'
#' @examples
#' #Decomposition of the change in income inequality into 4 variables using the Mexican Income
#' #data set
#' data(mex_inc_2008)
#' inequality_change <- dineq_change_rb(formula1=income~hh_structure+education+domicile_size+age_cat,
#' weights1="factor",data1=mex_inc_2008, formula2=income~hh_structure+education+
#' domicile_size+age_cat, weights2="factor",data2=mex_inc_2016)
#'
#' #selection of output: change in variance of log income decomposed in variables split into price
#' #and quantity effect and residual.
#' inequality_change["decomposition_change_absolute"]
#'
#' #selection of output: relatieve change in variance of log income decomposed in variables split
#' #into price and quantity effect and residual. Because of negative change in variance of log
#' #income, the negative contributuon of education (quantity) becomes a positive number.
#' inequality_change["decomposition_change_relative"]
#'
#' @references
#' Yun, M.-S. (2006) Earnings Inequality in USA, 1969–99: Comparing Inequality Using Earnings
#' Equations, \emph{Review of Income and Wealth}, 52 (1): p. 127–144.
#'
#' Fields, G. (2003) Accounting for income inequality and its change: a new method, with
#' application to the distribution of earnings in the United States, \emph{Research in Labor
#' Economics}, 22, p. 1–38.
#'
#' Brewer M., and L. Wren-Lewis (2016) Accounting for Changes in Income Inequality:
#' Decomposition Analyses for the UK, 1978–2008. \emph{Oxford Bulletin of economics and
#' statistics}, 78 (3), p. 289-322,
#'
#' @importFrom stats weights
#' @export

dineq_change_rb <- function(formula1, weights1 = NULL, data1,
                          formula2, weights2 = NULL, data2)
{
  attention= NULL
  if (formula1!=formula2) attention="be carefull: different formulas. If the order of variables is different, results will be wrong. Different names but the same order is good"

  if (length(formula1)!=length(formula2)) stop("Different number of independent variables in formulas", call.=FALSE)

  ###data 1
  data1 <- data.frame(data1)
  if (is.null(weights1)){
    data1[,"weights"] <- rep(1, nrow(data1))
    weights1 <-  "weights"
  } else {
    data1[,"weights"] <- data1[,weights1]
  }

  variables1 <- c(all.vars(formula1),"weights")
  y_name1 <- variables1[1]
  note1= paste(sum(  data1[,y_name1] <= 0, na.rm=TRUE  ), "negative or zero x's deleted (unweighted) in data1")
  if (!all(data1[,"weights"]>=0, na.rm=TRUE)) stop("At least one weight in data1 is negative", call.=FALSE)
  if (all(data1[,"weights"] == 0, na.rm=TRUE)) stop("All weights in data1 are zero", call.=FALSE)

  df1<- (data1[variables1])
  df1 <- df1[which(df1[,y_name1]>0),]
  df1 <- df1[stats::complete.cases(df1), ,drop = FALSE]

  #inequality measures
  df1[,y_name1] <- log(df1[,y_name1])
  variance_logincome1 <- Hmisc::wtd.var(df1[,y_name1],weights=df1[,"weights"])
  df1[, "weights_tot"] <- df1[, "weights"]
  df1[, "weights"] <- df1[, "weights"]/sum(df1[, "weights"])

  #regression
  regression1 <- stats::lm (formula1, weights=weights, data=df1 )
  summary_regression1 <- summary(regression1)

  #decomposition of log income
  prediction1 <- as.data.frame(stats::predict.lm(regression1, df1, type="terms"))
  prediction1[,"residual"] <- stats::resid(regression1)

  #calculate relative decomposition of inequality
  correlations1 <- sapply(prediction1, function(x) boot::corr(d=cbind(x, df1[,y_name1]),w=df1[,"weights"]))
  variance_x1 <- sapply(prediction1, function(x) Hmisc::wtd.var(x,weights=df1[,"weights_tot"]))
  covvariance1 <- correlations1 * sqrt(variance_x1*variance_logincome1)
  decomposition_inequality1 <- covvariance1/variance_logincome1

  ###data2
  data2 <- data.frame(data2)
  if (is.null(weights2)){
    data2[,"weights"] <- rep(1, nrow(data2))
    weights2 <-  "weights"
  } else {
    data2[,"weights"] <- data2[,weights2]
  }

  variables2 <- c(all.vars(formula2),"weights")
  y_name2 <- variables2[1]
  note2= paste(sum(  data2[,y_name2] <= 0, na.rm=TRUE  ), "negative or zero x's deleted (unweighted) in data2")
  if (!all(data2[,"weights"]>=0, na.rm=TRUE)) stop("At least one weight in data2 is negative", call.=FALSE)
  if (all(data2[,"weights"] == 0, na.rm=TRUE)) stop("All weights in data2 are zero", call.=FALSE)

  df2<- (data2[variables2])
  df2 <- df2[which(df2[,y_name2]>0),]
  df2 <- df2[stats::complete.cases(df2), ,drop = FALSE]

  #inequality measures
  df2[,y_name2] <- log(df2[,y_name2])
  variance_logincome2 <- Hmisc::wtd.var(df2[,y_name2],weights=df2[,"weights"])
  df2[, "weights_tot"] <- df2[, "weights"]
  df2[, "weights"] <- df2[, "weights"]/sum(df2[, "weights"])

  #regression
  regression2 <- stats::lm (formula2, weights=weights, data=df2 )
  summary_regression2 <- summary(regression2)

  #decomposition of log income
  prediction2 <- as.data.frame(stats::predict.lm(regression2, df2, type="terms"))
  prediction2_constant <- stats::predict.lm(regression2, df2, type="terms")
  prediction2[,"residual"] <- stats::resid(regression2)

  #calculate relative decomposition of inequality
  correlations2 <- sapply(prediction2, function(x) boot::corr(d=cbind(x, df2[,y_name2]),w=df2[,"weights"]))
  variance_x2 <- sapply(prediction2, function(x) Hmisc::wtd.var(x,weights=df2[,"weights_tot"]))
  covvariance2 <- correlations2 * sqrt(variance_x2*variance_logincome2)
  decomposition_inequality2 <- covvariance2/variance_logincome2

  ###new coefficients applied on data 1
  prediction_hat <- as.data.frame(stats::predict.lm(regression2,df1,type="terms"))
  prediction_hat[,"residual"] <- stats::resid(regression1)
  income_hat <- rowSums(prediction_hat) + attr(prediction2_constant,'constant')
  variance_logincome_hat <- Hmisc::wtd.var(income_hat,weights=df1[,"weights_tot"])

  correlations_hat <- sapply(prediction_hat, function(x) boot::corr(d=cbind(x, income_hat),w=df1[,"weights"]))
  variance_x_hat <- sapply(prediction_hat, function(x) Hmisc::wtd.var(x,weights=df1[,"weights_tot"]))
  covvariance_hat <- correlations_hat * sqrt(variance_x_hat*variance_logincome1)
  decomposition_inequality_hat <- covvariance_hat/variance_logincome1

  #decomposition change
  price_absolute <- (decomposition_inequality2[c(1:length(decomposition_inequality2)-1)]* variance_logincome2)- (decomposition_inequality_hat[c(1:length(decomposition_inequality_hat)-1)]*variance_logincome_hat)
  quantity_absolute <- (decomposition_inequality_hat[c(1:length(decomposition_inequality_hat)-1)]*variance_logincome_hat) - (decomposition_inequality1[c(1:length(decomposition_inequality1)-1)]* variance_logincome1)
  residual_absolute <-  (decomposition_inequality2[length(decomposition_inequality2)]* variance_logincome2)- (decomposition_inequality1[length(decomposition_inequality1)]*variance_logincome1)

  price_relative <- price_absolute/(variance_logincome2-variance_logincome1)
  quantity_relative <- quantity_absolute/(variance_logincome2-variance_logincome1)
  residual_relative <- residual_absolute/(variance_logincome2-variance_logincome1)

  if (variance_logincome2-variance_logincome1<0){
    price_relative <- price_relative*-1
    quantity_relative <- quantity_relative*-1
    residual_relative <- residual_relative*-1
  }

  return(list(attention=attention,
              variance_logincome=c(variance_logincome1=variance_logincome1,variance_logincome2=variance_logincome2,
                                   change=variance_logincome2-variance_logincome1),
              decomposition_inequality=cbind(decomposition_inequality1,decomposition_inequality2),
              decomposition_change_absolute=list(cbind(price_absolute,quantity_absolute),residual_absolute),
              decomposition_change_relative=list(cbind(price_relative,quantity_relative),residual_relative),
              notes=c(note_data1=note1, note_data2=note2)))
}
