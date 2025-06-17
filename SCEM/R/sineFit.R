#' @title Cosine model fitting
#'
#' @description This function performs the updated nonlinear least squares (NLS) regression method for the cosine model (see Chazin et al. 2019).
#'
#' @param data A data frame that contains the data for one individual. There should be two columns
#' with names 'distance' and 'oxygen'.
#'
#' @param amplitude Initial value for the amplitude parameter for the \code{method="initial"} method.
#'
#' @param intercept Initial value for the intercept parameter for the \code{method="initial"} method.
#'
#' @param method A character string giving the initialization for the nonlinear least squares regression. This must be either \code{method="initial"} or \code{method="OLS"}. Default is \code{method="OLS"} method. \code{method="initial"} performs the nonlinear least squares (NLS) regression method for the cosine model without initializing parameter selections. It begins with the given initial values for amplitude and intercept. \code{method="OLS"} uses the least squares estimates (see Chazin et al. 2019) as the initial parameter selection.
#'
#' @export
#'
#' @import stats
#'
#' @returns
#'
#' A fitted model object from the nls function in R:
#'
#' \item{m}{an 'nlsModel' object incorporating the model.}
#' \item{convInfo}{a list with convergence information}
#' \item{data}{the expression that was passed to 'nls' as the data argument. The actual data values are present in the environment of the 'm' component.}
#' \item{call}{the matched call with several components, notably 'algorithm'}
#' \item{dataClasses}{the '"dataClasses"' attribute (if any) of the '"terms"' attribute of the model frame.}
#' \item{control}{the control 'list' used}
#'
#' @references
#'
#' Florent Baty, Christian Ritz, Sandrine Charles, Martin Brutsche, Jean-Pierre Flandrois, Marie-Laure Delignette-Muller (2015). A Toolbox for Nonlinear Regression in R: The Package nlstools. Journal of Statistical Software, 66(5), 1-21. URL http://www.jstatsoft.org/v66/i05/.
#'
#' @examples
#' armenia_split = split(armenia,f = armenia$ID)
#' amp = seq(1,10,by=0.5)
#' int = seq(-25,0,by=0.5)
#' sineFit(armenia_split[[2]],amp[3],int[4],method = "initial")
#' sineFit(armenia_split[[1]],method = "OLS")


sineFit = function(data,amplitude=NULL,intercept=NULL,method = c("OLS", "initial")){
  if (method =="OLS") {sine_OLS(data)}
  else {sine_initial(data,amplitude=amplitude,intercept = intercept)}
}


