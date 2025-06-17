#' @title Prepare results for cosine model fit.
#'
#' @description This function performs the nonlinear least squares (NLS) regression method for the cosine model. It fits the NLS method as required, and then computes different quantities for the birth seasonality estimates corresponding to different individuals.
#'
#' @param paths A list of data frames, where each frame contains the data for one individual. Every
#' data frame should have two columns with names 'distance' and 'oxygen'.
#'
#' @param amplitude Initial value for the amplitude parameter for the \code{method="initial"} method.
#'
#' @param intercept Initial value for the intercept parameter for the \code{method="initial"} method.
#'
#' @param method A character string giving the initialization for the nonlinear least squares regression. This must be either \code{method="initial"} or \code{method="OLS"}. Default is \code{method="OLS"} method. \code{method="initial"} performs the nonlinear least squares (NLS) regression method for the cosine model without initializing parameter selections. It begins with the given initial values for amplitude and intercept. \code{method="OLS"} uses the least squares estimates (see Chazin et al. 2019) as the initial parameter selection.
#'
#' @export
#'
#' @returns
#'
#' A data frame containing the following components:
#'
#' \item{amplitude}{estimated amplitude}
#' \item{intercept}{estimated intercept}
#' \item{x0}{delay of the data}
#' \item{X}{period of the data}
#' \item{birth}{birth seasonality estimate}
#' \item{predictedMin}{predicted minimum for the oxygen isotope variable}
#' \item{predictedMax}{predicted maximum for the oxygen isotope variable}
#' \item{observedMin}{observed minimum for the oxygen isotope variable}
#' \item{observedMax}{observed minimum for the oxygen isotope variable}
#' \item{MSE}{mean squared error corresponding to the model fit for every individual}
#' \item{Pearson}{Pearson's R^2 corresponding to the model fit for every individual}
#'
#' @examples
#' armenia_split = split(armenia,f = armenia$ID)
#' amp = seq(1,10,by=0.5)
#' int = seq(-25,0,by=0.5)
#' makeFits(armenia_split,amp[1],int[1],method = "initial")
#' makeFits(armenia_split, method = "OLS")


makeFits = function(paths,amplitude=NULL,intercept=NULL,method = c("OLS", "initial")){
  if (method =="OLS") {makeFits_OLS(paths)}
  else {makeFits_initial(paths,amplitude=amplitude,intercept = intercept)}
}


