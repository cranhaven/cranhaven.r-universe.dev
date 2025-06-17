#' @title Prepare results for cosine model fit with proposed initialization.
#'
#' @description Performs the nonlinear least squares (NLS) regression method for the cosine
#' model, with the proposed initialization for all the parameters. It fits the NLS method
#' as required, and then computes different quantities for the birth seasonality estimates
#' corresponding to different individuals.
#'
#' @param paths A list of data frames, where each frame contains the data for one individual. Every
#' data frame should have two columns with names 'distance' and 'oxygen'.
#'
#' @export
#'
#' @import stats
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
#'
#' @examples
#' armenia_split = split(armenia,f = armenia$ID)
#' makeFits_OLS(armenia_split)

makeFits_OLS = function(paths) {

  for(i in 1:length(paths)){
    if (!any(colnames(paths[[i]])==c("distance","oxygen"))) {stop('data frame does not contain columns named distance and oxygen')}
  }
  for(i in 1:length(paths)){if (any(is.na(paths[[i]]))) {stop('Data has NAs')}}

  fits = c()
  for(i in 1:length(paths)) {
    data = paths[[i]]
    curve = sineFit(data, method = "OLS")
    fit = convertParameters(curve)
    fit$predictedMin = fit$intercept - abs(fit$amplitude)
    fit$predictedMax = fit$intercept + abs(fit$amplitude)
    fit$observedMin = min(data$oxygen)
    fit$observedMax = max(data$oxygen)
    fit$MSE = mean((predict(curve) - data$oxygen)^2)
    fit$PearsonCorrelation = stats::cor(predict(curve),paths[[i]]$oxygen,method = "pearson")
    fits = rbind(fits, as.numeric(fit))
  }
  fits = data.frame(fits)
  colnames(fits) = c("amplitude","intercept","x0","X","birth","predictedMin",
                     "predictedMax","observedMin","observedMax","MSE","Pearson")

  return(fits)

}
