#' @title Parameter estimates from a nonlinear lest squares (NLS) fit.
#'
#' @description This function converts the estimated parameters from the non-linear least squares (NLS) model fit to the appropriate parameter space corresponding to the cosine model proposed by Balasse et al (2012).
#'
#' @param curve A fitted model object from nls function. The fitted model should have the following
#' parameter estimates - amplitude, intercept, frequency, phase.
#'
#' @export
#'
#' @import stats
#'
#'
#' @returns
#'
#' A list containing the following components:
#'
#' \item{amplitude}{estimated amplitude}
#' \item{intercept}{estimated intercept}
#' \item{x0}{delay of the data}
#' \item{X}{period of the data}
#' \item{birth}{birth seasonality estimate}
#'
#' @examples
#' armenia_split = split(armenia,f = armenia$ID)
#' curve = sineFit(armenia_split[[1]],method = "OLS")
#' convertParameters(curve)

convertParameters = function(curve) {

  coefs = stats::coef(curve)
  amplitude = coefs['amplitude']
  intercept = coefs['intercept']
  freq = coefs['frequency']
  phase = coefs['phase']

  # ensure that X is positive (sine is an odd function)
  if (coefs['amplitude']<0){
    amplitude = - coefs['amplitude']
    phase = phase - pi
  }
  if (freq<0){
    X = -2*pi/freq
    nn = floor(-phase/(2*pi))
    birth = -phase/(2*pi) - nn
    x0 = birth*X
  }else{
    X = 2*pi/freq
    nn = floor(phase/(2*pi)) + 1
    birth = - phase/(2*pi) + nn
    x0 = birth*X
  }

  return(list(amplitude = amplitude,intercept = intercept,x0 = x0,X = X,birth = birth))

}
