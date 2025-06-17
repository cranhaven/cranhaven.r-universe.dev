#' @title Cosine model fitting with given initialization for two parameters.
#'
#' @description Performs the updated nonlinear least squares (NLS) regression method for the cosine
#' model proposed by Balasse et al. The method calculates with the proposed initial values for ampliitude and intercept,
#' and then fits the NLS method as required.
#'
#' @param data A data frame that contains the data for one individual. There should be two columns
#' with names 'distance' and 'oxygen'.
#'
#' @param amplitude Initial value for the amplitude parameter.
#'
#' @param intercept Initial value for the intercept parameter.
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
#'
#' @examples
#' armenia_split = split(armenia,f = armenia$ID)
#' amp = seq(1,10,by=0.5)
#' int = seq(-25,0,by=0.5)
#' sine_initial(armenia_split[[2]],amp[3],int[4])

sine_initial = function(data,
                        amplitude,
                        intercept) {

  if (!any(colnames(data)==c("distance","oxygen"))) {stop('data frame does not contain distance and oxygen columns')}
  if (! is.atomic(amplitude) || !length(amplitude)==1) {stop('amplitude needs to be a single value')}
  if (! is.atomic(intercept) || !length(intercept)==1) {stop('intercept needs to be a single value')}
  if (any(is.na(data))) {stop('Data has NA values')}

  frequency = 2*pi/max(data$distance)
  model = stats::lm(oxygen ~ sin(frequency*distance) + cos(frequency*distance),data = data)
  a = model$coef[2]
  b = model$coef[3]
  phase = -atan(a/b)

  start = list(intercept = intercept, amplitude = amplitude,phase = phase,frequency = frequency)
  curve = stats::nls(oxygen ~ intercept + amplitude*cos(frequency*distance+phase),
              data = data,start = start,control = nls.control(warnOnly=TRUE))

  return(curve)

}
