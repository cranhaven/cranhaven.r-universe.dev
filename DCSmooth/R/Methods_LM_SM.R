################################################################################
#                                                                              #
#                DCSmooth Package: Methods for SARMA/SFARIMA                   #
#                                                                              #
################################################################################

### Includes all methods for summarizing, printing and plotting classes "sarma"
### and "sfarima"

  # summary.sarma
    # print.summary_sarma
  # summary.sfarima
    # print.summary_sfarima

#--------------------Summary Methods for SARMA Estimation----------------------#

#' Summarizing SARMA/SFARIMA Estimation or Simulation
#' 
#' @description \code{summary} method for class "sarma" or "sfarima"
#' 
#' @param object an object of class "sarma" or "sfarima", usually a result of a
#'  call to the estimation functions \code{\link{sarma.est}}, 
#'  \code{\link{sfarima.est}} or to the corresponding simulation functions 
#'  \code{\link{sarma.sim}} and \code{\link{sfarima.sim}}.
#' @param ... Additional arguments passed to the \code{summary.sarma}/
#' \code{summary.sfarima} function.
#' 
#' @section Details:
#' \code{summary.sarma}/\code{summary.sfarima} strips an object of class
#' "sarma"/"sfarima" from all large matrices (\code{Y}, \code{innov}), allowing
#'  for easier handling of meta-statistics of the bandwidth selection procedure.
#' 
#'  \code{print.summary_sarma}/\code{print.summary_sarma} returns a list of 
#'  summary statistics from the estimation or simulation procedure.
#' 
#' @return The function \code{summary.sarma}/\code{summary.sfarima} returns an 
#' object of class \code{summary_sarma} including \tabular{ll}{
#'  \code{model} \tab estimated or simulated model parameters including 
#'  coefficient matrices \code{ar}, \code{ma}, the error term standard deviation
#'  \code{sigma} and the vector of long memory parameters \code{d} 
#'  (\code{summary.sarma} only)  \cr
#'  \code{model_order} \tab order of the estimated/simulated model computed from 
#'  the matrices \code{ar}, \code{ma}. \cr
#'  \code{stnry} \tab a flag for stationarity of the short memory part. \cr
#'  \code{subclass} \tab a flag indicating whether the object inherits from an
#'  estimation (\code{subclass = "est"}) or simulation procedure 
#'  (\code{subclass = "sim"}). \cr
#' }
#' 
#' @seealso \code{\link{sarma.est}, \link{sfarima.est}, \link{sarma.sim},
#'  \link{sfarima.sim}}
#' 
#' @examples
#' # SARMA Simulation and Estimation
#' ma = matrix(c(1, 0.2, 0.4, 0.1), nrow = 2, ncol = 2)
#' ar = matrix(c(1, 0.5, -0.1, 0.1), nrow = 2, ncol = 2)
#' sigma = 0.5
#' sarma_model = list(ar = ar, ma = ma, sigma = sigma)
#' sarma_sim = sarma.sim(100, 100, model = sarma_model)
#' summary(sarma_sim)
#' sarma_est = sarma.est(sarma_sim$Y)
#' summary(sarma_est)
#' 
#' # SFARIMA Simulation and Estimation
#' ma = matrix(c(1, 0.2, 0.4, 0.1), nrow = 2, ncol = 2)
#' ar = matrix(c(1, 0.5, -0.1, 0.1), nrow = 2, ncol = 2)
#' d = c(0.1, 0.1)
#' sigma = 0.5
#' sfarima_model = list(ar = ar, ma = ma, d = d, sigma = sigma)
#' sfarima_sim = sfarima.sim(100, 100, model = sfarima_model)
#' summary(sfarima_sim)
#' sfarima_est = sfarima.est(sfarima_sim$Y)
#' summary(sfarima_est)
#' 
#' @export
#' 
summary.sarma = function(object, ...)
{
  subclass = attr(object, "subclass")
  model_order = list(ar = dim(object$model$ar) - 1,
                     ma = dim(object$model$ma) - 1)
  ar = object$model$ar
  ma = object$model$ma
  sigma = object$model$sigma
  summary_sarma = list(ar = ar, ma = ma, sigma = sigma,
                       model_order = model_order, stnry = object$stnry,
                       subclass = subclass)
  class(summary_sarma) = "summary_sarma"
  return(summary_sarma)
}

#' Print the Summary of a "sarma"/"sfarima" object 
#' 
#' @description \code{print} methods for class \code{"summary_sarma"}/
#'  \code{"summary_sfarima"}
#' 
#' @param x An object of class \code{"summary_sarma"} or
#'  \code{"summary_sfarima"}.
#' @param ... Additional arguments passed to \code{print.summary_sarma}/
#'  \code{print.summary_sfarima}.
#' 
#' @return No return value.
#' 
#' @seealso \code{\link{summary.sarma}} \code{\link{summary.sfarima}}
#' 
#' @export
print.summary_sarma = function(x, ...)
{
  args_list = list(...)
  if (!exists("digits", args_list))
  {
    digits = max(3, getOption("digits") - 3)
  }
  
  # cat(class(x), "\n")
  cat("------------------------------------------", "\n")
  if (x$subclass == "est")
  {
    cat("Estimation of SARMA", .order.to.string(x$model_order), 
        "\n", sep = "")
  } else if (x$subclass == "sim")
  {
    cat("Simulation of SARMA", .order.to.string(x$model_order), 
        "\n", sep = "")
  }
  cat("------------------------------------------", "\n")
  cat("sigma:\t", signif(x$sigma, digits = digits), "\n")
  cat("stationary:\t", x$stnry, "\n")
  cat("ar:\n")
  print(signif(x$ar, digits = digits))
  cat("\nma:\n")
  print(signif(x$ma, digits = digits))
}

#-------------------Summary Methods for SFARIMA Estimation---------------------#

#' @rdname summary.sarma
#' @export
summary.sfarima = function(object, ...)
{
  subclass = attr(object, "subclass")
  ar = object$model$ar
  ma = object$model$ma
  d = object$model$d
  sigma = object$model$sigma
  model_order = list(ar = dim(object$model$ar) - 1,
                     ma = dim(object$model$ma) - 1)
  summary_sfarima = list(ar = ar, ma = ma, d = d, sigma = sigma, 
                         model_order = model_order, stnry = object$stnry,
                         subclass = subclass)
  class(summary_sfarima) = "summary_sfarima"
  return(summary_sfarima)
}

#' @rdname print.summary_sarma
#' @export
print.summary_sfarima = function(x, ...)
{
  args_list = list(...)
  if (!exists("digits", args_list))
  {
    digits = max(3, getOption("digits") - 3)
  }
  
  # cat(class(x), "\n")
  cat("------------------------------------------", "\n")
  if (x$subclass == "est")
  {
    cat("Estimation of SFARIMA", .order.to.string(x$model_order), 
        "\n", sep = "")
  } else if (x$subclass == "sim")
  {
    cat("Simulation of SFARIMA", .order.to.string(x$model_order), 
        "\n", sep = "")
  }
  cat("------------------------------------------", "\n")
  cat("d:\t\t", signif(x$d, digits = digits), "\n")
  cat("SD (sigma):\t", signif(x$sigma, digits = digits), "\n")
  cat("stationary:\t", x$stnry, "\n")
  cat("ar:\n")
  print(signif(x$ar, digits = digits))
  cat("\nma:\n")
  print(signif(x$ma, digits = digits))
}