#' Plot traceplots for model terms.
#'
#' This function takes a \code{fergm} object and plots the time series of each chain per model term using ggplot2.
#' @param fergm.fit A model object returned by the \code{fergm} function.  Must be specified.
#' @param custom_var_names A vector of custom variable names used in presentation that match the order of the \code{form} object passed to \code{fergm}.  If not provided, defaults to names inherited by \code{fergm.fit}.
#' @return This prints a ggplot2 traceplot for the effects of interest.
#' @keywords FERGM interpret summary
#' @references Box-Steffensmeier, Janet M., Dino P. Christenson, and Jason W. Morgan. 2018. ``Modeling Unobserved Heterogeneity in Social Networks with the Frailty Exponential Random Graph Model." \emph{Political Analysis}. (26)1:3-19.
#' @references Stan Development Team (2016). RStan: the R interface to Stan. R package version 2.14.1. \url{http://mc-stan.org/}.
#' @examples
#' # load example data
#' data("ergm.fit")
#' data("fergm.fit")
#' data("mesa")
#' # Use rstan's built in traceplot function
#' trace <- rstan::traceplot(fergm.fit$stan.fit, pars = "beta")
#' trace
#'
#' # We have our own version that includes variable names and tidies it up a bit
#' fergm_beta_traceplot(fergm.fit,
#'                     custom_var_names =  c("Edges", "Sex Homophily",
#'                     "Grade Homophily", "Race Homophily", "GWESP", "Alternating K-Stars"))
#
#' @export

fergm_beta_traceplot <- function(fergm.fit = NULL, custom_var_names = NULL){
  trace <- rstan::traceplot(fergm.fit$stan.fit, pars = "beta")

  if(is.null(custom_var_names)){
    custom_var_names <- colnames(fergm.fit$stan.dta$x)
  }

  var_names <- custom_var_names
  levels(trace$data$parameter) <- var_names
  trace$labels$colour <- "Chain"
  trace$labels$x <- "Iteration"
  trace$labels$y <- "Value"




  return(trace)
}
