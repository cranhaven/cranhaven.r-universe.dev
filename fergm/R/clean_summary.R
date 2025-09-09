#' Clean posterior description of FERGM.
#'
#' This function takes a \code{stan} object and return a clean summary of the posterior distribution.
#' @param fergm.fit A model object returned by the \code{fergm} function.  Must be specified.
#' @param custom_var_names A vector of custom variable names used in presentation that match the order of the \code{form} object passed to \code{fergm}.  If not provided, defaults to names inherited by \code{fergm.fit}.
#' @return This function returns a matrix summarizing the posterior distribution, including variable names, posterior means, and 95% credible intervals.
#' @keywords FERGM interpret summary
#' @references Box-Steffensmeier, Janet M., Dino P. Christenson, and Jason W. Morgan. 2018. ``Modeling Unobserved Heterogeneity in Social Networks with the Frailty Exponential Random Graph Model." \emph{Political Analysis}. (26)1:3-19.
#' @references Stan Development Team (2016). RStan: the R interface to Stan. R package version 2.14.1. \url{http://mc-stan.org/}.
#' @examples
#' \dontrun{
#' # The fergm.fit$stan.fit object is of class stanfit.
#'    # We keep it this way such that users can rely upon
#'    # conventional stan functions for interpretation
#'    # getting posterior distributions from the fergm
#'
#'    # Conventional rstan summary call
#'
#'  # load example data
#' data("ergm.fit")
#' data("fergm.fit")
#' data("mesa")
#' stan.smry <- summary(fergm.fit$stan.fit)$summary
#' beta_df <- stan.smry[grep("beta", rownames(stan.smry)),]
#' est <- round(beta_df[,c(1,4,8)], 3)
#'
#'   # We have a built in function to do this simply
#' est <- clean_summary(fergm.fit)
#' est <- clean_summary(fergm.fit,
#' custom_var_names = c("Edges", "Sex Homophily", "Grade Homophily",
#' "Race Homophily", "GWESP", "Alternating K-Stars"))
#' }
#' @export

clean_summary <- function(fergm.fit = NULL, custom_var_names = NULL){
  its <- rstan::extract(fergm.fit$stan.fit)$beta
  form <- fergm.fit$form

  fergm_df <- cbind(as.data.frame(colMeans(its)), as.data.frame(matrixStats::colQuantiles(its, probs = c(0.025, 0.975))))
  colnames(fergm_df)[1] <- "mean"

  if(!is.null(custom_var_names)){
    rownames(fergm_df) <- custom_var_names
  } else {
    rownames(fergm_df) <- colnames(fergm.fit$stan.dta$x)
  }
  return(fergm_df)
}
