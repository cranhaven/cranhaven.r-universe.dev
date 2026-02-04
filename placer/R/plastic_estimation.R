#------------------------------------------------------------------------------
# Main functions to estimate plastic prevalence probability in seabird's nests
# as a function of different sample sizes and thier corresponding confidence
# intervals.
#
# Developed by: Davi Castro Tavares
# Date: May 2018
# Modified by: Esteban Acevedo-Trejos
# Date: July 2019
#------------------------------------------------------------------------------


#' Confidence intervals of plastic prevalence probability
#'
#' Bootstrap simulations to estimate 95\% bootstrapped CIs for the
#' prevalence of debris obtained with different sample sizes.
#'
#' @param plastic_abs_pres numeric vector, containing a binary values with 0
#'   or no for absence of plastic, and 1 or yes for presence of plastic.
#' @param max_sample_size integer, specifying the maximum number of
#'   samples to use for estimating the prevalence of plastic debris.
#'   By default 300 samples. Increasing sample sizes substantially increases
#'   computational time.
#' @param bs_rep integer, specifying the number of bootstrap replications.
#'   By default 1000 replications.
#' @param lower_ci numeric, specifying lower confidence interval.
#'   By default 2.5\%, based on Efron and Tibshirani (1993)
#' @param upper_ci numeric, specifying upper confidence interval.
#'   By default 97.5\% default, based on Efron and Tibshirani (1993).
#' @return A list (cidtf) with a data frame with sample sizes, mean CI,
#' lower CI, upper CI, and a matrix (prevprob) with prevalence probability
#' of plastic debris for all sample sizes and their estimated prevalence of debris.
#' @references Efron, B., & Tibshirani, R. (1993). An introduction to the Bootstrap.
#'  Boca Raton: Chapman & Hall.
#' @seealso \code{\link{plastic.prev.prob}}, \code{\link{prevalence_plot}}
#' @note The confidence intervals are calculated in a sequence of varying
#' sample sizes, i.e. 1,2,3...,n and the function can be also used for defining
#' sample sizes that would provide 95\% CIs with the desired accuracy.
#' @examples
#' plastic.ci(rbinom(1000,1,0.5), 30, 100)
#' @export
plastic.ci <- function(plastic_abs_pres, max_sample_size = 300, bs_rep = 1000,
                       lower_ci = 0.025, upper_ci = 0.975){

  outmat <- matrix(nrow = max_sample_size, ncol = bs_rep)
  for(ss in 1:max_sample_size){
    outmat[ss,] <- replicate(bs_rep, plastic.prev.prob(plastic_abs_pres, ss))
  }
  ci <- apply(outmat, 1, function(x) quantile(x, probs = c(lower_ci, upper_ci)))
  mean_ci <- apply(outmat, 1, function(x) mean(x))
  cidtf <- data.frame(N = 1:max_sample_size, mean_ci = mean_ci,
                      lower_ci = ci[1,], upper_ci = ci[2,])

  return(list(cidtf=cidtf, prevprob=outmat))
}


#' Plastic prevalence probability
#'
#' \code{plastic.prev.prob} estimates the prevalence probability of plastic
#' from a randomly selected sample of absence/presence observations of plastic
#' debris.
#'
#' @param plastic_abs_pres numeric vector, containing a binary values with 0
#'   or 'no' for absence of plastic, and 1 or 'yes' for presence of plastic.
#' @param num_sample integer value, specifying the number of samples to
#'   randomly draw from the observations.
#' @return Prevalence probability of plastic debris in a given sample size.
#' @seealso \code{\link{plastic.ci}}, \code{\link{prevalence_plot}}
#' @examples
#' plastic.prev.prob(rbinom(1000,1,0.5), 1)
#' plastic.prev.prob(rbinom(1000,1,0.5), 10)
#' @importFrom stats quantile
#' @export
plastic.prev.prob <- function(plastic_abs_pres, num_sample){
  if(length(unique(plastic_abs_pres)) != 2){
    # check if a given variable is binary and in the adequate binary format
    stop('input variable plastic_abs_pre is not a binary varible')
  } else if(all(sort(unique(plastic_abs_pres)) != c(0,1)) &
            all(sort(unique(plastic_abs_pres)) != c('no','yes'))){
    stop('values of bynary variable must be 0 or no (for absence)
         and 1 or yes (for presence)' )
  } else {
    # Draw a random sample from the observations of plastic debris and
    # calculates the prevalence probability of plastic in this sample
    rand_sample <- dplyr::sample_n(data.frame(plastic_abs_pres),
                                   num_sample, replace = TRUE)
    rand_sample_pres <- subset(rand_sample,
                               rand_sample == 1 | rand_sample == 'yes')
    plastic_prev <- nrow(rand_sample_pres) / nrow(rand_sample)
    return(plastic_prev)
  }
}

