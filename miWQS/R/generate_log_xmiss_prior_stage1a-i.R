#------------------------------------------------------------------------------------------------------------------
##  Univariate Bayesian MI :  (1a - i) Priors for MCMC chains
##  Date Created:
##  Last Updated: April 6, 2018

## Purpose:  The following functions create the priors for the MCMC chains:

## Notes:  Currently only one function and prior here.
## I can add more and this is source code that does it.

# Log
# \item
# \item   5/7/18: SENT to David Wheeler, PhD.
# \item   6/14/18: Added example for multivariate data.
##########################################################################################################################

# #  Accessory Function: Prior or Initial Values for log(Xmiss)
#
# # @family imputation
#
# # @description Gives initial values for the log(X_miss) of each chemical used in
#  Bayesian imputation. Default: uniform. Recommended to set the seed before the function,
#  especially if using uniform.
# # @details
#  The \code{generate.log.xmiss.prior} function gives prior values for the log(X_miss) of each chemical used in
#  Bayesian imputation. \emph{x.miss.type} gives two different priors can be placed on log of values below the detection limit
#   (BDLs): \describe{
#   {uniform}{\deqn{  log( X_{miss,i,j} ) \sim^{iid} Unif(0,DL_j ) }}
#   {vague}{\deqn{  log( X_{miss,i,j}) =  DL_j/2} for all \eqn{i=1,...n_{0}} missing values}
#   }

# # @param x.miss.type = c("uniform","vague") The type of prior to use. Default: uniform.
# DEFUNCT '@param show.hist Logical, whether to plot a histogram of the prior or not using \pkg{ggplot2}. Default: FALSE.
# DEFUNCT # @param binwidth : the value that determines the width of the bins in the histogram. Defaults to 10, no effect if show.hist = FALSE.
# # @param n0 A vector of the number of missing data n0 for each chemical to generate.  Passed from other functions (default).
# # @param DL A vector of detection limit values DL for each chemical. Passed from other functions (default).
# DEFUNCT # @param debug: logical -- whether to print more information or not. Default: FALSE.

# # @return A list of initial values for those missing values.

# # @examples
#  #Functions mysummary is used here from makeJournalTables package.
#
#
#  #Example 1: Silly Example ------
#  set.seed(123)
#  log.x.miss.uniform <- generate.log.xmiss.prior( x.miss.type = "uniform",
#                                          n0 = c(10,6), DL = c(2,3)
#                                          )
#  log.x.miss.vague <- generate.log.xmiss.prior( x.miss.type = "vague",
#                                       n0 = c(10,6), DL = c(2,3)
#                                       )
#  lapply(log.x.miss.uniform, my.summary)
#  lapply(log.x.miss.vague, my.summary)
#
#  # Example 2: What I'd actually use for a simulation study. ------
#  data(simdata87); data(Params_Scenario)
#  print(Params_Scenario)
#  log.x.miss.uniform <- generate.log.xmiss.prior ( x.miss.type =  "uniform" ,
#                                          n0 = l.data$n0 , DL = simdata87$DL)
#  log.x.miss.vague <- generate.log.xmiss.prior ( x.miss.type =  "vague" ,
#                                        n0 = l.data$n0 , DL = simdata87$DL)
#  lapply(log.x.miss.vague, my.summary)
#  lapply(log.x.miss.uniform, my.summary)

# # export #Helper function and not called by user


generate.log.xmiss.prior <- function(x.miss.type = c("uniform", "vague"),
                                     n0, DL) {  # passed from other functions

  # Checks if there are any NA's in the Detection Limit
  stopifnot(sum(is.na(DL)) == 0)
  x.miss.type <- match.arg (x.miss.type)  # Sets the default type to be first word in string.
  c <- length(DL)  # Number of chemicals.

  if (tolower(x.miss.type)  == "uniform") {
    log.x <- vector(mode = "list", length = c)
    for (j in 1:c) {
      log.x[[j]] <- log(runif(n0[j], 0, DL[j]))
    }
  }

  if (tolower(x.miss.type)  == "vague") {
    log.x <- vector(mode = "list", length = c)
    for (j in 1:c) {
      log.x[[j]] <-  rep(log(DL[j]) / sqrt(2), n0[j])
    }
  }

  names(log.x) <- names(DL)  # 6/14/18: Added names of log(x)
  return(log.x)
}



### Save image ====================================
# save.image(file = "~/VCU Biostatistics/as work/UniBayesMI/Stage_1_Imputation_Model_R/1ai_log_x_miss_prior_function.RData")
# cat("> The file is saved in ", getwd(), "\n")
