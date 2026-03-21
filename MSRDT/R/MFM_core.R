#' @title Probability Core for Multi-state RDT with Multiple Failure Modes (MFM)
#'
#' @description  Define the summed core function inside of the integration which gets the probability of passing the test given specific failure probabilities
#' under different failure modes (for Multi-state RDT, Multiple Failure Modes).
#'
#' @param n RDT sample size
#' @param cvec Maximum allowable failures for each separate period
#' @param pivec Failure probability for each seperate period
#' @return Core probability of passing the test given specific failure probabilities
#' @examples
#' #' #Example for two failure modes
#' pi1 <- pi_MCSim_beta(M = 1000, seed = 10, a = 1, b = 1)
#' pi2 <- pi_MCSim_beta(M = 1000, seed = 10, a = 2, b = 18)
#' MFM_core(n = 10, cvec = c(1, 1), pivec = c(pi1[1], pi2[1]));
#' #The function also works for more than two failure modes.
#' #However, the computation cost may increase.
#' #Example for three failure modes
#' MFM_core(n = 10, cvec = c(1, 1, 1), pivec = c(0.8, 0.9, 0.8));
#' @export
#' @importFrom stats dbinom
#' @family MSRDT for MFM functions
#' @seealso \code{\link{MFM_consumerrisk}} for getting the consumer's risk;
#' \code{\link{MFM_Indicator}} for getting the binary indicator;
#' \code{\link{MFM_optimal_n}} for getting the optimal test sample size;



######define the inside sum core of integration fucntion
MFM_core<-function(n, cvec, pivec){
  if(n <= sum(cvec)){
    return(0)
  }
  else {
    res <- 1
    for (i in 1:length(cvec)){
      res <- res * sum(dbinom(c(0 : cvec[i]), n, pivec[i]))
    }
    return(res)
  }
}
