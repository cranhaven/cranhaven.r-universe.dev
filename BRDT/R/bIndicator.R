#' @title Binary Indicator for Binomial RDT
#'
#' @description Define the binary indicator function to check whether the failure probability satisfies the lower level reliability requirement (for binomial RDT).
#'
#' @param pi Failure probability.
#' @param R Lower Level reliability requirement.
#' @return 0 -- No; 1 -- Yes.
#' @examples
#' bIndicator(pi = 0.05, R = 0.9)
#' bIndicator(pi = 0.2, R = 0.9)
#' @export
#' @seealso \code{\link{bcore}} for getting the core probability of passting the test;
#' \code{\link{boptimal_n}} for getting the optimal test sample size;
#' \code{\link{bconsumerrisk}} for getting the consumer's risk;



######define the indicator function
bIndicator <- function(pi, R){
  if(pi <= (1 - R)){
    return(1)
  } else {
    return(0)
  }
}

