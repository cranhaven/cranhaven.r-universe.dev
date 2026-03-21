#' @title Binary Indicator for Multi-state RDT with Multiple Failure Modes (MFM)
#'
#' @description Define the binary indicator function to check whether the failure probability satisfies the lower level reliability requirements for each failure mode (for Multi-state RDT, Multiple Failure Models)
#'
#' @param pivec Failure probability for each separate period.
#' @param Rvec Lower level reliability requirements for each cumulative period from the begining of the test.
#' @return 0 -- No; 1 -- Yes.
#' @examples
#' MFM_Indicator(pivec = c(0.1, 0.2), Rvec = c(0.8, 0.6))
#' MFM_Indicator(pivec = c(0.1, 0.2, 0.1), Rvec = c(0.8, 0.6, 0.4))
#' MFM_Indicator(pivec = c(0.1, 0.4), Rvec = c(0.8, 0.7))
#' @export
#' @family MSRDT for MFM functions
#' @seealso \code{\link{MFM_core}} for getting the core probability of passting the test;
#' \code{\link{MFM_consumerrisk}} for getting the consumer's risk;
#' \code{\link{MFM_optimal_n}} for getting the optimal test sample size;


######define the indicator function
MFM_Indicator <- function(pivec, Rvec)
{
  if(all(pivec <= 1 - Rvec))
    return(1)
  else
    return(0)
}
