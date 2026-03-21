#' @title Binary Indicator for Multi-state RDT with Multiple Periods
#'
#' @description Define the binary indicator function to check whether the failure probability satisfies the lower level reliability requirements for each cumulative period (for Multi-state RDT, Multiple Periods)
#'
#' @param pivec Failure probability for each separate period.
#' @param Rvec Lower level reliability requirements for each cumulative period from the begining of the test.
#' @return 0 -- No; 1 -- Yes.
#' @examples
#' MP_Indicator(pivec = c(0.1, 0.2), Rvec = c(0.8, 0.6))
#' MP_Indicator(pivec = c(0.1, 0.2, 0.1), Rvec = c(0.8, 0.6, 0.4))
#' MP_Indicator(pivec = c(0.1, 0.3), Rvec = c(0.8, 0.7))
#' @export

######define the indicator function
MP_Indicator <- function(pivec, Rvec)
{
  condition <- sum(cumsum(pivec[(1:length(Rvec))]) <= 1 - Rvec) == length(Rvec)
  if(condition)
    return(1)
  else
    return(0)
}
