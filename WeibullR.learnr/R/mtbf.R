#' Mean Time To Failure (MTTF).
#'
#' @param failures Failures. A numeric value representing the number of failures for a given period.
#' @param totalTime Total Time. A numeric value representing the total time for a given period
#' or a numeric vector of time periods
#' @return The function returns a numeric value representing the MTTF for a given period.
#' @examples
#' fail <- 5
#' total <- 1000
#' mttf(fail, total)
#' @export
mttf <- function(failures, totalTime)

{

  # Check for a numeric value of failures
  if(all(!is.null(failures), !is.numeric(failures))) {
    stop("Argument \"failures\" must be a numeric vector")
  }

  # Check for a numeric vector of time periods
  if(all(!is.null(totalTime), !is.numeric(totalTime))) {
    stop("Argument \"totalTime\" must be a numeric vector")
  }

  # Calculate reliability
  mttf <-  totalTime/failures
  print(mttf)

}
