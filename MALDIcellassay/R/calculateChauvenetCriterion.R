#' Calculate Chauvenet's criterion for outlier detection
#'
#' @param x numeric, values (e.g. intensities) to test for outliers
#'
#' @details
#' Note that, as for all outlier detection criteria:
#' Excluding data points from your measurement should only be conducted with extreme care.
#' Even if this (or any other) function tells you that a data point is an outlier,
#' you might still want to have it in your sample population especially if you are not sure if your data is normal distributed.
#' See [Wikipedia](https://en.wikipedia.org/wiki/Chauvenet%27s_criterion) for details of the algorithm.
#'
#'
#' @return
#' logical vector, TRUE for detected outliers.
#' @export
#' 
#' @examples
#' set.seed(42) 
#' 
#' #no outlier
#' sample <- rnorm(n = 8, mean = 0, sd = 0.01)
#' calculateChauvenetCriterion(sample)
#' 
#' # introduce outlier
#' sample[1] <- 1
#' calculateChauvenetCriterion(sample)
#' 
#' @importFrom stats qnorm
calculateChauvenetCriterion <- function(x) {
  mean <- mean(x, na.rm = TRUE)
  sd <- sd(x, na.rm = TRUE)

  n <- sum(!is.na(x))

  if(n < 3) {
    warning("Chauvenet's criterion requires at least 3 data points.\n
            No outlier detection performed. Returning FALSE.\n")

    return(FALSE)
  }

  t <- abs(x-mean)/sd
  Dmax <- abs(qnorm(1/(4*n)))

  return(t > Dmax)
}
