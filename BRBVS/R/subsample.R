
#' Subsample Function
#'
#' This function generates a matrix of random samples from a given range of integers.
#'
#' @param n Sample size
#' @param m Subsample size
#' @param B Number of times the subsample must be extracted
#'
#' @return A matrix of an array object
#' @noRd
subsample <- function(n, m, B) {
  # Check if n is a positive integer
  if (!is.numeric(n) || n < 1) {
    stop("Argument 'n' must be a positive integer.")
  }

  # Check if m is a positive integer
  if (!is.numeric(m) || m < 1) {
    stop("Argument 'm' must be a positive integer.")
  }

  # Check if B is a positive integer
  if (!is.numeric(B) || B < 1) {
    stop("Argument 'B' must be a positive integer.")
  }

  # Check if B is a positive integer
  if (n < m) {
    stop("n must be greater of equal of m.")
  }

  # Calculate the number of times to repeat each sample
  r <- base::floor(n / m)
  r.t.m <- r * m

  # Create an empty matrix to store the results
  res <- base::matrix(0, nrow = r.t.m, ncol = B)

  # Generate 'B' columns of random samples
  for (i in 1:B) {
    res[, i] <- base::sample.int(n, r.t.m)
  }

  # Reshape the result matrix to have 'm' rows
  base::matrix(as.integer(res), nrow = m, byrow = FALSE)
}
