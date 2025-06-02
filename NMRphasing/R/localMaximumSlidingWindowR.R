#' A function primarily aimed at developers.
#' This function was originally from the MassSpecWavelet package.
#' It has been revised and converted from C to R to address specific bugs in the original implementation.
#' The function name has been changed to avoid confusion with the original MassSpecWavelet package functions.
#' @noRd

# R function to detect local maxima using a sliding window
localMaximumSlidingWindowR <- function(x, winSize = 5) {
  # Check if x is a numeric vector
  if (!is.numeric(x)) {
    stop("x must be a numeric vector")
  }

  len <- length(x)

  # Initialize the output vector with zeros (of the same length as x)
  localMax <- rep(0, len)

  # Loop through the vector in sliding windows of size winSize
  for (i in 1:(len - winSize + 1)) {
    window <- x[i:(i + winSize - 1)]  # Subvector of current window
    maxVal <- max(window)
    maxIndex <- which.max(window) + (i - 1)  # Adjust for the starting index

    # Check if the maximum value in the window is greater than both boundaries (left and right)
    if (maxVal > x[i] && maxVal > x[i + winSize - 1]) {
      localMax[maxIndex] <- 1  # Mark as a local max
    }
  }

  # Handle boundaries by shifting the window
  for (i in 1:floor(winSize / 2)) {
    # For the left boundary: use a window of size i to len-i
    window <- c(x[i], x[(len - floor(winSize / 2) + i):(len - i)])
    maxVal <- max(window)
    maxIndex <- which.max(window)

    # Check boundary condition
    if (maxVal > x[i] && maxVal > x[len - i + 1]) {
      localMax[maxIndex] <- 1
    }
  }

  return(localMax)
}
