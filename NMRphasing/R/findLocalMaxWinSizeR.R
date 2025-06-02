#' A function primarily aimed at developers
#' This function is adapted from 'find_local_maximum.c' in the MassSpecWavelet package
#' @importFrom stats filter
#' @noRd
findLocalMaxWinSizeR <- function(data, min_width = 5, threshold = 0, capWinSize = 50) {
  n <- length(data)
  if (n < min_width * 2) stop("Data length is too short for the given minimum window width.")

  # Function to compute moving average and find local maxima for a specific window size
  findMaximaForWinSize <- function(win_size) {
    ma <- stats::filter(data, rep(1/win_size, win_size), sides = 2)  # Moving average
    local_maxima <- numeric(0)  # Initialize the local maxima indices

    # Iterate over the data to find local maxima
    for (i in (min_width + 1):(n - min_width)) {
      left <- ma[(i - min_width):(i - 1)]
      right <- ma[(i + 1):(i + min_width)]

      # Check if current point is greater than the neighbors
      if (ma[i] > max(left) && ma[i] > max(right) && ma[i] > threshold) {
        local_maxima <- c(local_maxima, i)
      }
    }

    return(local_maxima)
  }

  # Apply the local maxima finding function across multiple window sizes
  result <- lapply(min_width:capWinSize, findMaximaForWinSize)

  # Return the results as a list where each entry corresponds to a window size
  names(result) <- as.character(min_width:capWinSize)
  return(result)
}
