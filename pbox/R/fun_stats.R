#' Summary Statistics
#'
#' Computes summary statistics for a numeric vector. This function is an S4 method
#' for the generic 'fun_stats', specifically tailored for numeric vectors. It calculates
#' the minimum, maximum, mean, and median values.
#'
#' @name fun_stats
#' @docType methods
#' @rdname fun_stats-methods
#' @export
#' @aliases fun_stats,numeric
#' @usage fun_stats(x)
#' @param x A numeric vector for which summary statistics are to be computed.
#'
#' @return A list containing the minimum, maximum, mean, and median of the input vector.
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' fun_stats(x)
#'
setGeneric("fun_stats", function(x) {
  standardGeneric("fun_stats")
})

#' Summary statistics method for numeric vectors
#'
#' This method is a specific implementation of the 'fun_stats' function for numeric vectors.
#' It efficiently calculates and returns summary statistics including the minimum, maximum,
#' mean, and median, excluding NA values.
#'
#' @param x Numeric vector for which summary statistics are computed.
#' @return A list with components min, max, mean, and median.
#' @export
setMethod("fun_stats", "numeric",
          function(x) {
            # Compute summary statistics, handling NA values
            min_val <- min(x, na.rm = TRUE)
            max_val <- max(x, na.rm = TRUE)
            mean_val <- mean(x, na.rm = TRUE)
            median_val <- median(x, na.rm = TRUE)

            # Compile the results into a list
            summary <- list(min = min_val, max = max_val, mean = mean_val, median = median_val)
            return(summary)
          })
