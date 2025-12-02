#' Weibull to RGA
#'
#' Converts Weibull data (failure, suspension, and interval-censored times)
#' into a format suitable for reliability growth analysis (RGA). The function
#' handles exact failure times, right-censored suspensions, and interval-censored data.
#' It approximates interval-censored failures by placing them at the midpoint of the interval.
#' The output is a data frame with cumulative time and failure counts. This format
#' can be used with RGA models such as Crow-AMSAA.
#'
#' @srrstats {G1.2} The Life Cycle Statement is in the CONTRIBUTING.md file.
#' @srrstats {G1.3} All statistical terminology is explicitly defined in the documentation.
#' @srrstats {G1.4} `roxygen2`](https://roxygen2.r-lib.org/) documentation is used
#' to document all functions.
#' @srrstats {G2.0} Inputs are validated for length.
#' @srrstats {G2.1} Inputs are validated for type.
#' @srrstats {G2.2} Multivariate inputs are prohibited where only univariate are allowed.
#' @srrstats {G2.6} One-dimensional inputs are appropriately pre-processed.
#' @srrstats {G2.13} The function checks for missing data and errors if any is found.
#' @srrstats {G2.14} See sub-tags for responses.
#' @srrstats {G2.14a} Missing data results in an error.
#' @srrstats {G2.14b} Missing data results in an error.
#' @srrstats {G2.14c} Missing data results in an error.
#' @srrstats {G2.15} The function checks for missing data and errors if any is found.
#' @srrstats {G2.16} The function checks for NA and NaN values and errors if any are found.
#' @srrstats {G5.2} Unit tests demonstrate error messages and compare results with expected values.
#' @srrstats {G5.2a} Every message produced by `stop()` is unique.
#' @srrstats {G5.2b} Unit tests demonstrate error messages and compare results with expected values.
#' @srrstats {G5.4} Unit tests include correctness tests to test that statistical
#' algorithms produce expected results to some fixed test data sets.
#' @srrstats {G5.5} Correctness tests are run with a fixed random seed.
#' @srrstats {G5.8} See sub-tags for responses.
#' @srrstats {G5.8a} Unit tests include checks for zero-length data.
#' @srrstats {G5.8b} Unit tests include checks for unsupported data types.
#' @srrstats {G5.8c} Unit tests include checks for data with 'NA' fields.
#' @srrstats {G5.8d} Unit tests include checks for data outside the scope of the algorithm.
#' @srrstats {G5.10} All unit tests run as part of continuous integration.
#'
#' @param failures A numeric vector of exact failure times. Each failure time
#' indicates when an item failed during the observation period.
#' @param suspensions A numeric vector of suspension (right-censored) times. A suspension
#' indicates that the item was removed from observation at that time without failure.
#' This parameter is optional and can be NULL if there are no suspensions.
#' @param interval_starts A numeric vector of interval start times (lower bound of censoring).
#' This parameter is optional and can be NULL if there are no interval-censored data.
#' If provided, it must be the same length as `interval_ends`.
#' @param interval_ends A numeric vector of interval end times (upper bound of censoring).
#' This parameter is optional and can be NULL if there are no interval-censored data.
#' If provided, it must be the same length as `interval_starts`.
#' @return The data frame contains two columns:
#' \item{CumulativeTime}{Cumulative time at each failure event.}
#' \item{Failures}{Number of failures at each cumulative time point.}
#' The function approximates interval-censored failures by placing them at the midpoint
#' of the interval.
#' @family data preparation
#' @examples
#' failures <- c(100, 200, 200, 400)
#' suspensions <- c(250, 350, 450)
#' interval_starts <- c(150, 300)
#' interval_ends <- c(180, 320)
#' result <- weibull_to_rga(failures, suspensions, interval_starts, interval_ends)
#' print(result)
#' @importFrom stats aggregate
#' @export
weibull_to_rga <- function(failures,
                           suspensions = NULL,
                           interval_starts = NULL,
                           interval_ends = NULL) {
  # Validation
  if (any(is.na(failures)) || any(is.nan(failures)) || any(!is.finite(failures))) {
    stop("`failures` contains missing (NA), NaN, or infinite values.")
  }
  if (!is.numeric(failures) || any(failures <= 0)) {
    stop("`failures` must be a numeric vector with positive values.")
  }
  if (any(is.na(suspensions)) || any(is.nan(suspensions)) || any(!is.finite(suspensions))) {
    stop("`suspensions` contains missing (NA), NaN, or infinite values.")
  }
  if (!is.null(suspensions)) {
    if (!is.numeric(suspensions) || any(suspensions <= 0)) {
      stop("`suspensions` must be a numeric vector with positive values.")
    }
  }
  if (!is.null(interval_starts) || !is.null(interval_ends)) {
    if (any(is.na(interval_starts)) || any(is.nan(interval_starts)) || any(!is.finite(interval_starts))) {
      stop("`interval_starts` contains missing (NA), NaN, or infinite values.")
    }
    if (any(is.na(interval_ends)) || any(is.nan(interval_ends)) || any(!is.finite(interval_ends))) {
      stop("`interval_ends` contains missing (NA), NaN, or infinite values.")
    }
    if (is.null(interval_starts) || is.null(interval_ends)) {
      stop("Both `interval_starts` and `interval_ends` must be provided together.")
    }
    if (!is.numeric(interval_starts) || !is.numeric(interval_ends)) {
      stop("Interval bounds must be numeric vectors.")
    }
    if (length(interval_starts) != length(interval_ends)) {
      stop("`interval_starts` and `interval_ends` must have the same length.")
    }
    if (any(interval_starts <= 0) || any(interval_ends <= 0)) {
      stop("Interval bounds must be positive.")
    }

    if (any(interval_starts >= interval_ends)) {
      stop("Each interval start must be strictly less than its corresponding end.")
    }
  }

  # Data prep
  all_times <- c(failures, suspensions)
  all_types <- c(
    rep("Failure", length(failures)),
    rep("Suspension", length(suspensions))
  )

  # Handle interval-censored data (approximate failures at midpoint)
  if (!is.null(interval_starts)) {
    midpoints <- (interval_starts + interval_ends) / 2
    all_times <- c(all_times, midpoints)
    all_types <- c(all_types, rep("IntervalFailure", length(midpoints)))
  }

  data <- data.frame(Time = all_times, Type = all_types)
  data <- data[order(data$Time), ]

  # Cumulative time is just running sum of times in order
  data$CumulativeTime <- cumsum(data$Time)

  # Failure counts by time (Failures + IntervalFailures)
  failure_counts <- stats::aggregate(Type ~ Time,
    data = data,
    FUN = function(x) sum(x %in% c("Failure", "IntervalFailure"))
  )

  result <- merge(failure_counts, data[, c("Time", "CumulativeTime")], by = "Time")
  result <- result[!duplicated(result$Time), ]
  colnames(result)[colnames(result) == "Type"] <- "Failures"

  # Keep only rows where there were actual failures
  result <- result[result$Failures > 0, ]

  # Final columns
  result <- result[, c("CumulativeTime", "Failures")]

  return(result)
}
