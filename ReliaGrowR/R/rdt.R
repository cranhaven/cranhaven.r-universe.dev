#' Reliability Demonstration Test (RDT) Plan Calculator
#'
#' This function calculates the required test time or sample size for a Reliability
#' Demonstration Test (RDT) based on specified reliability, mission time, confidence
#' level, and Weibull shape parameter.
#'
#' @srrstats {G1.2} The Life Cycle Statement is in the CONTRIBUTING.md file.
#' @srrstats {G1.3} All statistical terminology is explicitly defined in the documentation.
#' @srrstats {G1.4} `roxygen2`](https://roxygen2.r-lib.org/) documentation is used
#' to document all functions.
#' @srrstats {G2.0} Inputs are validated for length.
#' @srrstats {G2.1} Inputs are validated for type.
#' @srrstats {G2.2} Multivariate inputs are prohibited where only univariate are allowed.
#' @srrstats {G2.6} One-dimensional inputs are appropriately pre-processed.
#' @srrstats {G2.8} Sub-function `print.rdt` is provided for the `rdt` class.
#' @srrstats {G2.13} The function checks for missing data and errors if any is found.
#' @srrstats {G2.14} See sub-tags for responses.
#' @srrstats {G2.14a} Missing data results in an error.
#' @srrstats {G2.14b} Missing data results in an error.
#' @srrstats {G2.14c} Missing data results in an error.
#' @srrstats {G2.15} The function checks for missing data and errors if any is found.
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
#' @srrstats {G5.9} Unit tests include noise susceptibility tests for expected stochastic behavior.
#' @srrstats {G5.9a} Unit tests check that adding trivial noise to data does not meaningfully change results.
#' @srrstats {G5.10} All unit tests run as part of continuous integration.
#'
#' @param target Required reliability at mission time (0 < target < 1).
#' @param mission_time Mission duration (time units). Must be greater than 0.
#' @param conf_level Desired confidence level (e.g., 0.9 for 90% confidence). The
#' confidence level must be between 0 and 1 (exclusive).
#' @param beta Weibull shape parameter (beta=1 corresponds to exponential distribution).
#' Must be greater than 0. Default is 1.
#' @param n Sample size (optional, supply if solving for test_time). Must be a positive integer.
#' @param test_time Test time per unit (optional, supply if solving for n). Must be greater than 0.
#' @return The function returns an object of class `rdt` that contains:
#' \item{Distribution}{Type of distribution used (Exponential or Weibull).}
#' \item{Beta}{Weibull shape parameter.}
#' \item{Target_Reliability}{Specified target reliability.}
#' \item{Mission_Time}{Specified mission time.}
#' \item{Required_Test_Time}{Calculated required test time (if n is provided).}
#' \item{Input_Sample_Size}{Provided sample size (if test_time is calculated).}
#' \item{Required_Sample_Size}{Calculated required sample size (if test_time is provided).}
#' \item{Input_Test_Time}{Provided test time (if n is calculated).}
#'
#' @examples
#' #' # Example 1: Calculate required test time
#' plan1 <- rdt(target = 0.9, mission_time = 1000, conf_level = 0.9, beta = 1, n = 10)
#' print(plan1)
#' # Example 2: Calculate required sample size
#' plan2 <- rdt(target = 0.9, mission_time = 1000, conf_level = 0.9, beta = 1, test_time = 2000)
#' print(plan2)
#' @export
rdt <- function(target, mission_time, conf_level,
                beta = 1, n = NULL, test_time = NULL) {
  # Input validation
  if (!is.numeric(target) || length(target) != 1 || !is.finite(target)) {
    stop("'target' must be a single finite numeric value.")
  }
  if (target <= 0 || target >= 1) {
    stop("'target' must be between 0 and 1 (exclusive).")
  }

  if (!is.numeric(mission_time) || length(mission_time) != 1 || !is.finite(mission_time)) {
    stop("'mission_time' must be a single finite numeric value.")
  }
  if (mission_time <= 0) {
    stop("'mission_time' must be greater than 0.")
  }

  if (!is.numeric(conf_level) || length(conf_level) != 1 || !is.finite(conf_level)) {
    stop("'conf_level' must be a single finite numeric value.")
  }
  if (conf_level <= 0 || conf_level >= 1) {
    stop("'conf_level' must be between 0 and 1 (exclusive).")
  }

  if (!is.numeric(beta) || length(beta) != 1 || !is.finite(beta)) {
    stop("'beta' must be a single finite numeric value.")
  }
  if (beta <= 0) {
    stop("'beta' must be greater than 0.")
  }

  if (!is.null(n) && !is.null(test_time)) {
    stop("Provide only one of 'n' (sample size) or 'test_time', not both.")
  }
  if (is.null(n) && is.null(test_time)) {
    stop("Provide exactly one of 'n' (sample size) or 'test_time'.")
  }

  if (!is.null(n)) {
    if (!is.numeric(n) || length(n) != 1 || !is.finite(n)) {
      stop("'n' must be a single finite numeric value.")
    }
    if (n <= 0 || n != floor(n)) {
      stop("'n' must be a positive integer.")
    }
  }

  if (!is.null(test_time)) {
    if (!is.numeric(test_time) || length(test_time) != 1 || !is.finite(test_time)) {
      stop("'test_time' must be a single finite numeric value.")
    }
    if (test_time <= 0) {
      stop("'test_time' must be greater than 0.")
    }
  }

  # Scale parameter under H0 (Weibull with specified beta)
  eta0 <- mission_time / (-log(target))^(1 / beta)

  if (!is.null(n) & is.null(test_time)) {
    # Solve for required test time
    T_req <- eta0 * ((-log(1 - conf_level)) / n)^(1 / beta)
    result <- list(
      Distribution = ifelse(beta == 1, "Exponential", "Weibull"),
      Beta = beta,
      Target_Reliability = target,
      Mission_Time = mission_time,
      Required_Test_Time = T_req,
      Input_Sample_Size = n
    )
  } else if (is.null(n) & !is.null(test_time)) {
    # Solve for required sample size
    n_req <- ceiling((-log(1 - conf_level)) / ((test_time / eta0)^beta))
    result <- list(
      Distribution = ifelse(beta == 1, "Exponential", "Weibull"),
      Beta = beta,
      Target_Reliability = target,
      Mission_Time = mission_time,
      Required_Sample_Size = n_req,
      Input_Test_Time = test_time
    )
  } else {
    stop("Please provide exactly one of n (sample size) or test_time (test time).")
  }

  class(result) <- "rdt"
  return(result)
}

#' Print method for rdt objects
#'
#' This function provides a formatted print method for objects of class `rdt`.
#'
#' @srrstats {G1.2} The Life Cycle Statement is in the CONTRIBUTING.md file.
#' @srrstats {G1.3} All statistical terminology is explicitly defined in the documentation.
#' @srrstats {G1.4} `roxygen2`](https://roxygen2.r-lib.org/) documentation is used
#' to document all functions.
#' @srrstats {G2.0} Inputs are validated for length.
#' @srrstats {G2.1} Inputs are validated for type.
#' @srrstats {G2.2} Multivariate inputs are prohibited where only univariate are allowed.
#' @srrstats {G2.6} One-dimensional inputs are appropriately pre-processed.
#' @srrstats {G2.8} Sub-function `print.rdt` is provided for the `rdt` class.
#' @srrstats {G2.13} The function checks for missing data and errors if any is found.
#' @srrstats {G2.14} See sub-tags for responses.
#' @srrstats {G2.14a} Missing data results in an error.
#' @srrstats {G2.14b} Missing data results in an error.
#' @srrstats {G2.14c} Missing data results in an error.
#' @srrstats {G2.15} The function checks for missing data and errors if any is found.
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
#' @srrstats {G5.9} Unit tests include noise susceptibility tests for expected stochastic behavior.
#' @srrstats {G5.9a} Unit tests check that adding trivial noise to data does not meaningfully change results.
#' @srrstats {G5.10} All unit tests run as part of continuous integration.
#'
#' @param x An object of class `rdt`.
#' @param ... Additional arguments (not used).
#' @examples
#' plan <- rdt(target = 0.9, mission_time = 1000, conf_level = 0.9, beta = 1, n = 10)
#' print(plan)
#' @return Invisibly returns the input object.
#' @export
print.rdt <- function(x, ...) {
  # Input validation
  if (!inherits(x, "rdt")) {
    stop("'x' must be an object of class 'rdt'.")
  }

  cat("Reliability Demonstration Test (RDT) Plan\n")
  cat("-----------------------------------------\n")
  cat("Distribution: ", x$Distribution, "\n")
  cat("Weibull Shape Parameter (Beta): ", x$Beta, "\n")
  cat("Target Reliability: ", x$Target_Reliability, "\n")
  cat("Mission Time: ", x$Mission_Time, "\n")

  if (!is.null(x$Required_Test_Time)) {
    cat("Input Sample Size (n): ", x$Input_Sample_Size, "\n")
    cat("Required Test Time (T): ", round(x$Required_Test_Time, 2), "\n")
  } else if (!is.null(x$Required_Sample_Size)) {
    cat("Input Test Time (T): ", x$Input_Test_Time, "\n")
    cat("Required Sample Size (n): ", x$Required_Sample_Size, "\n")
  }
}
