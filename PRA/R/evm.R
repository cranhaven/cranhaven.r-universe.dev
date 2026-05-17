#' Planned Value (PV).
#'
#' Calculates the Planned Value (PV) of work completed based on the Budget at Completion
#' (BAC) and the planned schedule.
#'
#' @srrstats {G1.0} *Software lists primary reference from published academic literature.*
#' @srrstats {G1.1} *Software is the first implementation within **R** of the algorithm which has previously been implemented in other languages or contexts.*
#' @srrstats {G1.4} *Software uses [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#' @srrstats {G2.0} *Implements assertions on lengths of inputs - validates single values and non-empty vectors.*
#' @srrstats {G2.0a} *Parameter documentation explicitly states expected input structure.*
#' @srrstats {G2.1} *Implements assertions on types of inputs via is.numeric() checks.*
#' @srrstats {G2.1a} *Parameter documentation explicitly states data types expected.*
#' @srrstats {G2.2} *Prohibits multivariate input for bac and time_period which must be single values.*
#' @srrstats {G2.13} *Implements checks for missing data via anyNA() prior to processing.*
#' @srrstats {G2.14a} *Errors on missing data with informative message.*
#' @srrstats {G2.15} *Implements checks for NaN values via is.nan() prior to processing.*
#' @srrstats {G2.16} *Implements checks for Inf/-Inf values via is.infinite() prior to processing.*
#' @srrstats {G5.2a} *Each error message produced by stop() is unique.*
#'
#' @param bac Budget at Completion (BAC) (total planned budget).
#' @param schedule Vector of planned work completion (in terms of percentage) at each
#' time period.
#' @param time_period Current time period.
#' @return The function returns the Planned Value (PV) of work completed.
#' @references
#' Damnjanovic, Ivan, and Kenneth Reinschmidt. Data analytics for engineering and
#' construction project risk management. No. 172534. Cham, Switzerland: Springer, 2020.
#' @examples
#' # Set the BAC, schedule, and current time period for a toy project.
#' bac <- 100000
#' schedule <- c(0.1, 0.2, 0.4, 0.7, 1.0)
#' time_period <- 3
#'
#' # Calculate the PV and print the results.
#' pv <- pv(bac, schedule, time_period)
#' cat("Planned Value (PV):", pv, "\n")
#'
#' @seealso \code{\link{ev}}, \code{\link{ac}}, \code{\link{sv}}, \code{\link{cv}},
#' \code{\link{spi}}, \code{\link{cpi}}, \code{\link{eac}}
#'
#' @export
pv <- function(bac, schedule, time_period) {
  # Error handling
  if (is.null(bac) || is.null(schedule) || is.null(time_period)) {
    stop("bac, schedule, and time_period must not be NULL")
  }
  if (!is.numeric(bac) || !is.numeric(schedule) || !is.numeric(time_period)) {
    stop("bac, schedule, and time_period must be numeric")
  }
  if (length(bac) != 1) {
    stop("bac must be a single numeric value")
  }
  if (length(time_period) != 1) {
    stop("time_period must be a single numeric value")
  }
  if (any(is.nan(bac)) || any(is.nan(schedule)) || any(is.nan(time_period))) {
    stop("bac, schedule, and time_period must not contain NaN values")
  }
  if (anyNA(bac) || anyNA(schedule) || anyNA(time_period)) {
    stop("bac, schedule, and time_period must not contain NA values")
  }
  if (any(is.infinite(bac)) || any(is.infinite(schedule)) || any(is.infinite(time_period))) {
    stop("bac, schedule, and time_period must not contain infinite values")
  }
  if (length(schedule) == 0) {
    stop("schedule must not be empty")
  }
  if (time_period < 1 || time_period > length(schedule)) {
    stop("time_period must be within the range of the schedule vector")
  }
  if (any(schedule < 0) || any(schedule > 1)) {
    stop("schedule values must be between 0 and 1")
  }
  if (bac < 0) {
    stop("bac must be non-negative")
  }

  pv <- bac * schedule[time_period]
  return(pv)
}

#' Earned Value (EV).
#'
#' Calculates the Earned Value (EV) of work completed based on the Budget at Completion
#' (BAC) and the actual work completion percentage.
#'
#' @srrstats {G1.0} *Software lists primary reference from published academic literature.*
#' @srrstats {G1.1} *Software is the first implementation within **R** of the algorithm which has previously been implemented in other languages or contexts.*
#' @srrstats {G1.4} *Software uses [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#' @srrstats {G2.0} *Implements assertions on lengths of inputs - validates single values.*
#' @srrstats {G2.0a} *Parameter documentation explicitly states expected input structure (single numeric values).*
#' @srrstats {G2.1} *Implements assertions on types of inputs via is.numeric() checks.*
#' @srrstats {G2.1a} *Parameter documentation explicitly states data types expected.*
#' @srrstats {G2.2} *Prohibits multivariate input for bac and actual_per_complete which must be single values.*
#' @srrstats {G2.13} *Implements checks for missing data via anyNA() prior to processing.*
#' @srrstats {G2.14a} *Errors on missing data with informative message.*
#' @srrstats {G2.15} *Implements checks for NaN values via is.nan() prior to processing.*
#' @srrstats {G2.16} *Implements checks for Inf/-Inf values via is.infinite() prior to processing.*
#' @srrstats {G5.2a} *Each error message produced by stop() is unique.*
#'
#' @param bac Budget at Completion (BAC) (total planned budget).
#' @param actual_per_complete Actual work completion percentage.
#' @return The function returns the Earned Value (EV) of work completed.
#' @references
#' Damnjanovic, Ivan, and Kenneth Reinschmidt. Data analytics for engineering and
#' construction project risk management. No. 172534. Cham, Switzerland: Springer, 2020.
#' @examples
#' # Set the BAC and actual % complete for a toy project.
#' bac <- 100000
#' actual_per_complete <- 0.35
#'
#' # Calculate the EV and print the results.
#' ev <- ev(bac, actual_per_complete)
#' cat("Earned Value (EV):", ev, "\n")
#' @seealso \code{\link{pv}}, \code{\link{ac}}, \code{\link{sv}}, \code{\link{cv}},
#' \code{\link{spi}}, \code{\link{cpi}}, \code{\link{eac}}
#'
#' @export
ev <- function(bac, actual_per_complete) {
  # Error handling
  if (is.null(bac) || is.null(actual_per_complete)) {
    stop("bac and actual_per_complete must not be NULL")
  }
  if (!is.numeric(bac) || !is.numeric(actual_per_complete)) {
    stop("bac and actual_per_complete must be numeric")
  }
  if (length(bac) != 1) {
    stop("bac must be a single numeric value")
  }
  if (length(actual_per_complete) != 1) {
    stop("actual_per_complete must be a single numeric value")
  }
  if (is.nan(bac) || is.nan(actual_per_complete)) {
    stop("bac and actual_per_complete must not be NaN")
  }
  if (anyNA(bac) || anyNA(actual_per_complete)) {
    stop("bac and actual_per_complete must not be NA")
  }
  if (is.infinite(bac) || is.infinite(actual_per_complete)) {
    stop("bac and actual_per_complete must not be infinite")
  }
  if (actual_per_complete < 0 || actual_per_complete > 1) {
    stop("actual_per_complete must be between 0 and 1")
  }
  if (bac < 0) {
    stop("bac must be non-negative")
  }

  ev <- bac * actual_per_complete
  return(ev)
}

#' Actual Cost (AC).
#'
#' Calculates the Actual Cost (AC) of work completed based on the actual costs incurred
#' at each time period.
#'
#' @srrstats {G1.0} *Software lists primary reference from published academic literature.*
#' @srrstats {G1.1} *Software is the first implementation within **R** of the algorithm which has previously been implemented in other languages or contexts.*
#' @srrstats {G1.4} *Software uses [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#' @srrstats {G2.0} *Implements assertions on lengths of inputs - validates single values and non-empty vectors.*
#' @srrstats {G2.0a} *Parameter documentation explicitly states expected input structure.*
#' @srrstats {G2.1} *Implements assertions on types of inputs via is.numeric() and is.logical() checks.*
#' @srrstats {G2.1a} *Parameter documentation explicitly states data types expected.*
#' @srrstats {G2.2} *Prohibits multivariate input for time_period and cumulative which must be single values.*
#' @srrstats {G2.13} *Implements checks for missing data via anyNA() prior to processing.*
#' @srrstats {G2.14a} *Errors on missing data with informative message.*
#' @srrstats {G2.15} *Implements checks for NaN values via is.nan() prior to processing.*
#' @srrstats {G2.16} *Implements checks for Inf/-Inf values via is.infinite() prior to processing.*
#' @srrstats {G5.2a} *Each error message produced by stop() is unique.*
#'
#' @param actual_costs Vector of actual costs incurred at each time period. Can be either
#'   period costs (cost per period) or cumulative costs depending on the cumulative parameter.
#' @param time_period Current time period.
#' @param cumulative Logical. If TRUE (default), actual_costs are already cumulative and
#'   the value at time_period is returned directly. If FALSE, actual_costs are period costs
#'   and will be summed up to time_period.
#' @return The function returns the Actual Cost (AC) of work completed to date.
#' @references
#' Damnjanovic, Ivan, and Kenneth Reinschmidt. Data analytics for engineering and
#' construction project risk management. No. 172534. Cham, Switzerland: Springer, 2020.
#' @examples
#' # Using cumulative costs (default)
#' cumulative_costs <- c(9000, 27000, 63000, 133000, 233000)
#' time_period <- 3
#' ac <- ac(cumulative_costs, time_period)
#' cat("Actual Cost (AC):", ac, "\n")
#'
#' # Using period costs
#' period_costs <- c(9000, 18000, 36000, 70000, 100000)
#' ac <- ac(period_costs, time_period, cumulative = FALSE)
#' cat("Actual Cost (AC):", ac, "\n")
#'
#' @seealso \code{\link{pv}}, \code{\link{ev}}, \code{\link{sv}}, \code{\link{cv}},
#' \code{\link{spi}}, \code{\link{cpi}}, \code{\link{eac}}
#'
#' @export
ac <- function(actual_costs, time_period, cumulative = TRUE) {
  # Error handling
  if (is.null(actual_costs) || is.null(time_period)) {
    stop("actual_costs and time_period must not be NULL")
  }
  if (!is.numeric(actual_costs) || !is.numeric(time_period)) {
    stop("actual_costs and time_period must be numeric")
  }
  if (length(time_period) != 1) {
    stop("time_period must be a single numeric value")
  }
  if (!is.logical(cumulative) || length(cumulative) != 1) {
    stop("cumulative must be a single logical value")
  }
  if (any(is.nan(actual_costs)) || any(is.nan(time_period))) {
    stop("actual_costs and time_period must not contain NaN values")
  }
  if (anyNA(actual_costs) || anyNA(time_period)) {
    stop("actual_costs and time_period must not contain NA values")
  }
  if (any(is.infinite(actual_costs)) || any(is.infinite(time_period))) {
    stop("actual_costs and time_period must not contain infinite values")
  }
  if (length(actual_costs) == 0) {
    stop("actual_costs must not be empty")
  }
  if (time_period < 1 || time_period > length(actual_costs)) {
    stop("time_period must be within the range of the actual_costs vector")
  }
  if (any(actual_costs < 0)) {
    stop("actual_costs must be non-negative")
  }

  if (cumulative) {
    ac <- actual_costs[time_period]
  } else {
    ac <- sum(actual_costs[1:time_period])
  }
  return(ac)
}

#' Schedule Variance (SV).
#'
#' Calculates the Schedule Variance (SV) of work completed based on the Earned Value (EV)
#' and Planned Value (PV).
#'
#' @srrstats {G1.0} *Software lists primary reference from published academic literature.*
#' @srrstats {G1.1} *Software is the first implementation within **R** of the algorithm which has previously been implemented in other languages or contexts.*
#' @srrstats {G1.4} *Software uses [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#' @srrstats {G2.0} *Implements assertions on lengths of inputs - validates single values.*
#' @srrstats {G2.0a} *Parameter documentation explicitly states expected input structure (single numeric values).*
#' @srrstats {G2.1} *Implements assertions on types of inputs via is.numeric() checks.*
#' @srrstats {G2.1a} *Parameter documentation explicitly states data types expected.*
#' @srrstats {G2.2} *Prohibits multivariate input for ev and pv which must be single values.*
#' @srrstats {G2.13} *Implements checks for missing data via anyNA() prior to processing.*
#' @srrstats {G2.14a} *Errors on missing data with informative message.*
#' @srrstats {G2.15} *Implements checks for NaN values via is.nan() prior to processing.*
#' @srrstats {G2.16} *Implements checks for Inf/-Inf values via is.infinite() prior to processing.*
#' @srrstats {G5.2a} *Each error message produced by stop() is unique.*
#'
#' @param ev Earned Value.
#' @param pv Planned Value.
#' @return The function returns the Schedule Variance (SV) of work completed.
#' @references
#' Damnjanovic, Ivan, and Kenneth Reinschmidt. Data analytics for engineering and
#' construction project risk management. No. 172534. Cham, Switzerland: Springer, 2020.
#' @examples
#' # Set the BAC, schedule, and current time period for an example project.
#' bac <- 100000
#' schedule <- c(0.1, 0.2, 0.4, 0.7, 1.0)
#' time_period <- 3
#'
#' # Calculate the PV.
#' pv <- pv(bac, schedule, time_period)
#'
#' # Set the actual % complete and calculate the EV.
#' actual_per_complete <- 0.35
#' ev <- ev(bac, actual_per_complete)
#'
#' # Calculate the SV and print the results.
#' sv <- sv(ev, pv)
#' cat("Schedule Variance (SV):", sv, "\n")
#'
#' @seealso \code{\link{pv}}, \code{\link{ev}}, \code{\link{ac}}, \code{\link{cv}},
#' \code{\link{spi}}, \code{\link{cpi}}, \code{\link{eac}}
#'
#' @export
sv <- function(ev, pv) {
  # Error handling
  if (is.null(ev) || is.null(pv)) {
    stop("ev and pv must not be NULL")
  }
  if (!is.numeric(ev) || !is.numeric(pv)) {
    stop("ev and pv must be numeric")
  }
  if (length(ev) != 1 || length(pv) != 1) {
    stop("ev and pv must be single numeric values")
  }
  if (is.nan(ev) || is.nan(pv)) {
    stop("ev and pv must not be NaN")
  }
  if (anyNA(ev) || anyNA(pv)) {
    stop("ev and pv must not be NA")
  }
  if (is.infinite(ev) || is.infinite(pv)) {
    stop("ev and pv must not be infinite")
  }
  if (ev < 0) {
    stop("ev must be non-negative")
  }
  if (pv < 0) {
    stop("pv must be non-negative")
  }

  sv <- ev - pv
  return(sv)
}

#' Cost Variance (CV).
#'
#' Calculates the Cost Variance (CV) of work completed based on the Earned Value (EV)
#' and Actual Cost (AC).
#'
#' @srrstats {G1.0} *Software lists primary reference from published academic literature.*
#' @srrstats {G1.1} *Software is the first implementation within **R** of the algorithm which has previously been implemented in other languages or contexts.*
#' @srrstats {G1.4} *Software uses [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#' @srrstats {G2.0} *Implements assertions on lengths of inputs - validates single values.*
#' @srrstats {G2.0a} *Parameter documentation explicitly states expected input structure (single numeric values).*
#' @srrstats {G2.1} *Implements assertions on types of inputs via is.numeric() checks.*
#' @srrstats {G2.1a} *Parameter documentation explicitly states data types expected.*
#' @srrstats {G2.2} *Prohibits multivariate input for ev and ac which must be single values.*
#' @srrstats {G2.13} *Implements checks for missing data via anyNA() prior to processing.*
#' @srrstats {G2.14a} *Errors on missing data with informative message.*
#' @srrstats {G2.15} *Implements checks for NaN values via is.nan() prior to processing.*
#' @srrstats {G2.16} *Implements checks for Inf/-Inf values via is.infinite() prior to processing.*
#' @srrstats {G5.2a} *Each error message produced by stop() is unique.*
#'
#' @param ev Earned Value.
#' @param ac Actual Cost.
#' @return The function returns the Cost Variance (CV) of work completed.
#' @references
#' Damnjanovic, Ivan, and Kenneth Reinschmidt. Data analytics for engineering and
#' construction project risk management. No. 172534. Cham, Switzerland: Springer, 2020.
#' @examples
#' # Set the BAC and actual % complete for an example project.
#' bac <- 100000
#' actual_per_complete <- 0.35
#'
#' # Calcualte the EV
#' ev <- ev(bac, actual_per_complete)
#'
#' # Set the actual costs and current time period and calculate the AC.
#' actual_costs <- c(9000, 18000, 36000, 70000, 100000)
#' time_period <- 3
#' ac <- ac(actual_costs, time_period)
#'
#' # Calculate the CV and print the results.
#' cv <- cv(ev, ac)
#' cat("Cost Variance (CV):", cv, "\n")
#' @seealso \code{\link{pv}}, \code{\link{ev}}, \code{\link{ac}}, \code{\link{sv}},
#' \code{\link{spi}}, \code{\link{cpi}}, \code{\link{eac}}
#'
#' @export
cv <- function(ev, ac) {
  # Error handling
  if (is.null(ev) || is.null(ac)) {
    stop("ev and ac must not be NULL")
  }
  if (!is.numeric(ev) || !is.numeric(ac)) {
    stop("ev and ac must be numeric")
  }
  if (length(ev) != 1 || length(ac) != 1) {
    stop("ev and ac must be single numeric values")
  }
  if (is.nan(ev) || is.nan(ac)) {
    stop("ev and ac must not be NaN")
  }
  if (anyNA(ev) || anyNA(ac)) {
    stop("ev and ac must not be NA")
  }
  if (is.infinite(ev) || is.infinite(ac)) {
    stop("ev and ac must not be infinite")
  }
  if (ev < 0) {
    stop("ev must be non-negative")
  }
  if (ac < 0) {
    stop("ac must be non-negative")
  }

  cv <- ev - ac
  return(cv)
}

#' Schedule Performance Index (SPI).
#'
#' Calculates the Schedule Performance Index (SPI) of work completed based on the Earned Value (EV)
#' and Planned Value (PV).
#'
#' @srrstats {G1.0} *Software lists primary reference from published academic literature.*
#' @srrstats {G1.1} *Software is the first implementation within **R** of the algorithm which has previously been implemented in other languages or contexts.*
#' @srrstats {G1.4} *Software uses [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#' @srrstats {G2.0} *Implements assertions on lengths of inputs - validates single values.*
#' @srrstats {G2.0a} *Parameter documentation explicitly states expected input structure (single numeric values).*
#' @srrstats {G2.1} *Implements assertions on types of inputs via is.numeric() checks.*
#' @srrstats {G2.1a} *Parameter documentation explicitly states data types expected.*
#' @srrstats {G2.2} *Prohibits multivariate input for ev and pv which must be single values.*
#' @srrstats {G2.13} *Implements checks for missing data via anyNA() prior to processing.*
#' @srrstats {G2.14a} *Errors on missing data with informative message.*
#' @srrstats {G2.15} *Implements checks for NaN values via is.nan() prior to processing.*
#' @srrstats {G2.16} *Implements checks for Inf/-Inf values via is.infinite() prior to processing.*
#' @srrstats {G5.2a} *Each error message produced by stop() is unique.*
#'
#' @param ev Earned Value.
#' @param pv Planned Value.
#' @return The function returns the Schedule Performance Index (SPI) of work completed.
#' @references
#' Damnjanovic, Ivan, and Kenneth Reinschmidt. Data analytics for engineering and
#' construction project risk management. No. 172534. Cham, Switzerland: Springer, 2020.
#' @examples
#' # Set the BAC, schedule, and current time period for an example project.
#' bac <- 100000
#' schedule <- c(0.1, 0.2, 0.4, 0.7, 1.0)
#' time_period <- 3
#'
#' # Calculate the PV.
#' pv <- pv(bac, schedule, time_period)
#'
#' # Set the actual % complete and calculate the EV.
#' actual_per_complete <- 0.35
#' ev <- ev(bac, actual_per_complete)
#'
#' # Calculate the SPI and print the results.
#' spi <- spi(ev, pv)
#' cat("Schedule Performance Index (SPI):", spi, "\n")
#' @seealso \code{\link{pv}}, \code{\link{ev}}, \code{\link{ac}}, \code{\link{sv}},
#' \code{\link{cv}}, \code{\link{cpi}}, \code{\link{eac}}
#'
#' @export
spi <- function(ev, pv) {
  # Error handling
  if (is.null(ev) || is.null(pv)) {
    stop("ev and pv must not be NULL")
  }
  if (!is.numeric(ev) || !is.numeric(pv)) {
    stop("ev and pv must be numeric")
  }
  if (length(ev) != 1 || length(pv) != 1) {
    stop("ev and pv must be single numeric values")
  }
  if (is.nan(ev) || is.nan(pv)) {
    stop("ev and pv must not be NaN")
  }
  if (anyNA(ev) || anyNA(pv)) {
    stop("ev and pv must not be NA")
  }
  if (is.infinite(ev) || is.infinite(pv)) {
    stop("ev and pv must not be infinite")
  }
  if (ev < 0) {
    stop("ev must be non-negative")
  }
  if (pv <= 0) {
    stop("pv must be greater than zero")
  }

  spi <- ev / pv
  return(spi)
}

#' Cost Performance Index (CPI).
#'
#' Calculates the Cost Performance Index (CPI) of work completed based on the Earned Value (EV)
#' and Actual Cost (AC).
#'
#' @srrstats {G1.0} *Software lists primary reference from published academic literature.*
#' @srrstats {G1.1} *Software is the first implementation within **R** of the algorithm which has previously been implemented in other languages or contexts.*
#' @srrstats {G1.4} *Software uses [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#' @srrstats {G2.0} *Implements assertions on lengths of inputs - validates single values.*
#' @srrstats {G2.0a} *Parameter documentation explicitly states expected input structure (single numeric values).*
#' @srrstats {G2.1} *Implements assertions on types of inputs via is.numeric() checks.*
#' @srrstats {G2.1a} *Parameter documentation explicitly states data types expected.*
#' @srrstats {G2.2} *Prohibits multivariate input for ev and ac which must be single values.*
#' @srrstats {G2.13} *Implements checks for missing data via anyNA() prior to processing.*
#' @srrstats {G2.14a} *Errors on missing data with informative message.*
#' @srrstats {G2.15} *Implements checks for NaN values via is.nan() prior to processing.*
#' @srrstats {G2.16} *Implements checks for Inf/-Inf values via is.infinite() prior to processing.*
#' @srrstats {G5.2a} *Each error message produced by stop() is unique.*
#'
#' @param ev Earned Value.
#' @param ac Actual Cost.
#' @return The function returns the Cost Performance Index (CPI) of work completed.
#' @references
#' Damnjanovic, Ivan, and Kenneth Reinschmidt. Data analytics for engineering and
#' construction project risk management. No. 172534. Cham, Switzerland: Springer, 2020.
#' @examples
#' # Set the BAC and actual % complete for an example project.
#' bac <- 100000
#' actual_per_complete <- 0.35
#'
#' # Calcualte the EV
#' ev <- ev(bac, actual_per_complete)
#'
#' # Set the actual costs and current time period and calculate the AC.
#' actual_costs <- c(9000, 18000, 36000, 70000, 100000)
#' time_period <- 3
#' ac <- ac(actual_costs, time_period)
#'
#' # Calculate the CPI and print the results.
#' cpi <- cpi(ev, ac)
#' cat("Cost Performance Index (CPI):", cpi, "\n")
#'
#' @seealso \code{\link{pv}}, \code{\link{ev}}, \code{\link{ac}}, \code{\link{sv}},
#' \code{\link{cv}}, \code{\link{spi}}, \code{\link{eac}}
#'
#' @export
cpi <- function(ev, ac) {
  # Error handling
  if (is.null(ev) || is.null(ac)) {
    stop("ev and ac must not be NULL")
  }
  if (!is.numeric(ev) || !is.numeric(ac)) {
    stop("ev and ac must be numeric")
  }
  if (length(ev) != 1 || length(ac) != 1) {
    stop("ev and ac must be single numeric values")
  }
  if (is.nan(ev) || is.nan(ac)) {
    stop("ev and ac must not be NaN")
  }
  if (anyNA(ev) || anyNA(ac)) {
    stop("ev and ac must not be NA")
  }
  if (is.infinite(ev) || is.infinite(ac)) {
    stop("ev and ac must not be infinite")
  }
  if (ev < 0) {
    stop("ev must be non-negative")
  }
  if (ac <= 0) {
    stop("ac must be greater than zero")
  }

  cpi <- ev / ac
  return(cpi)
}

#' Estimate at Completion (EAC).
#'
#' Calculates the Estimate at Completion (EAC) using various methods based on
#' project performance assumptions.
#'
#' @srrstats {G1.0} *Software lists primary reference from published academic literature.*
#' @srrstats {G1.1} *Software is the first implementation within **R** of the algorithm which has previously been implemented in other languages or contexts.*
#' @srrstats {G1.4} *Software uses [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#' @srrstats {G2.0} *Implements assertions on lengths of inputs - validates single values.*
#' @srrstats {G2.0a} *Parameter documentation explicitly states expected input structure (single numeric values).*
#' @srrstats {G2.1} *Implements assertions on types of inputs via is.numeric() checks.*
#' @srrstats {G2.1a} *Parameter documentation explicitly states data types expected.*
#' @srrstats {G2.2} *Prohibits multivariate input for all parameters which must be single values.*
#' @srrstats {G2.3a} *Uses explicit validation against valid_methods vector for method parameter.*
#' @srrstats {G2.3b} *Uses tolower() to ensure method parameter is not case sensitive.*
#' @srrstats {G2.13} *Implements checks for missing data via is.na() prior to processing.*
#' @srrstats {G2.14a} *Errors on missing data with informative message.*
#' @srrstats {G2.15} *Implements checks for NaN values via is.nan() prior to processing.*
#' @srrstats {G2.16} *Implements checks for Inf/-Inf values via is.infinite() prior to processing.*
#' @srrstats {G5.2a} *Each error message produced by stop() is unique.*
#'
#' @param bac Budget at Completion (BAC) (total planned budget).
#' @param method The EAC calculation method. One of:
#'   \itemize{
#'     \item "typical" (default): EAC = BAC / CPI. Assumes future work performed at current cost efficiency.
#'     \item "atypical": EAC = AC + (BAC - EV). Assumes future work performed at planned rate.
#'     \item "combined": EAC = AC + (BAC - EV) / (CPI * SPI). Considers both cost and schedule performance.
#'   }
#' @param cpi Cost Performance Index (CPI). Required for "typical" and "combined" methods.
#' @param ac Actual Cost. Required for "atypical" and "combined" methods.
#' @param ev Earned Value. Required for "atypical" and "combined" methods.
#' @param spi Schedule Performance Index. Required for "combined" method.
#' @return The function returns the Estimate at Completion (EAC).
#' @references
#' Damnjanovic, Ivan, and Kenneth Reinschmidt. Data analytics for engineering and
#' construction project risk management. No. 172534. Cham, Switzerland: Springer, 2020.
#' @examples
#' # Method 1: Typical - assumes current CPI continues
#' bac <- 100000
#' cpi <- 0.83
#' eac <- eac(bac, cpi = cpi)
#' cat("EAC (typical):", round(eac, 2), "\n")
#'
#' # Method 2: Atypical - assumes future work at planned rate
#' ac <- 63000
#' ev <- 35000
#' eac <- eac(bac, method = "atypical", ac = ac, ev = ev)
#' cat("EAC (atypical):", round(eac, 2), "\n")
#'
#' # Method 3: Combined - considers both CPI and SPI
#' spi <- 0.875
#' eac <- eac(bac, method = "combined", cpi = cpi, ac = ac, ev = ev, spi = spi)
#' cat("EAC (combined):", round(eac, 2), "\n")
#'
#' @seealso \code{\link{pv}}, \code{\link{ev}}, \code{\link{ac}}, \code{\link{sv}},
#' \code{\link{cv}}, \code{\link{spi}}, \code{\link{cpi}}, \code{\link{etc}},
#' \code{\link{vac}}, \code{\link{tcpi}}
#'
#' @export
eac <- function(bac, method = "typical", cpi = NULL, ac = NULL, ev = NULL, spi = NULL) {
  # Validate method
  method <- tolower(method)
  valid_methods <- c("typical", "atypical", "combined")
  if (!method %in% valid_methods) {
    stop("method must be one of: ", paste(valid_methods, collapse = ", "))
  }

  # Error handling for bac
  if (is.null(bac)) {
    stop("bac must not be NULL")
  }
  if (!is.numeric(bac) || length(bac) != 1 || is.nan(bac)) {
    stop("bac must be a single non-NaN numeric value")
  }
  if (is.na(bac)) {
    stop("bac must be a single numeric value")
  }
  if (is.infinite(bac)) {
    stop("bac must not be infinite")
  }
  if (bac < 0) {
    stop("bac must be non-negative")
  }

  # Calculate EAC based on method
  if (method == "typical") {
    # EAC = BAC / CPI
    if (is.null(cpi)) {
      stop("cpi is required for the 'typical' method")
    }
    if (!is.numeric(cpi) || length(cpi) != 1 || is.nan(cpi)) {
      stop("cpi must be a single non-NaN numeric value")
    }
    if (is.na(cpi)) {
      stop("cpi must be a single numeric value")
    }
    if (is.infinite(cpi)) {
      stop("cpi must not be infinite")
    }
    if (cpi <= 0) {
      stop("cpi must be greater than zero")
    }
    eac <- bac / cpi
  } else if (method == "atypical") {
    # EAC = AC + (BAC - EV)
    if (is.null(ac) || is.null(ev)) {
      stop("ac and ev are required for the 'atypical' method")
    }
    if (!is.numeric(ac) || length(ac) != 1 || is.nan(ac)) {
      stop("ac must be a single non-NaN numeric value")
    }
    if (is.na(ac)) {
      stop("ac must be a single numeric value")
    }
    if (is.infinite(ac)) {
      stop("ac must not be infinite")
    }
    if (!is.numeric(ev) || length(ev) != 1 || is.nan(ev)) {
      stop("ev must be a single non-NaN numeric value")
    }
    if (is.na(ev)) {
      stop("ev must be a single numeric value")
    }
    if (is.infinite(ev)) {
      stop("ev must not be infinite")
    }
    if (ac < 0 || ev < 0) {
      stop("ac and ev must be non-negative")
    }
    eac <- ac + (bac - ev)
  } else if (method == "combined") {
    # EAC = AC + (BAC - EV) / (CPI * SPI)
    if (is.null(cpi) || is.null(ac) || is.null(ev) || is.null(spi)) {
      stop("cpi, ac, ev, and spi are required for the 'combined' method")
    }
    if (!is.numeric(cpi) || length(cpi) != 1 || is.nan(cpi)) {
      stop("cpi must be a single non-NaN numeric value for 'combined' method")
    }
    if (is.na(cpi)) {
      stop("cpi must be a single numeric value")
    }
    if (is.infinite(cpi)) {
      stop("cpi must not be infinite for 'combined' method")
    }
    if (!is.numeric(spi) || length(spi) != 1 || is.nan(spi)) {
      stop("spi must be a single non-NaN numeric value")
    }
    if (is.na(spi)) {
      stop("spi must be a single numeric value")
    }
    if (is.infinite(spi)) {
      stop("spi must not be infinite")
    }
    if (!is.numeric(ac) || length(ac) != 1 || is.nan(ac)) {
      stop("ac must be a single non-NaN numeric value for 'combined' method")
    }
    if (is.na(ac)) {
      stop("ac must be a single numeric value")
    }
    if (is.infinite(ac)) {
      stop("ac must not be infinite for 'combined' method")
    }
    if (!is.numeric(ev) || length(ev) != 1 || is.nan(ev)) {
      stop("ev must be a single non-NaN numeric value for 'combined' method")
    }
    if (is.na(ev)) {
      stop("ev must be a single numeric value")
    }
    if (is.infinite(ev)) {
      stop("ev must not be infinite for 'combined' method")
    }
    if (cpi <= 0 || spi <= 0) {
      stop("cpi and spi must be greater than zero")
    }
    if (ac < 0 || ev < 0) {
      stop("ac and ev must be non-negative")
    }
    eac <- ac + (bac - ev) / (cpi * spi)
  }

  return(eac)
}

#' Estimate to Complete (ETC).
#'
#' Calculates the Estimate to Complete (ETC), which is the expected cost to finish
#' the remaining work.
#'
#' @srrstats {G1.0} *Software lists primary reference from published academic literature.*
#' @srrstats {G1.1} *Software is the first implementation within **R** of the algorithm which has previously been implemented in other languages or contexts.*
#' @srrstats {G1.4} *Software uses [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#' @srrstats {G2.0} *Implements assertions on lengths of inputs - validates single values.*
#' @srrstats {G2.0a} *Parameter documentation explicitly states expected input structure (single numeric values).*
#' @srrstats {G2.1} *Implements assertions on types of inputs via is.numeric() checks.*
#' @srrstats {G2.1a} *Parameter documentation explicitly states data types expected.*
#' @srrstats {G2.2} *Prohibits multivariate input for bac, ev, and cpi which must be single values.*
#' @srrstats {G2.13} *Implements checks for missing data via anyNA() and is.na() prior to processing.*
#' @srrstats {G2.14a} *Errors on missing data with informative message.*
#' @srrstats {G2.15} *Implements checks for NaN values via is.nan() prior to processing.*
#' @srrstats {G2.16} *Implements checks for Inf/-Inf values via is.infinite() prior to processing.*
#' @srrstats {G5.2a} *Each error message produced by stop() is unique.*
#'
#' @param bac Budget at Completion (BAC) (total planned budget).
#' @param ev Earned Value.
#' @param cpi Cost Performance Index. If NULL, assumes remaining work will be completed
#'   at planned cost (ETC = BAC - EV). If provided, adjusts for current performance
#'   (ETC = (BAC - EV) / CPI).
#' @return The function returns the Estimate to Complete (ETC).
#' @references
#' Damnjanovic, Ivan, and Kenneth Reinschmidt. Data analytics for engineering and
#' construction project risk management. No. 172534. Cham, Switzerland: Springer, 2020.
#' @examples
#' bac <- 100000
#' ev <- 35000
#' cpi <- 0.83
#'
#' # ETC assuming remaining work at planned rate
#' etc <- etc(bac, ev)
#' cat("ETC (planned rate):", etc, "\n")
#'
#' # ETC assuming remaining work at current CPI
#' etc <- etc(bac, ev, cpi)
#' cat("ETC (current CPI):", round(etc, 2), "\n")
#'
#' @seealso \code{\link{eac}}, \code{\link{vac}}, \code{\link{tcpi}}
#'
#' @export
etc <- function(bac, ev, cpi = NULL) {
  # Error handling
  if (is.null(bac) || is.null(ev)) {
    stop("bac and ev must not be NULL")
  }
  if (!is.numeric(bac) || !is.numeric(ev)) {
    stop("bac and ev must be numeric")
  }
  if (length(bac) != 1 || length(ev) != 1) {
    stop("bac and ev must be single numeric values")
  }
  if (is.nan(bac) || is.nan(ev)) {
    stop("bac and ev must not be NaN")
  }
  if (anyNA(bac) || anyNA(ev)) {
    stop("bac and ev must not be NA")
  }
  if (is.infinite(bac) || is.infinite(ev)) {
    stop("bac and ev must not be infinite")
  }
  if (bac < 0 || ev < 0) {
    stop("bac and ev must be non-negative")
  }

  if (is.null(cpi)) {
    # Remaining work at planned rate
    etc <- bac - ev
  } else {
    if (!is.numeric(cpi) || length(cpi) != 1 || is.nan(cpi)) {
      stop("cpi must be a single non-NaN numeric value")
    }
    if (is.na(cpi)) {
      stop("cpi must be a single numeric value")
    }
    if (is.infinite(cpi)) {
      stop("cpi must not be infinite")
    }
    if (cpi <= 0) {
      stop("cpi must be greater than zero")
    }
    # Remaining work at current CPI
    etc <- (bac - ev) / cpi
  }

  return(etc)
}

#' Variance at Completion (VAC).
#'
#' Calculates the Variance at Completion (VAC), which is the difference between
#' the budget and the expected final cost. Positive VAC indicates under budget,
#' negative indicates over budget.
#'
#' @srrstats {G1.0} *Software lists primary reference from published academic literature.*
#' @srrstats {G1.1} *Software is the first implementation within **R** of the algorithm which has previously been implemented in other languages or contexts.*
#' @srrstats {G1.4} *Software uses [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#' @srrstats {G2.0} *Implements assertions on lengths of inputs - validates single values.*
#' @srrstats {G2.0a} *Parameter documentation explicitly states expected input structure (single numeric values).*
#' @srrstats {G2.1} *Implements assertions on types of inputs via is.numeric() checks.*
#' @srrstats {G2.1a} *Parameter documentation explicitly states data types expected.*
#' @srrstats {G2.2} *Prohibits multivariate input for bac and eac which must be single values.*
#' @srrstats {G2.13} *Implements checks for missing data via anyNA() prior to processing.*
#' @srrstats {G2.14a} *Errors on missing data with informative message.*
#' @srrstats {G2.15} *Implements checks for NaN values via is.nan() prior to processing.*
#' @srrstats {G2.16} *Implements checks for Inf/-Inf values via is.infinite() prior to processing.*
#' @srrstats {G5.2a} *Each error message produced by stop() is unique.*
#'
#' @param bac Budget at Completion (BAC) (total planned budget).
#' @param eac Estimate at Completion.
#' @return The function returns the Variance at Completion (VAC).
#' @references
#' Damnjanovic, Ivan, and Kenneth Reinschmidt. Data analytics for engineering and
#' construction project risk management. No. 172534. Cham, Switzerland: Springer, 2020.
#' @examples
#' bac <- 100000
#' eac <- 120482 # From EAC calculation
#'
#' vac <- vac(bac, eac)
#' cat("Variance at Completion (VAC):", round(vac, 2), "\n")
#' cat("Project is expected to be", abs(round(vac, 2)), ifelse(vac < 0, "over", "under"), "budget\n")
#'
#' @seealso \code{\link{eac}}, \code{\link{etc}}, \code{\link{tcpi}}
#'
#' @export
vac <- function(bac, eac) {
  # Error handling
  if (is.null(bac) || is.null(eac)) {
    stop("bac and eac must not be NULL")
  }
  if (!is.numeric(bac) || !is.numeric(eac)) {
    stop("bac and eac must be numeric")
  }
  if (length(bac) != 1 || length(eac) != 1) {
    stop("bac and eac must be single numeric values")
  }
  if (is.nan(bac) || is.nan(eac)) {
    stop("bac and eac must not be NaN")
  }
  if (anyNA(bac) || anyNA(eac)) {
    stop("bac and eac must not be NA")
  }
  if (is.infinite(bac) || is.infinite(eac)) {
    stop("bac and eac must not be infinite")
  }
  if (bac < 0 || eac < 0) {
    stop("bac and eac must be non-negative")
  }

  vac <- bac - eac
  return(vac)
}

#' To-Complete Performance Index (TCPI).
#'
#' Calculates the To-Complete Performance Index (TCPI), which indicates the cost
#' performance required on remaining work to meet a target (BAC or EAC).
#' TCPI > 1 means efficiency must improve; TCPI < 1 means efficiency can decrease.
#'
#' @srrstats {G1.0} *Software lists primary reference from published academic literature.*
#' @srrstats {G1.1} *Software is the first implementation within **R** of the algorithm which has previously been implemented in other languages or contexts.*
#' @srrstats {G1.4} *Software uses [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#' @srrstats {G2.0} *Implements assertions on lengths of inputs - validates single values.*
#' @srrstats {G2.0a} *Parameter documentation explicitly states expected input structure (single numeric values).*
#' @srrstats {G2.1} *Implements assertions on types of inputs via is.numeric() checks.*
#' @srrstats {G2.1a} *Parameter documentation explicitly states data types expected.*
#' @srrstats {G2.2} *Prohibits multivariate input for all parameters which must be single values.*
#' @srrstats {G2.3} *Uses tolower() to ensure target parameter is not case sensitive.*
#' @srrstats {G2.3a} *Uses explicit validation for target parameter to only permit expected values.*
#' @srrstats {G2.13} *Implements checks for missing data via anyNA() and is.na() prior to processing.*
#' @srrstats {G2.14a} *Errors on missing data with informative message.*
#' @srrstats {G2.15} *Implements checks for NaN values via is.nan() prior to processing.*
#' @srrstats {G2.16} *Implements checks for Inf/-Inf values via is.infinite() prior to processing.*
#' @srrstats {G5.2a} *Each error message produced by stop() is unique.*
#'
#' @param bac Budget at Completion (BAC) (total planned budget).
#' @param ev Earned Value.
#' @param ac Actual Cost.
#' @param target The target to calculate TCPI against. Either "bac" (default) to meet
#'   original budget, or "eac" to meet revised estimate. If "eac", the eac parameter
#'   must be provided.
#' @param eac Estimate at Completion. Required when target = "eac".
#' @return The function returns the To-Complete Performance Index (TCPI).
#' @references
#' Damnjanovic, Ivan, and Kenneth Reinschmidt. Data analytics for engineering and
#' construction project risk management. No. 172534. Cham, Switzerland: Springer, 2020.
#' @examples
#' bac <- 100000
#' ev <- 35000
#' ac <- 63000
#'
#' # TCPI to complete within original budget
#' tcpi_bac <- tcpi(bac, ev, ac)
#' cat("TCPI (to meet BAC):", round(tcpi_bac, 2), "\n")
#'
#' # TCPI to complete within revised estimate
#' eac <- 120482
#' tcpi_eac <- tcpi(bac, ev, ac, target = "eac", eac = eac)
#' cat("TCPI (to meet EAC):", round(tcpi_eac, 2), "\n")
#'
#' @seealso \code{\link{eac}}, \code{\link{etc}}, \code{\link{vac}}, \code{\link{cpi}}
#'
#' @export
tcpi <- function(bac, ev, ac, target = "bac", eac = NULL) {
  # Convert target to lowercase for case-insensitive matching
  target <- tolower(target)

  # Error handling
  if (is.null(bac) || is.null(ev) || is.null(ac)) {
    stop("bac, ev, and ac must not be NULL")
  }
  if (!is.numeric(bac) || !is.numeric(ev) || !is.numeric(ac)) {
    stop("bac, ev, and ac must be numeric")
  }
  if (length(bac) != 1 || length(ev) != 1 || length(ac) != 1) {
    stop("bac, ev, and ac must be single numeric values")
  }
  if (is.nan(bac) || is.nan(ev) || is.nan(ac)) {
    stop("bac, ev, and ac must not be NaN")
  }
  if (anyNA(bac) || anyNA(ev) || anyNA(ac)) {
    stop("bac, ev, and ac must not be NA")
  }
  if (is.infinite(bac) || is.infinite(ev) || is.infinite(ac)) {
    stop("bac, ev, and ac must not be infinite")
  }
  if (bac < 0 || ev < 0 || ac < 0) {
    stop("bac, ev, and ac must be non-negative")
  }

  if (!target %in% c("bac", "eac")) {
    stop("target must be either 'bac' or 'eac'")
  }

  if (target == "bac") {
    # TCPI = (BAC - EV) / (BAC - AC)
    denominator <- bac - ac
    if (denominator <= 0) {
      stop("Cannot calculate TCPI: actual cost already meets or exceeds budget")
    }
    tcpi <- (bac - ev) / denominator
  } else {
    # TCPI = (BAC - EV) / (EAC - AC)
    if (is.null(eac)) {
      stop("eac is required when target = 'eac'")
    }
    if (!is.numeric(eac) || length(eac) != 1 || is.nan(eac)) {
      stop("eac must be a single non-NaN numeric value")
    }
    if (is.na(eac)) {
      stop("eac must be a single numeric value")
    }
    if (is.infinite(eac)) {
      stop("eac must not be infinite")
    }
    if (eac < 0) {
      stop("eac must be non-negative")
    }
    denominator <- eac - ac
    if (denominator <= 0) {
      stop("Cannot calculate TCPI: actual cost already meets or exceeds EAC")
    }
    tcpi <- (bac - ev) / denominator
  }

  return(tcpi)
}
