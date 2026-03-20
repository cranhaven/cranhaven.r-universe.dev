#' Seasonal Analysis Functions for Functional Data
#'
#' Functions for analyzing seasonal patterns in functional data including
#' period estimation, peak detection, seasonal strength measurement, and
#' detection of seasonality changes.

# ==============================================================================
# Period Estimation
# ==============================================================================

#' Detect Period with Multiple Methods
#'
#' Unified interface for period detection that dispatches to specialized
#' algorithms based on the chosen method. Provides a single entry point
#' for all period detection functionality.
#'
#' @param fdataobj An fdata object.
#' @param method Detection method to use:
#' \describe{
#'   \item{"sazed"}{SAZED ensemble (default) - parameter-free, most robust}
#'   \item{"autoperiod"}{Autoperiod - hybrid FFT + ACF with gradient ascent}
#'   \item{"cfd"}{CFDAutoperiod - differencing + clustering + ACF validation}
#'   \item{"fft"}{Simple FFT periodogram peak}
#'   \item{"acf"}{Simple ACF peak detection}
#' }
#' @param ... Additional arguments passed to the underlying method:
#' \describe{
#'   \item{tolerance}{For SAZED: relative tolerance for voting (default 0.1)}
#'   \item{n_candidates}{For Autoperiod: max FFT peaks to consider (default 5)}
#'   \item{gradient_steps}{For Autoperiod: refinement steps (default 10)}
#'   \item{cluster_tolerance}{For CFD: clustering tolerance (default 0.1)}
#'   \item{min_cluster_size}{For CFD: minimum cluster size (default 1)}
#'   \item{max_lag}{For ACF: maximum lag to search}
#'   \item{detrend_method}{For FFT/ACF: "none", "linear", or "auto"}
#' }
#'
#' @return A period detection result. The exact class depends on the method:
#' \describe{
#'   \item{sazed_result}{For SAZED method}
#'   \item{autoperiod_result}{For Autoperiod method}
#'   \item{cfd_autoperiod_result}{For CFD method}
#'   \item{period_estimate}{For FFT and ACF methods}
#' }
#'
#' @details
#' \strong{Method selection guidance:}
#' \itemize{
#'   \item \strong{sazed}: Best general-purpose choice. Combines 5 methods
#'     and uses voting - robust across signal types with no tuning needed.
#'   \item \strong{autoperiod}: Good when you need candidate details and
#'     gradient-refined estimates. Slightly more precise than SAZED.
#'   \item \strong{cfd}: Best for signals with strong polynomial trends.
#'     Also detects multiple concurrent periods.
#'   \item \strong{fft}: Fastest, but sensitive to noise and trends.
#'   \item \strong{acf}: Simple but effective for clean periodic signals.
#' }
#'
#' @seealso
#' \code{\link{sazed}}, \code{\link{autoperiod}}, \code{\link{cfd.autoperiod}},
#' \code{\link{estimate.period}}, \code{\link{detect.periods}}
#'
#' @export
#' @examples
#' # Generate seasonal data
#' t <- seq(0, 20, length.out = 400)
#' X <- matrix(sin(2 * pi * t / 2) + 0.1 * rnorm(400), nrow = 1)
#' fd <- fdata(X, argvals = t)
#'
#' # Default (SAZED) - most robust
#' result <- detect.period(fd)
#' print(result$period)
#'
#' # Autoperiod with custom settings
#' result <- detect.period(fd, method = "autoperiod", n_candidates = 10)
#'
#' # CFDAutoperiod for trended data
#' X_trend <- matrix(0.3 * t + sin(2 * pi * t / 2), nrow = 1)
#' fd_trend <- fdata(X_trend, argvals = t)
#' result <- detect.period(fd_trend, method = "cfd")
#'
#' # Simple FFT for speed
#' result <- detect.period(fd, method = "fft")
detect.period <- function(fdataobj,
                          method = c("sazed", "autoperiod", "cfd", "fft", "acf"),
                          ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  method <- match.arg(method)
  dots <- list(...)

  result <- switch(method,
    sazed = {
      tolerance <- dots$tolerance %||% 0.1
      detrend_method <- dots$detrend_method %||% "none"
      sazed(fdataobj, tolerance = tolerance, detrend_method = detrend_method)
    },
    autoperiod = {
      n_candidates <- dots$n_candidates %||% 5
      gradient_steps <- dots$gradient_steps %||% 10
      detrend_method <- dots$detrend_method %||% "none"
      autoperiod(fdataobj, n_candidates = n_candidates,
                 gradient_steps = gradient_steps,
                 detrend_method = detrend_method)
    },
    cfd = {
      cluster_tolerance <- dots$cluster_tolerance %||% 0.1
      min_cluster_size <- dots$min_cluster_size %||% 1
      cfd.autoperiod(fdataobj, cluster_tolerance = cluster_tolerance,
                     min_cluster_size = min_cluster_size)
    },
    fft = {
      detrend_method <- dots$detrend_method %||% "none"
      estimate.period(fdataobj, method = "fft", detrend_method = detrend_method)
    },
    acf = {
      max_lag <- dots$max_lag
      detrend_method <- dots$detrend_method %||% "none"
      estimate.period(fdataobj, method = "acf", max_lag = max_lag,
                      detrend_method = detrend_method)
    }
  )

  result
}

# Helper for NULL-coalescing (R doesn't have %||% by default in older versions)
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Estimate Seasonal Period using FFT
#'
#' Estimates the dominant period in functional data using Fast Fourier Transform
#' and periodogram analysis.
#'
#' @param fdataobj An fdata object.
#' @param method Method for period estimation: "fft" (Fast Fourier Transform,
#'   default) or "acf" (autocorrelation function).
#' @param max_lag Maximum lag for ACF method. Default: half the series length.
#' @param detrend_method Detrending method to apply before period estimation:
#' \describe{
#'   \item{"none"}{No detrending (default)}
#'   \item{"linear"}{Remove linear trend}
#'   \item{"auto"}{Automatic AIC-based selection of detrending method}
#' }
#'
#' @return A list with components:
#' \describe{
#'   \item{period}{Estimated period}
#'   \item{frequency}{Dominant frequency (1/period)}
#'   \item{power}{Power at the dominant frequency}
#'   \item{confidence}{Confidence measure (ratio of peak power to mean power)}
#' }
#'
#' @details
#' The function computes the periodogram of the mean curve and finds the
#' frequency with maximum power. The confidence measure indicates how
#' pronounced the dominant frequency is relative to the background.
#'
#' For data with trends, the detrend_method parameter can significantly
#' improve period estimation accuracy. Strong trends can mask the true
#' seasonal period.
#'
#' @export
#' @examples
#' # Generate seasonal data with period = 2
#' t <- seq(0, 10, length.out = 200)
#' X <- matrix(sin(2 * pi * t / 2) + rnorm(200, sd = 0.1), nrow = 1)
#' fd <- fdata(X, argvals = t)
#'
#' # Estimate period
#' result <- estimate.period(fd, method = "fft")
#' print(result$period)  # Should be close to 2
#'
#' # With trend - detrending improves estimation
#' X_trend <- matrix(2 + 0.5 * t + sin(2 * pi * t / 2), nrow = 1)
#' fd_trend <- fdata(X_trend, argvals = t)
#' result <- estimate.period(fd_trend, detrend_method = "linear")
estimate.period <- function(fdataobj, method = c("fft", "acf"),
                            max_lag = NULL,
                            detrend_method = c("none", "linear", "auto")) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("estimate.period not yet implemented for 2D functional data")
  }

  method <- match.arg(method)
  detrend_method <- match.arg(detrend_method)

  # Apply detrending if requested
  if (detrend_method != "none") {
    fdataobj <- detrend(fdataobj, method = detrend_method)
  }

  if (method == "fft") {
    result <- .Call("wrap__seasonal_estimate_period_fft",
                    fdataobj$data, fdataobj$argvals)
  } else {
    if (is.null(max_lag)) {
      max_lag <- as.integer(ncol(fdataobj$data) / 2)
    }
    result <- .Call("wrap__seasonal_estimate_period_acf",
                    fdataobj$data, fdataobj$argvals, as.integer(max_lag))
  }

  class(result) <- "period_estimate"
  result
}

#' Print method for period_estimate objects
#' @param x A period_estimate object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.period_estimate <- function(x, ...) {
  cat("Period Estimate\n")
  cat("---------------\n")
  cat(sprintf("Period:     %.4f\n", x$period))
  cat(sprintf("Frequency:  %.4f\n", x$frequency))
  cat(sprintf("Power:      %.4f\n", x$power))
  cat(sprintf("Confidence: %.4f\n", x$confidence))
  invisible(x)
}

#' CFDAutoperiod: Clustered Filtered Detrended Autoperiod
#'
#' Implements the CFDAutoperiod algorithm (Puech et al. 2020) which applies
#' first-order differencing for detrending, then uses density-based clustering
#' of spectral peaks and ACF validation on the original signal.
#'
#' @param fdataobj An fdata object.
#' @param cluster_tolerance Relative tolerance for clustering nearby period
#'   candidates. Default: 0.1 (10% relative difference). Candidates within
#'   this tolerance are grouped into clusters.
#' @param min_cluster_size Minimum number of candidates required to form a
#'   valid cluster. Default: 1.
#'
#' @return A list of class "cfd_autoperiod_result" with components:
#' \describe{
#'   \item{period}{Primary detected period (best validated cluster center)}
#'   \item{confidence}{Combined confidence (ACF validation * spectral power)}
#'   \item{acf_validation}{ACF validation score for the primary period}
#'   \item{n_periods}{Number of validated period clusters}
#'   \item{periods}{All detected period cluster centers}
#'   \item{confidences}{Confidence scores for each detected period}
#' }
#'
#' @details
#' The CFDAutoperiod algorithm works in five stages:
#' \enumerate{
#'   \item \strong{Differencing}: First-order differencing removes polynomial trends
#'   \item \strong{FFT}: Computes periodogram on the detrended signal
#'   \item \strong{Peak Detection}: Finds all peaks above the noise floor
#'   \item \strong{Clustering}: Groups nearby period candidates into clusters
#'   \item \strong{ACF Validation}: Validates cluster centers using the ACF
#'     of the original (non-differenced) signal
#' }
#'
#' This method is particularly effective for:
#' \itemize{
#'   \item Signals with strong polynomial trends
#'   \item Multiple concurrent periodicities
#'   \item Signals where spectral leakage creates nearby spurious peaks
#' }
#'
#' @references
#' Puech, T., Boussard, M., D'Amato, A., & Millerand, G. (2020). A fully
#' automated periodicity detection in time series. In Advanced Analytics
#' and Learning on Temporal Data (pp. 43-54). Springer.
#'
#' @seealso \code{\link{autoperiod}} for the original Autoperiod algorithm,
#'   \code{\link{sazed}} for an ensemble method
#'
#' @export
#' @examples
#' # Generate data with trend
#' t <- seq(0, 20, length.out = 400)
#' X <- matrix(0.2 * t + sin(2 * pi * t / 2), nrow = 1)
#' fd <- fdata(X, argvals = t)
#'
#' # CFDAutoperiod handles trends via differencing
#' result <- cfd.autoperiod(fd)
#' print(result)
#'
#' # Multiple periods detected
#' X2 <- matrix(sin(2 * pi * t / 2) + 0.5 * sin(2 * pi * t / 5), nrow = 1)
#' fd2 <- fdata(X2, argvals = t)
#' result2 <- cfd.autoperiod(fd2)
#' print(result2$periods)  # All detected periods
cfd.autoperiod <- function(fdataobj, cluster_tolerance = 0.1,
                           min_cluster_size = 1) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("cfd.autoperiod not yet implemented for 2D functional data")
  }

  result <- .Call("wrap__seasonal_cfd_autoperiod",
                  fdataobj$data, fdataobj$argvals,
                  as.double(cluster_tolerance), as.integer(min_cluster_size))

  class(result) <- "cfd_autoperiod_result"
  result
}

#' Print method for cfd_autoperiod_result objects
#' @param x A cfd_autoperiod_result object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.cfd_autoperiod_result <- function(x, ...) {
  cat("CFDAutoperiod Detection\n")
  cat("-----------------------\n")
  cat(sprintf("Primary Period: %.4f\n", x$period))
  cat(sprintf("Confidence:     %.4f\n", x$confidence))
  cat(sprintf("ACF Validation: %.4f\n", x$acf_validation))
  cat(sprintf("Periods Found:  %d\n", x$n_periods))
  if (x$n_periods > 1) {
    cat("\nAll detected periods:\n")
    for (i in seq_along(x$periods)) {
      cat(sprintf("  Period %d: %.4f (conf: %.4f)\n",
                  i, x$periods[i], x$confidences[i]))
    }
  }
  invisible(x)
}

#' Autoperiod: Hybrid FFT + ACF Period Detection
#'
#' Implements the Autoperiod algorithm (Vlachos et al. 2005) which combines
#' FFT-based candidate detection with ACF validation and gradient ascent
#' refinement for robust period estimation.
#'
#' @param fdataobj An fdata object.
#' @param n_candidates Maximum number of FFT peaks to consider as candidates.
#'   Default: 5. More candidates increases robustness but also computation time.
#' @param gradient_steps Number of gradient ascent steps for period refinement.
#'   Default: 10. More steps improves precision.
#' @param detrend_method Detrending method to apply before period estimation:
#' \describe{
#'   \item{"none"}{No detrending (default)}
#'   \item{"linear"}{Remove linear trend}
#'   \item{"auto"}{Automatic AIC-based selection of detrending method}
#' }
#'
#' @return A list of class "autoperiod_result" with components:
#' \describe{
#'   \item{period}{Best detected period}
#'   \item{confidence}{Combined confidence (normalized FFT power * ACF validation)}
#'   \item{fft_power}{FFT power at the detected period}
#'   \item{acf_validation}{ACF validation score (0-1)}
#'   \item{n_candidates}{Number of candidates evaluated}
#'   \item{candidates}{Data frame of all candidate periods with their scores}
#' }
#'
#' @details
#' The Autoperiod algorithm works in three stages:
#' \enumerate{
#'   \item \strong{Candidate Detection}: Finds peaks in the FFT periodogram
#'   \item \strong{ACF Validation}: Validates each candidate using the
#'     autocorrelation function. Checks that the ACF shows a peak at the
#'     candidate period, and applies harmonic analysis to distinguish
#'     fundamental periods from harmonics.
#'   \item \strong{Gradient Refinement}: Refines each candidate using gradient
#'     ascent on the ACF to find the exact period that maximizes the ACF peak.
#' }
#'
#' The final period is chosen based on the product of normalized FFT power
#' and ACF validation score.
#'
#' @references
#' Vlachos, M., Yu, P., & Castelli, V. (2005). On periodicity detection and
#' structural periodic similarity. In Proceedings of the 2005 SIAM
#' International Conference on Data Mining.
#'
#' @seealso \code{\link{sazed}} for an ensemble method,
#'   \code{\link{estimate.period}} for simpler single-method estimation
#'
#' @export
#' @examples
#' # Generate seasonal data with period = 2
#' t <- seq(0, 20, length.out = 400)
#' X <- matrix(sin(2 * pi * t / 2) + 0.1 * rnorm(400), nrow = 1)
#' fd <- fdata(X, argvals = t)
#'
#' # Detect period using Autoperiod
#' result <- autoperiod(fd)
#' print(result)
#'
#' # View all candidates
#' print(result$candidates)
autoperiod <- function(fdataobj, n_candidates = 5, gradient_steps = 10,
                       detrend_method = c("none", "linear", "auto")) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("autoperiod not yet implemented for 2D functional data")
  }

  detrend_method <- match.arg(detrend_method)

  # Apply detrending if requested
  if (detrend_method != "none") {
    fdataobj <- detrend(fdataobj, method = detrend_method)
  }

  result <- .Call("wrap__seasonal_autoperiod",
                  fdataobj$data, fdataobj$argvals,
                  as.integer(n_candidates), as.integer(gradient_steps))

  # Convert candidates list to data frame for easier inspection
  if (result$n_candidates > 0) {
    result$candidates <- data.frame(
      period = result$candidates$period,
      fft_power = result$candidates$fft_power,
      acf_score = result$candidates$acf_score,
      combined_score = result$candidates$combined_score
    )
  } else {
    result$candidates <- data.frame(
      period = numeric(0),
      fft_power = numeric(0),
      acf_score = numeric(0),
      combined_score = numeric(0)
    )
  }

  class(result) <- "autoperiod_result"
  result
}

#' Print method for autoperiod_result objects
#' @param x An autoperiod_result object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.autoperiod_result <- function(x, ...) {
  cat("Autoperiod Detection\n")
  cat("--------------------\n")
  cat(sprintf("Period:         %.4f\n", x$period))
  cat(sprintf("Confidence:     %.4f\n", x$confidence))
  cat(sprintf("FFT Power:      %.4f\n", x$fft_power))
  cat(sprintf("ACF Validation: %.4f\n", x$acf_validation))
  cat(sprintf("Candidates:     %d\n", x$n_candidates))
  invisible(x)
}

#' SAZED: Spectral-ACF Zero-crossing Ensemble Detection
#'
#' A parameter-free ensemble method for robust period detection that combines
#' five different detection approaches and uses majority voting to determine
#' the consensus period.
#'
#' @param fdataobj An fdata object.
#' @param tolerance Relative tolerance for considering periods equal when voting.
#'   Default: 0.1 (10% relative difference). Use smaller values for stricter
#'   matching, larger values for more lenient matching.
#' @param detrend_method Detrending method to apply before period estimation:
#' \describe{
#'   \item{"none"}{No detrending (default)}
#'   \item{"linear"}{Remove linear trend}
#'   \item{"auto"}{Automatic AIC-based selection of detrending method}
#' }
#'
#' @return A list of class "sazed_result" with components:
#' \describe{
#'   \item{period}{Consensus period (average of agreeing components)}
#'   \item{confidence}{Confidence score (0-1, proportion of agreeing components)}
#'   \item{agreeing_components}{Number of components that agreed on the period}
#'   \item{components}{List of individual component estimates:}
#'   \describe{
#'     \item{spectral}{Period from FFT periodogram peak}
#'     \item{acf_peak}{Period from first ACF peak}
#'     \item{acf_average}{Weighted average of ACF peaks}
#'     \item{zero_crossing}{Period from ACF zero crossings}
#'     \item{spectral_diff}{Period from FFT on differenced signal}
#'   }
#' }
#'
#' @details
#' SAZED combines five detection methods:
#' \enumerate{
#'   \item \strong{Spectral}: Finds peaks in the FFT periodogram above the noise floor
#'   \item \strong{ACF Peak}: Identifies the first significant peak in the autocorrelation function
#'   \item \strong{ACF Average}: Computes a weighted mean of ACF peak locations
#'   \item \strong{Zero-crossing}: Estimates period from ACF zero-crossing intervals
#'   \item \strong{Spectral Diff}: Applies FFT to first-differenced signal (trend removal)
#' }
#'
#' The final period is chosen by majority voting: periods within the tolerance
#' are grouped together, and the group with the most members determines the
#' consensus. The returned period is the average of the agreeing estimates.
#'
#' This method is particularly robust because:
#' \itemize{
#'   \item It requires no tuning parameters (tolerance has sensible defaults)
#'   \item Multiple methods must agree for high confidence
#'   \item Differencing component handles trends automatically
#'   \item Works well across different signal types
#' }
#'
#' @seealso \code{\link{estimate.period}} for single-method estimation,
#'   \code{\link{detect.periods}} for detecting multiple concurrent periods
#'
#' @export
#' @examples
#' # Generate seasonal data with period = 2
#' t <- seq(0, 20, length.out = 400)
#' X <- matrix(sin(2 * pi * t / 2) + 0.1 * rnorm(400), nrow = 1)
#' fd <- fdata(X, argvals = t)
#'
#' # Detect period using SAZED
#' result <- sazed(fd)
#' print(result)  # Shows consensus period and component details
#'
#' # With trend - SAZED's spectral_diff component handles this
#' X_trend <- matrix(0.3 * t + sin(2 * pi * t / 2), nrow = 1)
#' fd_trend <- fdata(X_trend, argvals = t)
#' result <- sazed(fd_trend)
sazed <- function(fdataobj, tolerance = 0.1,
                  detrend_method = c("none", "linear", "auto")) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("sazed not yet implemented for 2D functional data")
  }

  detrend_method <- match.arg(detrend_method)

  # Apply detrending if requested
  if (detrend_method != "none") {
    fdataobj <- detrend(fdataobj, method = detrend_method)
  }

  result <- .Call("wrap__seasonal_sazed",
                  fdataobj$data, fdataobj$argvals, as.double(tolerance))

  class(result) <- "sazed_result"
  result
}

#' Print method for sazed_result objects
#' @param x A sazed_result object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.sazed_result <- function(x, ...) {
  cat("SAZED Period Detection\n")
  cat("----------------------\n")
  cat(sprintf("Period:     %.4f\n", x$period))
  cat(sprintf("Confidence: %.2f (%d/5 components agree)\n",
              x$confidence, x$agreeing_components))
  cat("\nComponent estimates:\n")
  cat(sprintf("  Spectral:      %.4f\n", x$components$spectral))
  cat(sprintf("  ACF Peak:      %.4f\n", x$components$acf_peak))
  cat(sprintf("  ACF Average:   %.4f\n", x$components$acf_average))
  cat(sprintf("  Zero-crossing: %.4f\n", x$components$zero_crossing))
  cat(sprintf("  Spectral Diff: %.4f\n", x$components$spectral_diff))
  invisible(x)
}

# ==============================================================================
# Lomb-Scargle Periodogram
# ==============================================================================

#' Lomb-Scargle Periodogram
#'
#' Computes the Lomb-Scargle periodogram for period detection in
#' unevenly-sampled data. The Lomb-Scargle method is designed for
#' irregularly spaced observations and reduces to the standard
#' periodogram for evenly-spaced data.
#'
#' @param fdataobj An fdata object. Can have regular or irregular sampling.
#' @param oversampling Oversampling factor for the frequency grid.
#'   Higher values give finer frequency resolution. Default: 4.
#' @param nyquist_factor Maximum frequency as a multiple of the pseudo-Nyquist
#'   frequency. Default: 1.
#'
#' @return A list of class "lomb_scargle_result" with components:
#' \describe{
#'   \item{frequencies}{Vector of evaluated frequencies}
#'   \item{periods}{Corresponding periods (1/frequency)}
#'   \item{power}{Normalized Lomb-Scargle power at each frequency}
#'   \item{peak_period}{Period with highest power}
#'   \item{peak_frequency}{Frequency with highest power}
#'   \item{peak_power}{Maximum power value}
#'   \item{false_alarm_probability}{False alarm probability at peak}
#'   \item{significance}{Significance level (1 - FAP)}
#' }
#'
#' @details
#' The Lomb-Scargle periodogram is particularly useful when:
#' - Data has gaps or missing observations
#' - Sampling is not uniform (e.g., astronomical observations)
#' - Working with irregular functional data
#'
#' The algorithm follows Scargle (1982) with significance estimation
#' from Horne & Baliunas (1986). For each test frequency, it computes:
#'
#' \deqn{P(\omega) = \frac{1}{2\sigma^2} \left[
#'   \frac{(\sum_j (y_j - \bar{y}) \cos\omega(t_j - \tau))^2}{\sum_j \cos^2\omega(t_j - \tau)} +
#'   \frac{(\sum_j (y_j - \bar{y}) \sin\omega(t_j - \tau))^2}{\sum_j \sin^2\omega(t_j - \tau)}
#' \right]}
#'
#' where \eqn{\tau} is a phase shift chosen to make the sine and cosine
#' terms orthogonal.
#'
#' @references
#' Scargle, J.D. (1982). Studies in astronomical time series analysis. II.
#' Statistical aspects of spectral analysis of unevenly spaced data.
#' The Astrophysical Journal, 263, 835-853.
#'
#' Horne, J.H., & Baliunas, S.L. (1986). A prescription for period analysis
#' of unevenly sampled time series. The Astrophysical Journal, 302, 757-763.
#'
#' @examples
#' # Regular sampling
#' t <- seq(0, 10, length.out = 200)
#' X <- matrix(sin(2 * pi * t / 2), nrow = 1)
#' fd <- fdata(X, argvals = t)
#' result <- lomb.scargle(fd)
#' print(result)
#'
#' # Irregular sampling (simulated)
#' set.seed(42)
#' t_irreg <- sort(runif(100, 0, 10))
#' X_irreg <- matrix(sin(2 * pi * t_irreg / 2), nrow = 1)
#' fd_irreg <- fdata(X_irreg, argvals = t_irreg)
#' result_irreg <- lomb.scargle(fd_irreg)
#'
#' @seealso \code{\link{estimate.period}}, \code{\link{sazed}}
#'
#' @export
lomb.scargle <- function(fdataobj, oversampling = 4, nyquist_factor = 1) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be an fdata object")
  }
  if (ncol(fdataobj$data) < 3) {
    stop("fdata must have at least 3 time points")
  }

  result <- .Call("wrap__seasonal_lomb_scargle",
                  fdataobj$data, fdataobj$argvals,
                  as.double(oversampling), as.double(nyquist_factor))

  class(result) <- "lomb_scargle_result"
  result
}

#' Print method for lomb_scargle_result objects
#' @param x A lomb_scargle_result object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.lomb_scargle_result <- function(x, ...) {
  cat("Lomb-Scargle Periodogram\n")
  cat("------------------------\n")
  cat(sprintf("Peak period:    %.4f\n", x$peak_period))
  cat(sprintf("Peak frequency: %.4f\n", x$peak_frequency))
  cat(sprintf("Peak power:     %.4f\n", x$peak_power))
  cat(sprintf("FAP:            %.4e\n", x$false_alarm_probability))
  cat(sprintf("Significance:   %.4f\n", x$significance))
  cat(sprintf("\nFrequency grid: %d points (%.4f to %.4f)\n",
              length(x$frequencies),
              min(x$frequencies, na.rm = TRUE),
              max(x$frequencies, na.rm = TRUE)))
  invisible(x)
}

#' Plot method for lomb_scargle_result objects
#' @param x A lomb_scargle_result object.
#' @param ... Additional arguments passed to \code{plot}.
#' @return Invisibly returns \code{NULL}.
#' @export
plot.lomb_scargle_result <- function(x, ...) {
  plot(x$frequencies, x$power, type = "l",
       xlab = "Frequency", ylab = "Power",
       main = "Lomb-Scargle Periodogram", ...)
  abline(v = x$peak_frequency, col = "red", lty = 2)
  legend("topright",
         legend = sprintf("Peak: period=%.4f", x$peak_period),
         col = "red", lty = 2, bty = "n")
}

# ==============================================================================
# Matrix Profile (STOMP Algorithm)
# ==============================================================================

#' Matrix Profile for Motif Discovery and Period Detection
#'
#' Computes the Matrix Profile using the STOMP (Scalable Time series Ordered-search
#' Matrix Profile) algorithm. The Matrix Profile stores the z-normalized Euclidean
#' distance between each subsequence and its nearest neighbor, enabling efficient
#' motif discovery and period detection for non-sinusoidal patterns.
#'
#' @param fdataobj An fdata object.
#' @param subsequence_length Length of subsequences to compare. If NULL,
#'   automatically determined as approximately 1/4 of series length.
#' @param exclusion_zone Fraction of subsequence length to exclude around each
#'   position to prevent trivial self-matches. Default: 0.5.
#'
#' @return A list of class "matrix_profile_result" with components:
#' \describe{
#'   \item{profile}{Numeric vector of minimum z-normalized distances at each position}
#'   \item{profile_index}{Integer vector of nearest neighbor indices (1-based)}
#'   \item{subsequence_length}{Subsequence length used}
#'   \item{detected_periods}{Candidate periods detected from arc analysis (top 5)}
#'   \item{arc_counts}{Arc counts at each index distance (for diagnostics)}
#'   \item{primary_period}{Most prominent detected period}
#'   \item{confidence}{Confidence score (0-1) based on arc prominence}
#' }
#'
#' @details
#' The Matrix Profile algorithm (Yeh et al., 2016; Zhu et al., 2016) is particularly
#' suited for:
#' \itemize{
#'   \item Non-sinusoidal repeating patterns (unlike FFT/spectral methods)
#'   \item Motif discovery (finding repeated subsequences)
#'   \item Anomaly detection (subsequences with no good match)
#' }
#'
#' The STOMP variant uses O(n^2) time complexity but is highly cache-efficient.
#' For very long series (>10000 points), consider downsampling.
#'
#' Period detection is based on "arc analysis": counting how often the nearest
#' neighbor is a fixed distance away. Peaks in arc counts indicate periodicity.
#'
#' @references
#' Yeh, C. C. M., et al. (2016). Matrix profile I: All pairs similarity joins
#' for time series: A unifying view that includes motifs, discords and shapelets.
#' ICDM 2016.
#'
#' Zhu, Y., et al. (2016). Matrix profile II: Exploiting a novel algorithm and
#' GPUs to break the one hundred million barrier for time series motifs and joins.
#' ICDM 2016.
#'
#' @export
#' @examples
#' # Periodic sawtooth wave (non-sinusoidal)
#' t <- seq(0, 10, length.out = 200)
#' period <- 2
#' X <- matrix((t %% period) / period, nrow = 1)
#' fd <- fdata(X, argvals = t)
#'
#' # Compute Matrix Profile
#' result <- matrix.profile(fd, subsequence_length = 30)
#' print(result)
#' plot(result)
#'
#' # Sine wave for comparison
#' X_sine <- matrix(sin(2 * pi * t / period), nrow = 1)
#' fd_sine <- fdata(X_sine, argvals = t)
#' result_sine <- matrix.profile(fd_sine, subsequence_length = 30)
#'
#' @seealso \code{\link{estimate.period}}, \code{\link{sazed}}, \code{\link{lomb.scargle}}
#'
matrix.profile <- function(fdataobj, subsequence_length = NULL, exclusion_zone = 0.5) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be an fdata object")
  }
  if (ncol(fdataobj$data) < 8) {
    stop("fdata must have at least 8 time points")
  }

  subseq_len <- if (is.null(subsequence_length)) -1L else as.integer(subsequence_length)

  result <- .Call("wrap__seasonal_matrix_profile",
                  fdataobj$data, subseq_len, as.double(exclusion_zone))

  class(result) <- "matrix_profile_result"
  result
}

#' Print method for matrix_profile_result objects
#' @param x A matrix_profile_result object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.matrix_profile_result <- function(x, ...) {
  cat("Matrix Profile (STOMP)\n")
  cat("----------------------\n")
  cat(sprintf("Subsequence length: %d\n", x$subsequence_length))
  cat(sprintf("Profile length:     %d\n", length(x$profile)))
  cat(sprintf("Primary period:     %.2f\n", x$primary_period))
  cat(sprintf("Confidence:         %.4f\n", x$confidence))
  if (length(x$detected_periods) > 0) {
    cat(sprintf("\nTop detected periods: %s\n",
                paste(round(x$detected_periods, 2), collapse = ", ")))
  }
  cat(sprintf("\nProfile statistics:\n"))
  cat(sprintf("  Min:  %.4f\n", min(x$profile, na.rm = TRUE)))
  cat(sprintf("  Mean: %.4f\n", mean(x$profile, na.rm = TRUE)))
  cat(sprintf("  Max:  %.4f\n", max(x$profile, na.rm = TRUE)))
  invisible(x)
}

#' Plot method for matrix_profile_result objects
#' @param x A matrix_profile_result object.
#' @param type Plot type: "profile", "arcs", or "both".
#' @param ... Additional arguments passed to \code{plot}.
#' @return Invisibly returns \code{NULL}.
#' @export
plot.matrix_profile_result <- function(x, type = c("profile", "arcs", "both"), ...) {
  type <- match.arg(type)

  if (type == "both") {
    old_par <- par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
    on.exit(par(old_par))
  }

  if (type %in% c("profile", "both")) {
    plot(seq_along(x$profile), x$profile, type = "l",
         xlab = "Position", ylab = "Distance",
         main = "Matrix Profile", col = "steelblue", ...)
  }

  if (type %in% c("arcs", "both")) {
    n <- length(x$arc_counts)
    # Skip very small distances (exclusion zone artifacts)
    start_idx <- max(1, x$subsequence_length %/% 2)
    if (start_idx < n) {
      plot(start_idx:n, x$arc_counts[start_idx:n], type = "h",
           xlab = "Index Distance", ylab = "Arc Count",
           main = "Arc Counts (for Period Detection)", col = "darkgreen", ...)
      if (!is.na(x$primary_period) && x$primary_period > 0) {
        abline(v = x$primary_period, col = "red", lty = 2, lwd = 2)
        legend("topright",
               legend = sprintf("Primary period: %.2f", x$primary_period),
               col = "red", lty = 2, lwd = 2, bty = "n")
      }
    }
  }
}

# ==============================================================================
# STL Decomposition (Cleveland et al., 1990)
# ==============================================================================

#' STL Decomposition: Seasonal and Trend decomposition using LOESS
#'
#' Performs STL (Seasonal and Trend decomposition using LOESS) on functional data
#' following Cleveland et al. (1990). This is a robust iterative procedure that
#' separates a time series into trend, seasonal, and remainder components.
#'
#' @param fdataobj An fdata object.
#' @param period Integer. The seasonal period (number of observations per cycle).
#' @param s.window Seasonal smoothing window. Must be odd. If NULL, defaults to 7.
#'   Larger values produce smoother seasonal components.
#' @param t.window Trend smoothing window. Must be odd. If NULL, automatically
#'   calculated based on period and s.window.
#' @param robust Logical. If TRUE, performs robustness iterations to downweight
#'   outliers using bisquare weighting. Default: TRUE.
#'
#' @return A list of class "stl_result" with components:
#' \describe{
#'   \item{trend}{fdata object containing trend components}
#'   \item{seasonal}{fdata object containing seasonal components}
#'   \item{remainder}{fdata object containing remainder (residual) components}
#'   \item{weights}{Matrix of robustness weights (1 = full weight, 0 = outlier)}
#'   \item{period}{The period used}
#'   \item{s.window}{Seasonal smoothing window used}
#'   \item{t.window}{Trend smoothing window used}
#'   \item{inner.iterations}{Number of inner loop iterations}
#'   \item{outer.iterations}{Number of outer (robustness) iterations}
#'   \item{call}{The function call}
#' }
#'
#' @details
#' The STL algorithm proceeds as follows:
#'
#' **Inner Loop** (repeated n.inner times):
#' 1. Detrending: Subtract current trend estimate
#' 2. Cycle-subseries smoothing: Smooth values at each seasonal position across cycles
#' 3. Low-pass filtering: Remove high-frequency noise
#' 4. Detrending the smoothed cycle-subseries
#' 5. Deseasonalizing: Subtract seasonal from original data
#' 6. Trend smoothing: Apply LOESS to deseasonalized data
#'
#' **Outer Loop** (for robustness):
#' 1. Compute residuals from current decomposition
#' 2. Calculate robustness weights using bisquare function
#' 3. Re-run inner loop with weighted smoothing
#'
#' STL is particularly effective for:
#' \itemize{
#'   \item Long time series with many cycles
#'   \item Data with outliers (when robust=TRUE)
#'   \item Slowly changing seasonal patterns
#' }
#'
#' @references
#' Cleveland, R. B., Cleveland, W. S., McRae, J. E., & Terpenning, I. (1990).
#' STL: A Seasonal-Trend Decomposition Procedure Based on Loess.
#' Journal of Official Statistics, 6(1), 3-73.
#'
#' @export
#' @examples
#' # Create seasonal data with trend
#' t <- seq(0, 20, length.out = 400)
#' period <- 2  # corresponds to 40 observations
#' period_obs <- 40
#' X <- matrix(0.05 * t + sin(2 * pi * t / period) + rnorm(length(t), sd = 0.2), nrow = 1)
#' fd <- fdata(X, argvals = t)
#'
#' # Perform STL decomposition
#' result <- stl.fd(fd, period = period_obs)
#' print(result)
#'
#' # Plot the decomposition
#' plot(result)
#'
#' # Non-robust version (faster but sensitive to outliers)
#' result_fast <- stl.fd(fd, period = period_obs, robust = FALSE)
#'
#' @seealso \code{\link{decompose}}, \code{\link{detrend}}, \code{\link{seasonal.strength}}
#'
stl.fd <- function(fdataobj, period, s.window = NULL, t.window = NULL, robust = TRUE) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be an fdata object")
  }

  m <- ncol(fdataobj$data)
  if (m < 2 * period) {
    stop("Series must have at least 2 complete cycles (", 2 * period, " observations)")
  }
  if (period < 2) {
    stop("Period must be at least 2")
  }

  s_win <- if (is.null(s.window)) -1L else as.integer(s.window)
  t_win <- if (is.null(t.window)) -1L else as.integer(t.window)

  result <- .Call("wrap__seasonal_stl",
                  fdataobj$data, as.integer(period),
                  s_win, t_win, as.logical(robust))

  # Construct fdata objects for components
  out <- list(
    trend = fdata(result$trend, argvals = fdataobj$argvals, rangeval = fdataobj$rangeval),
    seasonal = fdata(result$seasonal, argvals = fdataobj$argvals, rangeval = fdataobj$rangeval),
    remainder = fdata(result$remainder, argvals = fdataobj$argvals, rangeval = fdataobj$rangeval),
    weights = result$weights,
    period = result$period,
    s.window = result$s_window,
    t.window = result$t_window,
    inner.iterations = result$inner_iterations,
    outer.iterations = result$outer_iterations,
    call = match.call()
  )

  class(out) <- "stl_result"
  out
}

#' Print method for stl_result objects
#' @param x An stl_result object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.stl_result <- function(x, ...) {
  cat("STL Decomposition\n")
  cat("-----------------\n")
  cat(sprintf("Period:           %d observations\n", x$period))
  cat(sprintf("Seasonal window:  %d\n", x$s.window))
  cat(sprintf("Trend window:     %d\n", x$t.window))
  cat(sprintf("Inner iterations: %d\n", x$inner.iterations))
  cat(sprintf("Outer iterations: %d (robust=%s)\n",
              x$outer.iterations, x$outer.iterations > 1))
  cat(sprintf("\nNumber of curves: %d\n", nrow(x$trend$data)))
  cat(sprintf("Series length:    %d\n", ncol(x$trend$data)))

  # Variance decomposition
  var_trend <- mean(apply(x$trend$data, 1, var))
  var_seasonal <- mean(apply(x$seasonal$data, 1, var))
  var_remainder <- mean(apply(x$remainder$data, 1, var))
  var_total <- var_trend + var_seasonal + var_remainder

  cat(sprintf("\nVariance decomposition:\n"))
  cat(sprintf("  Trend:     %.1f%%\n", 100 * var_trend / var_total))
  cat(sprintf("  Seasonal:  %.1f%%\n", 100 * var_seasonal / var_total))
  cat(sprintf("  Remainder: %.1f%%\n", 100 * var_remainder / var_total))

  invisible(x)
}

#' Plot method for stl_result objects
#' @param x An stl_result object.
#' @param curves Indices of curves to plot.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns \code{NULL}.
#' @export
plot.stl_result <- function(x, curves = 1, ...) {
  n_curves <- min(length(curves), nrow(x$trend$data))
  curves <- curves[1:n_curves]

  old_par <- par(mfrow = c(4, 1), mar = c(2, 4, 2, 1), oma = c(2, 0, 2, 0))
  on.exit(par(old_par))

  argvals <- x$trend$argvals

  for (i in seq_along(curves)) {
    curve_idx <- curves[i]

    # Original (reconstructed)
    original <- x$trend$data[curve_idx, ] + x$seasonal$data[curve_idx, ] + x$remainder$data[curve_idx, ]
    plot(argvals, original, type = "l", ylab = "Data",
         main = if (i == 1) paste("STL Decomposition - Curve", curve_idx) else "", ...)

    # Trend
    plot(argvals, x$trend$data[curve_idx, ], type = "l", ylab = "Trend", col = "blue", ...)

    # Seasonal
    plot(argvals, x$seasonal$data[curve_idx, ], type = "l", ylab = "Seasonal", col = "darkgreen", ...)

    # Remainder
    plot(argvals, x$remainder$data[curve_idx, ], type = "l", ylab = "Remainder", col = "gray50", ...)
    abline(h = 0, lty = 2)
  }

  mtext("Time", side = 1, outer = TRUE)
}

# ==============================================================================
# Singular Spectrum Analysis (SSA)
# ==============================================================================

#' Singular Spectrum Analysis (SSA) for Time Series Decomposition
#'
#' Performs Singular Spectrum Analysis on functional data to decompose each curve
#' into trend, seasonal (oscillatory), and noise components. SSA is a model-free,
#' non-parametric technique based on singular value decomposition of the
#' trajectory matrix.
#'
#' @param fdataobj An fdata object.
#' @param window.length Embedding window length (L). If NULL, automatically
#'   determined as min(n/2, 50). Larger values capture longer-term patterns.
#' @param n.components Number of SVD components to extract. Default: 10.
#'   More components allow finer decomposition but increase noise.
#'
#' @return A list of class "ssa_result" with components:
#' \describe{
#'   \item{trend}{fdata object containing reconstructed trend component}
#'   \item{seasonal}{fdata object containing reconstructed seasonal component}
#'   \item{noise}{fdata object containing noise/residual component}
#'   \item{singular.values}{Singular values from SVD (sorted descending)}
#'   \item{contributions}{Proportion of variance explained by each component}
#'   \item{window.length}{Window length used}
#'   \item{n.components}{Number of components extracted}
#'   \item{detected.period}{Auto-detected period (if any)}
#'   \item{confidence}{Confidence score for detected period}
#'   \item{call}{The function call}
#' }
#'
#' @details
#' The SSA algorithm consists of four stages:
#'
#' **1. Embedding**: The time series is converted into a trajectory matrix
#' by arranging lagged versions of the series as columns.
#'
#' **2. SVD Decomposition**: Singular value decomposition of the trajectory
#' matrix produces orthogonal components.
#'
#' **3. Grouping**: Components are grouped into trend (slowly varying),
#' seasonal (oscillatory), and noise. Auto-detection uses sign change analysis
#' and autocorrelation.
#'
#' **4. Reconstruction**: Diagonal averaging (Hankelization) converts grouped
#' trajectory matrices back to time series.
#'
#' SSA is particularly suited for:
#' \itemize{
#'   \item Short time series where spectral methods fail
#'   \item Noisy data with weak periodic signals
#'   \item Non-stationary data with changing trend
#'   \item Separating multiple periodicities
#' }
#'
#' @references
#' Golyandina, N., & Zhigljavsky, A. (2013). Singular Spectrum Analysis for
#' Time Series. Springer.
#'
#' Elsner, J. B., & Tsonis, A. A. (1996). Singular Spectrum Analysis: A New
#' Tool in Time Series Analysis. Plenum Press.
#'
#' @export
#' @examples
#' # Signal with trend + seasonal + noise
#' t <- seq(0, 10, length.out = 200)
#' X <- matrix(0.05 * t + sin(2 * pi * t / 1.5) + rnorm(length(t), sd = 0.3), nrow = 1)
#' fd <- fdata(X, argvals = t)
#'
#' # Perform SSA
#' result <- ssa.fd(fd)
#' print(result)
#'
#' # Plot components
#' plot(result)
#'
#' # Examine singular value spectrum (scree plot)
#' plot(result, type = "spectrum")
#'
#' @seealso \code{\link{stl.fd}}, \code{\link{decompose}}, \code{\link{estimate.period}}
#'
ssa.fd <- function(fdataobj, window.length = NULL, n.components = 10) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be an fdata object")
  }

  m <- ncol(fdataobj$data)
  if (m < 4) {
    stop("Series must have at least 4 observations")
  }

  win_len <- if (is.null(window.length)) -1L else as.integer(window.length)
  n_comp <- as.integer(n.components)

  result <- .Call("wrap__seasonal_ssa",
                  fdataobj$data, win_len, n_comp)

  # Construct fdata objects for components
  out <- list(
    trend = fdata(result$trend, argvals = fdataobj$argvals, rangeval = fdataobj$rangeval),
    seasonal = fdata(result$seasonal, argvals = fdataobj$argvals, rangeval = fdataobj$rangeval),
    noise = fdata(result$noise, argvals = fdataobj$argvals, rangeval = fdataobj$rangeval),
    singular.values = result$singular_values,
    contributions = result$contributions,
    window.length = result$window_length,
    n.components = result$n_components,
    detected.period = result$detected_period,
    confidence = result$confidence,
    call = match.call()
  )

  class(out) <- "ssa_result"
  out
}

#' Print method for ssa_result objects
#' @param x An ssa_result object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.ssa_result <- function(x, ...) {
  cat("Singular Spectrum Analysis (SSA)\n")
  cat("--------------------------------\n")
  cat(sprintf("Window length:    %d\n", x$window.length))
  cat(sprintf("N components:     %d\n", x$n.components))
  cat(sprintf("Detected period:  %.2f\n", x$detected.period))
  cat(sprintf("Confidence:       %.4f\n", x$confidence))
  cat(sprintf("\nNumber of curves: %d\n", nrow(x$trend$data)))
  cat(sprintf("Series length:    %d\n", ncol(x$trend$data)))

  # Top singular value contributions
  if (length(x$contributions) > 0) {
    cat(sprintf("\nTop component contributions:\n"))
    n_show <- min(5, length(x$contributions))
    for (i in seq_len(n_show)) {
      cat(sprintf("  Component %d: %.1f%%\n", i, 100 * x$contributions[i]))
    }
    cum_contrib <- sum(x$contributions[1:n_show])
    cat(sprintf("  Cumulative:   %.1f%%\n", 100 * cum_contrib))
  }

  # Variance decomposition
  var_trend <- mean(apply(x$trend$data, 1, var))
  var_seasonal <- mean(apply(x$seasonal$data, 1, var))
  var_noise <- mean(apply(x$noise$data, 1, var))
  var_total <- var_trend + var_seasonal + var_noise

  cat(sprintf("\nVariance decomposition:\n"))
  cat(sprintf("  Trend:    %.1f%%\n", 100 * var_trend / var_total))
  cat(sprintf("  Seasonal: %.1f%%\n", 100 * var_seasonal / var_total))
  cat(sprintf("  Noise:    %.1f%%\n", 100 * var_noise / var_total))

  invisible(x)
}

#' Plot method for ssa_result objects
#' @param x An ssa_result object.
#' @param type Plot type: "decomposition" or "spectrum".
#' @param curves Indices of curves to plot.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns \code{NULL}.
#' @export
plot.ssa_result <- function(x, type = c("decomposition", "spectrum"), curves = 1, ...) {
  type <- match.arg(type)

  if (type == "spectrum") {
    # Scree plot of singular values
    sv <- x$singular.values
    if (length(sv) == 0) {
      warning("No singular values available")
      return(invisible(x))
    }

    old_par <- par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
    on.exit(par(old_par))

    # Singular value spectrum
    plot(seq_along(sv), sv, type = "b", pch = 19,
         xlab = "Component", ylab = "Singular Value",
         main = "Singular Value Spectrum", col = "steelblue", ...)

    # Cumulative contribution
    cum_contrib <- cumsum(x$contributions)
    plot(seq_along(cum_contrib), 100 * cum_contrib, type = "b", pch = 19,
         xlab = "Component", ylab = "Cumulative Variance (%)",
         main = "Explained Variance", col = "darkgreen", ylim = c(0, 100), ...)
    abline(h = 90, lty = 2, col = "gray50")

  } else {
    # Decomposition plot
    n_curves <- min(length(curves), nrow(x$trend$data))
    curves <- curves[1:n_curves]

    old_par <- par(mfrow = c(4, 1), mar = c(2, 4, 2, 1), oma = c(2, 0, 2, 0))
    on.exit(par(old_par))

    argvals <- x$trend$argvals

    for (i in seq_along(curves)) {
      curve_idx <- curves[i]

      # Original (reconstructed)
      original <- x$trend$data[curve_idx, ] + x$seasonal$data[curve_idx, ] + x$noise$data[curve_idx, ]
      plot(argvals, original, type = "l", ylab = "Data",
           main = if (i == 1) paste("SSA Decomposition - Curve", curve_idx) else "", ...)

      # Trend
      plot(argvals, x$trend$data[curve_idx, ], type = "l", ylab = "Trend", col = "blue", ...)

      # Seasonal
      plot(argvals, x$seasonal$data[curve_idx, ], type = "l", ylab = "Seasonal", col = "darkgreen", ...)

      # Noise
      plot(argvals, x$noise$data[curve_idx, ], type = "l", ylab = "Noise", col = "gray50", ...)
      abline(h = 0, lty = 2)
    }

    mtext("Time", side = 1, outer = TRUE)
  }
}

# ==============================================================================
# Multiple Period Detection
# ==============================================================================

#' Detect Multiple Concurrent Periods
#'
#' Detects multiple periodicities in functional data using iterative residual
#' subtraction. At each iteration, the dominant period is detected using FFT,
#' its sinusoidal component is subtracted, and the process repeats on the
#' residual until stopping criteria are met.
#'
#' @param fdataobj An fdata object.
#' @param max_periods Maximum number of periods to detect. Default: 3.
#' @param min_confidence Minimum FFT confidence to continue detection.
#'   Default: 0.5.
#' @param min_strength Minimum seasonal strength to continue detection.
#'   Default: 0.2.
#' @param detrend_method Detrending method to apply before period detection:
#' \describe{
#'   \item{"auto"}{Automatic AIC-based selection of detrending method (default)}
#'   \item{"none"}{No detrending}
#'   \item{"linear"}{Remove linear trend}
#' }
#'
#' @return A list with components:
#' \describe{
#'   \item{periods}{Numeric vector of detected periods}
#'   \item{confidence}{FFT confidence for each period}
#'   \item{strength}{Seasonal strength for each period}
#'   \item{amplitude}{Amplitude of the sinusoidal component}
#'   \item{phase}{Phase of the sinusoidal component (radians)}
#'   \item{n_periods}{Number of periods detected}
#' }
#'
#' @details
#' The function uses two stopping criteria:
#' \itemize{
#'   \item FFT confidence: How prominent the dominant frequency is
#'   \item Seasonal strength: How much variance is explained by the periodicity
#' }
#'
#' Both must exceed their thresholds for detection to continue. Higher thresholds
#' result in fewer (but more reliable) detected periods.
#'
#' Periods are detected in order of amplitude (FFT power), not period length.
#' A weak yearly cycle will be detected after a strong weekly cycle.
#'
#' Trends can interfere with period detection. The default "auto" detrending
#' automatically selects an appropriate method to remove trends.
#'
#' @seealso \code{\link{estimate.period}} for single period estimation,
#'   \code{\link{detrend}} for standalone detrending
#'
#' @export
#' @examples
#' # Signal with two periods: 2 and 7
#' t <- seq(0, 20, length.out = 400)
#' X <- sin(2 * pi * t / 2) + 0.6 * sin(2 * pi * t / 7)
#' fd <- fdata(matrix(X, nrow = 1), argvals = t)
#'
#' # Detect multiple periods
#' result <- detect.periods(fd, max_periods = 3)
#' print(result$periods)  # Should find approximately 2 and 7
#' print(result$n_periods)  # Should be 2
detect.periods <- function(fdataobj, max_periods = 3,
                                     min_confidence = 0.5,
                                     min_strength = 0.2,
                                     detrend_method = c("auto", "none", "linear")) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("detect.periods not yet implemented for 2D functional data")
  }

  detrend_method <- match.arg(detrend_method)

  # Apply detrending if requested
  if (detrend_method != "none") {
    fdataobj <- detrend(fdataobj, method = detrend_method)
  }

  # Pure R implementation using iterative residual subtraction
  periods <- numeric(0)
  confidence <- numeric(0)
  strength <- numeric(0)
  amplitude <- numeric(0)

  residual <- fdataobj
  tt <- fdataobj$argvals

  for (i in seq_len(max_periods)) {
    # Estimate dominant period
    est <- estimate.period(residual, method = "fft")
    if (est$confidence < min_confidence) break

    # Check seasonal strength
    ss <- seasonal.strength(residual, period = est$period)
    if (ss < min_strength) break

    # Compute amplitude via Fourier coefficients
    omega <- 2 * pi / est$period
    cos_comp <- cos(omega * tt)
    sin_comp <- sin(omega * tt)
    y <- residual$data[1, ]
    a <- 2 * mean(y * cos_comp)
    b <- 2 * mean(y * sin_comp)
    amp <- sqrt(a^2 + b^2)

    # Store results
    periods <- c(periods, est$period)
    confidence <- c(confidence, est$confidence)
    strength <- c(strength, ss)
    amplitude <- c(amplitude, amp)

    # Subtract fitted sinusoid from residual
    fitted <- a * cos_comp + b * sin_comp
    residual$data <- residual$data - matrix(fitted, nrow = 1)
  }

  result <- list(
    periods = periods,
    confidence = confidence,
    strength = strength,
    amplitude = amplitude,
    n_periods = length(periods)
  )

  class(result) <- "multiple_periods"
  result
}

#' Print method for multiple_periods objects
#' @param x A multiple_periods object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.multiple_periods <- function(x, ...) {
  cat("Multiple Period Detection\n")
  cat("-------------------------\n")
  cat(sprintf("Periods detected: %d\n\n", x$n_periods))

  if (x$n_periods > 0) {
    for (i in seq_len(x$n_periods)) {
      cat(sprintf("Period %d: %.3f (confidence=%.3f, strength=%.3f, amplitude=%.3f)\n",
                  i, x$periods[i], x$confidence[i], x$strength[i], x$amplitude[i]))
    }
  } else {
    cat("No significant periods detected.\n")
  }
  invisible(x)
}

# ==============================================================================
# Peak Detection
# ==============================================================================

#' Detect Peaks in Functional Data
#'
#' Detects local maxima (peaks) in functional data using derivative
#' zero-crossings. Returns peak times, values, and prominence measures.
#'
#' @param fdataobj An fdata object.
#' @param min_distance Minimum time between peaks. Default: NULL (no constraint).
#' @param min_prominence Minimum prominence for a peak (0-1 scale). Peaks with
#'   lower prominence are filtered out. Default: NULL (no filter).
#' @param smooth_first Logical. If TRUE, apply Fourier basis smoothing before
#'   peak detection. Recommended for noisy data. Default: FALSE.
#' @param smooth_nbasis Number of Fourier basis functions for smoothing.
#'   If NULL and smooth_first=TRUE, uses GCV to automatically select
#'   optimal nbasis (range 5-25). Default: NULL (auto).
#' @param detrend_method Detrending method to apply before peak detection:
#' \describe{
#'   \item{"none"}{No detrending (default)}
#'   \item{"linear"}{Remove linear trend}
#'   \item{"auto"}{Automatic AIC-based selection of detrending method}
#' }
#'
#' @return A list with components:
#' \describe{
#'   \item{peaks}{List of data frames, one per curve, with columns: time, value, prominence}
#'   \item{inter_peak_distances}{List of numeric vectors with distances between consecutive peaks}
#'   \item{mean_period}{Mean inter-peak distance across all curves (estimates period)}
#' }
#'
#' @details
#' Peak prominence measures how much a peak stands out from its surroundings.
#' It is computed as the height difference between the peak and the highest
#' of the two minimum values on either side, normalized by the data range.
#'
#' Fourier basis smoothing is ideal for seasonal signals because it naturally
#' captures periodic patterns without introducing boundary artifacts.
#'
#' For data with trends, use detrend_method to remove the trend before
#' detecting peaks. This prevents the trend from affecting peak prominence
#' calculations.
#'
#' @export
#' @examples
#' # Generate data with clear peaks
#' t <- seq(0, 10, length.out = 200)
#' X <- matrix(sin(2 * pi * t / 2), nrow = 1)
#' fd <- fdata(X, argvals = t)
#'
#' # Detect peaks
#' peaks <- detect.peaks(fd, min_distance = 1.5)
#' print(peaks$mean_period)  # Should be close to 2
#'
#' # With automatic Fourier smoothing (GCV selects nbasis)
#' peaks_smooth <- detect.peaks(fd, min_distance = 1.5, smooth_first = TRUE)
#'
#' # With detrending for trending data
#' X_trend <- matrix(2 + 0.5 * t + sin(2 * pi * t / 2), nrow = 1)
#' fd_trend <- fdata(X_trend, argvals = t)
#' peaks_det <- detect.peaks(fd_trend, detrend_method = "linear")
detect.peaks <- function(fdataobj, min_distance = NULL, min_prominence = NULL,
                         smooth_first = FALSE, smooth_nbasis = NULL,
                         detrend_method = c("none", "linear", "auto")) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("detect.peaks not yet implemented for 2D functional data")
  }

  detrend_method <- match.arg(detrend_method)

  # Apply detrending if requested
  if (detrend_method != "none") {
    fdataobj <- detrend(fdataobj, method = detrend_method)
  }

  # Handle NULL values - pass as NULL Robj
  min_dist_arg <- if (is.null(min_distance)) NULL else as.double(min_distance)
  min_prom_arg <- if (is.null(min_prominence)) NULL else as.double(min_prominence)
  # NULL smooth_nbasis triggers automatic GCV selection
  nbasis_arg <- if (is.null(smooth_nbasis)) NULL else as.integer(smooth_nbasis)

  result <- .Call("wrap__seasonal_detect_peaks",
                  fdataobj$data, fdataobj$argvals,
                  min_dist_arg, min_prom_arg,
                  as.logical(smooth_first), nbasis_arg)

  # Convert peaks list to data frames
  result$peaks <- lapply(result$peaks, function(p) {
    if (length(p$time) == 0) {
      data.frame(time = numeric(0), value = numeric(0), prominence = numeric(0))
    } else {
      data.frame(time = p$time, value = p$value, prominence = p$prominence)
    }
  })

  class(result) <- "peak_detection"
  result
}

#' Print method for peak_detection objects
#' @param x A peak_detection object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.peak_detection <- function(x, ...) {
  n_curves <- length(x$peaks)
  total_peaks <- sum(sapply(x$peaks, nrow))
  cat("Peak Detection Result\n")
  cat("---------------------\n")
  cat(sprintf("Number of curves:  %d\n", n_curves))
  cat(sprintf("Total peaks found: %d\n", total_peaks))
  cat(sprintf("Mean period:       %.4f\n", x$mean_period))
  invisible(x)
}

# ==============================================================================
# Seasonal Strength
# ==============================================================================

#' Measure Seasonal Strength
#'
#' Computes the strength of seasonality in functional data. Values range from
#' 0 (no seasonality) to 1 (pure seasonal signal).
#'
#' @param fdataobj An fdata object.
#' @param period Known or estimated period. If NULL, period is estimated
#'   automatically using FFT.
#' @param method Method for computing strength:
#'   \describe{
#'     \item{"variance"}{Variance decomposition: Var(seasonal) / Var(total)}
#'     \item{"spectral"}{Spectral: power at seasonal frequencies / total power}
#'     \item{"wavelet"}{Wavelet: Morlet wavelet power at seasonal period / total variance}
#'   }
#' @param n_harmonics Number of Fourier harmonics to use (for variance method).
#'   Default: 3.
#' @param detrend_method Detrending method to apply before computing strength:
#' \describe{
#'   \item{"none"}{No detrending (default)}
#'   \item{"linear"}{Remove linear trend}
#'   \item{"auto"}{Automatic AIC-based selection of detrending method}
#' }
#'
#' @return A numeric value between 0 and 1 representing seasonal strength.
#'
#' @details
#' The variance method decomposes the signal into a seasonal component
#' (using Fourier basis with the specified period) and computes the
#' proportion of variance explained by the seasonal component.
#'
#' The spectral method computes the proportion of total spectral power
#' that falls at the seasonal frequency and its harmonics.
#'
#' The wavelet method uses a Morlet wavelet to measure power at the target
#' period. It provides time-localized frequency information and is robust
#' to non-stationary signals.
#'
#' Trends can artificially lower the seasonal strength measure by
#' contributing non-seasonal variance. Use detrend_method to remove
#' trends before computing strength.
#'
#' @export
#' @examples
#' # Pure seasonal signal
#' t <- seq(0, 10, length.out = 200)
#' X <- matrix(sin(2 * pi * t / 2), nrow = 1)
#' fd_seasonal <- fdata(X, argvals = t)
#' seasonal.strength(fd_seasonal, period = 2)  # Should be close to 1
#'
#' # Pure noise
#' X_noise <- matrix(rnorm(200), nrow = 1)
#' fd_noise <- fdata(X_noise, argvals = t)
#' seasonal.strength(fd_noise, period = 2)  # Should be close to 0
#'
#' # Trending data - detrending improves strength estimate
#' X_trend <- matrix(2 + 0.5 * t + sin(2 * pi * t / 2), nrow = 1)
#' fd_trend <- fdata(X_trend, argvals = t)
#' seasonal.strength(fd_trend, period = 2, detrend_method = "linear")
seasonal.strength <- function(fdataobj, period = NULL,
                              method = c("variance", "spectral", "wavelet"),
                              n_harmonics = 3,
                              detrend_method = c("none", "linear", "auto")) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("seasonal.strength not yet implemented for 2D functional data")
  }

  method <- match.arg(method)
  detrend_method <- match.arg(detrend_method)

  # Apply detrending if requested
  if (detrend_method != "none") {
    fdataobj <- detrend(fdataobj, method = detrend_method)
  }

  if (is.null(period)) {
    period <- estimate.period(fdataobj)$period
  }

  if (method == "variance") {
    result <- .Call("wrap__seasonal_strength_variance",
                    fdataobj$data, fdataobj$argvals,
                    as.double(period), as.integer(n_harmonics))
  } else if (method == "spectral") {
    result <- .Call("wrap__seasonal_strength_spectral",
                    fdataobj$data, fdataobj$argvals, as.double(period))
  } else {
    # wavelet method
    result <- .Call("wrap__seasonal_strength_wavelet",
                    fdataobj$data, fdataobj$argvals, as.double(period))
  }

  result
}

#' Time-Varying Seasonal Strength
#'
#' Computes seasonal strength at each time point using a sliding window,
#' allowing detection of how seasonality changes over time.
#'
#' @param fdataobj An fdata object.
#' @param period Known or estimated period.
#' @param window_size Width of the sliding window. Recommended: 2 * period.
#' @param method Method for computing strength: "variance" or "spectral".
#'
#' @return An fdata object containing the time-varying seasonal strength curve.
#'
#' @export
#' @examples
#' # Signal that transitions from seasonal to non-seasonal
#' t <- seq(0, 20, length.out = 400)
#' X <- ifelse(t < 10, sin(2 * pi * t / 2), rnorm(length(t[t >= 10]), sd = 0.5))
#' X <- matrix(X, nrow = 1)
#' fd <- fdata(X, argvals = t)
#'
#' # Compute time-varying strength
#' ss <- seasonal.strength.curve(fd, period = 2, window_size = 4)
#' # plot(ss)  # Shows strength declining around t = 10
seasonal.strength.curve <- function(fdataobj, period, window_size = NULL,
                                    method = c("variance", "spectral")) {
 if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("seasonal.strength.curve not yet implemented for 2D functional data")
  }

  method <- match.arg(method)

  if (is.null(window_size)) {
    window_size <- 2 * period
  }

  strength <- .Call("wrap__seasonal_strength_windowed",
                    fdataobj$data, fdataobj$argvals,
                    as.double(period), as.double(window_size), method)

  fdata(matrix(strength, nrow = 1), argvals = fdataobj$argvals,
        rangeval = fdataobj$rangeval)
}

# ==============================================================================
# Seasonality Change Detection
# ==============================================================================

#' Detect Changes in Seasonality
#'
#' Detects points in time where seasonality starts (onset) or stops (cessation)
#' by monitoring time-varying seasonal strength.
#'
#' @param fdataobj An fdata object.
#' @param period Known or estimated period.
#' @param threshold Seasonal strength threshold for classification (0-1).
#'   Above threshold = seasonal, below = non-seasonal. Default: 0.3.
#' @param window_size Width of sliding window for strength estimation.
#'   Default: 2 * period.
#' @param min_duration Minimum duration to confirm a change. Prevents
#'   detection of spurious short-term fluctuations. Default: period.
#'
#' @return A list with components:
#' \describe{
#'   \item{change_points}{Data frame with columns: time, type ("onset" or "cessation"),
#'     strength_before, strength_after}
#'   \item{strength_curve}{Time-varying seasonal strength used for detection}
#' }
#'
#' @export
#' @examples
#' # Signal that starts non-seasonal, becomes seasonal, then stops
#' t <- seq(0, 30, length.out = 600)
#' X <- ifelse(t < 10, rnorm(sum(t < 10), sd = 0.3),
#'             ifelse(t < 20, sin(2 * pi * t[t >= 10 & t < 20] / 2),
#'                    rnorm(sum(t >= 20), sd = 0.3)))
#' X <- matrix(X, nrow = 1)
#' fd <- fdata(X, argvals = t)
#'
#' # Detect changes
#' changes <- detect.seasonality.changes(fd, period = 2)
#' print(changes$change_points)  # Should show onset ~10, cessation ~20
detect.seasonality.changes <- function(fdataobj, period, threshold = 0.3,
                                       window_size = NULL, min_duration = NULL) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("detect.seasonality.changes not yet implemented for 2D functional data")
  }

  if (is.null(window_size)) {
    window_size <- 2 * period
  }

  if (is.null(min_duration)) {
    min_duration <- period
  }

  result <- .Call("wrap__seasonal_detect_changes",
                  fdataobj$data, fdataobj$argvals,
                  as.double(period), as.double(threshold),
                  as.double(window_size), as.double(min_duration))

  # Convert to data frame
  if (length(result$change_times) > 0) {
    result$change_points <- data.frame(
      time = result$change_times,
      type = result$change_types,
      strength_before = result$strength_before,
      strength_after = result$strength_after
    )
  } else {
    result$change_points <- data.frame(
      time = numeric(0),
      type = character(0),
      strength_before = numeric(0),
      strength_after = numeric(0)
    )
  }

  # Remove intermediate vectors
  result$change_times <- NULL
  result$change_types <- NULL
  result$strength_before <- NULL
  result$strength_after <- NULL

  class(result) <- "seasonality_changes"
  result
}

#' Print method for seasonality_changes objects
#' @param x A seasonality_changes object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.seasonality_changes <- function(x, ...) {
  cat("Seasonality Change Detection\n")
  cat("----------------------------\n")
  if (nrow(x$change_points) == 0) {
    cat("No changes detected.\n")
  } else {
    cat(sprintf("Number of changes: %d\n\n", nrow(x$change_points)))
    print(x$change_points)
  }
  invisible(x)
}

# ==============================================================================
# Instantaneous Period
# ==============================================================================

#' Estimate Instantaneous Period
#'
#' For signals with time-varying frequency (drifting period), estimates the
#' instantaneous period at each time point using the Hilbert transform.
#'
#' @param fdataobj An fdata object.
#'
#' @return A list with fdata objects:
#' \describe{
#'   \item{period}{Instantaneous period at each time point}
#'   \item{frequency}{Instantaneous frequency at each time point}
#'   \item{amplitude}{Instantaneous amplitude (envelope) at each time point}
#' }
#'
#' @details
#' The Hilbert transform is used to compute the analytic signal, from which
#' the instantaneous phase is extracted. The derivative of the phase gives
#' the instantaneous frequency, and 1/frequency gives the period.
#'
#' This is particularly useful for signals where the period is not constant,
#' such as circadian rhythms with drift or frequency-modulated signals.
#'
#' @export
#' @examples
#' # Chirp signal with increasing frequency
#' t <- seq(0, 10, length.out = 500)
#' freq <- 0.5 + 0.1 * t  # Frequency increases from 0.5 to 1.5
#' X <- matrix(sin(2 * pi * cumsum(freq) * diff(c(0, t))), nrow = 1)
#' fd <- fdata(X, argvals = t)
#'
#' # Estimate instantaneous period
#' inst <- instantaneous.period(fd)
#' # plot(inst$period)  # Shows decreasing period over time
instantaneous.period <- function(fdataobj) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("instantaneous.period not yet implemented for 2D functional data")
  }

  result <- .Call("wrap__seasonal_instantaneous_period",
                  fdataobj$data, fdataobj$argvals)

  # Convert to fdata objects
  list(
    period = fdata(matrix(result$period, nrow = 1),
                   argvals = fdataobj$argvals, rangeval = fdataobj$rangeval),
    frequency = fdata(matrix(result$frequency, nrow = 1),
                      argvals = fdataobj$argvals, rangeval = fdataobj$rangeval),
    amplitude = fdata(matrix(result$amplitude, nrow = 1),
                      argvals = fdataobj$argvals, rangeval = fdataobj$rangeval)
  )
}

# ==============================================================================
# Peak Timing Variability Analysis
# ==============================================================================

#' Analyze Peak Timing Variability
#'
#' For short series (e.g., 3-5 years of yearly data), this function detects
#' one peak per cycle and analyzes how peak timing varies between cycles.
#' Uses Fourier basis smoothing for peak detection.
#'
#' @param fdataobj An fdata object.
#' @param period Known period (e.g., 365 for daily data with yearly seasonality).
#' @param smooth_nbasis Number of Fourier basis functions for smoothing.
#'   If NULL, uses GCV for automatic selection (range 5-25). Default: NULL.
#'
#' @return A list with components:
#' \describe{
#'   \item{peak_times}{Vector of peak times}
#'   \item{peak_values}{Vector of peak values}
#'   \item{normalized_timing}{Position within cycle (0-1 scale)}
#'   \item{mean_timing}{Mean normalized timing}
#'   \item{std_timing}{Standard deviation of normalized timing}
#'   \item{range_timing}{Range of normalized timing (max - min)}
#'   \item{variability_score}{Variability score (0 = stable, 1 = highly variable)}
#'   \item{timing_trend}{Linear trend in timing (positive = peaks getting later)}
#'   \item{cycle_indices}{Cycle indices (1-indexed)}
#' }
#'
#' @details
#' The variability score is computed as std_timing / 0.1, capped at 1.
#' A score > 0.5 suggests peaks are shifting substantially between cycles.
#' The timing_trend indicates if peaks are systematically moving earlier
#' or later over time.
#'
#' Fourier basis smoothing is ideal for seasonal signals because it naturally
#' captures periodic patterns.
#'
#' @export
#' @examples
#' # 5 years of yearly data where peak shifts
#' t <- seq(0, 5, length.out = 365 * 5)
#' periods <- c(1, 1, 1, 1, 1)  # 5 complete years
#' # Peaks shift: March (0.2), April (0.3), May (0.4), April (0.3), March (0.2)
#' peak_phases <- c(0.2, 0.3, 0.4, 0.3, 0.2)
#' X <- sin(2 * pi * t + rep(peak_phases, each = 365))
#' fd <- fdata(matrix(X, nrow = 1), argvals = t)
#'
#' result <- analyze.peak.timing(fd, period = 1)
#' print(result$variability_score)  # Shows timing variability
analyze.peak.timing <- function(fdataobj, period, smooth_nbasis = NULL) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("analyze.peak.timing not yet implemented for 2D functional data")
  }

  nbasis_arg <- if (is.null(smooth_nbasis)) NULL else as.integer(smooth_nbasis)

  result <- .Call("wrap__seasonal_analyze_peak_timing",
                  fdataobj$data, fdataobj$argvals,
                  as.double(period), nbasis_arg)

  class(result) <- "peak_timing"
  result
}

#' Print method for peak_timing objects
#' @param x A peak_timing object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.peak_timing <- function(x, ...) {
  cat("Peak Timing Variability Analysis\n")
  cat("---------------------------------\n")
  cat(sprintf("Number of peaks: %d\n", length(x$peak_times)))
  cat(sprintf("Mean timing:     %.4f\n", x$mean_timing))
  cat(sprintf("Std timing:      %.4f\n", x$std_timing))
  cat(sprintf("Range timing:    %.4f\n", x$range_timing))
  cat(sprintf("Variability:     %.4f", x$variability_score))
  if (x$variability_score > 0.5) {
    cat(" (HIGH)\n")
  } else if (x$variability_score > 0.2) {
    cat(" (moderate)\n")
  } else {
    cat(" (low)\n")
  }
  cat(sprintf("Timing trend:    %.4f\n", x$timing_trend))
  invisible(x)
}

# ==============================================================================
# Seasonality Classification
# ==============================================================================

#' Classify Seasonality Type
#'
#' Classifies the type of seasonality in functional data. Particularly useful
#' for short series (3-5 years) to identify stable vs variable timing patterns.
#'
#' @param fdataobj An fdata object.
#' @param period Known seasonal period.
#' @param strength_threshold Threshold for seasonal/non-seasonal (default: 0.3).
#' @param timing_threshold Max std of normalized timing for "stable" (default: 0.05).
#'
#' @return A list with components:
#' \describe{
#'   \item{is_seasonal}{Logical: is the series seasonal overall?}
#'   \item{has_stable_timing}{Logical: is peak timing stable across cycles?}
#'   \item{timing_variability}{Timing variability score (0-1)}
#'   \item{seasonal_strength}{Overall seasonal strength}
#'   \item{cycle_strengths}{Per-cycle seasonal strength}
#'   \item{weak_seasons}{Indices of weak/missing seasons (0-indexed)}
#'   \item{classification}{One of: "StableSeasonal", "VariableTiming",
#'     "IntermittentSeasonal", "NonSeasonal"}
#'   \item{peak_timing}{Peak timing analysis (if peaks detected)}
#' }
#'
#' @details
#' Classification types:
#' \itemize{
#'   \item StableSeasonal: Regular peaks with consistent timing
#'   \item VariableTiming: Regular peaks but timing shifts between cycles
#'   \item IntermittentSeasonal: Some cycles seasonal, some not
#'   \item NonSeasonal: No clear seasonality
#' }
#'
#' @export
#' @examples
#' # Pure seasonal signal
#' t <- seq(0, 10, length.out = 500)
#' X <- matrix(sin(2 * pi * t / 2), nrow = 1)
#' fd <- fdata(X, argvals = t)
#'
#' result <- classify.seasonality(fd, period = 2)
#' print(result$classification)  # "StableSeasonal"
classify.seasonality <- function(fdataobj, period,
                                  strength_threshold = NULL,
                                  timing_threshold = NULL) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("classify.seasonality not yet implemented for 2D functional data")
  }

  str_thresh <- if (is.null(strength_threshold)) NULL else as.double(strength_threshold)
  tim_thresh <- if (is.null(timing_threshold)) NULL else as.double(timing_threshold)

  result <- .Call("wrap__seasonal_classify_seasonality",
                  fdataobj$data, fdataobj$argvals,
                  as.double(period), str_thresh, tim_thresh)

  class(result) <- "seasonality_classification"
  result
}

#' Print method for seasonality_classification objects
#' @param x A seasonality_classification object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.seasonality_classification <- function(x, ...) {
  cat("Seasonality Classification\n")
  cat("--------------------------\n")
  cat(sprintf("Classification:   %s\n", x$classification))
  cat(sprintf("Is seasonal:      %s\n", x$is_seasonal))
  cat(sprintf("Stable timing:    %s\n", x$has_stable_timing))
  cat(sprintf("Timing variability: %.4f\n", x$timing_variability))
  cat(sprintf("Seasonal strength:  %.4f\n", x$seasonal_strength))
  if (length(x$weak_seasons) > 0) {
    cat(sprintf("Weak seasons:     %s\n", paste(x$weak_seasons, collapse = ", ")))
  }
  invisible(x)
}

# ==============================================================================
# Automatic Threshold Detection
# ==============================================================================

#' Detect Seasonality Changes with Automatic Threshold
#'
#' Detects points where seasonality starts or stops, using automatic
#' threshold selection instead of a fixed value.
#'
#' @param fdataobj An fdata object.
#' @param period Known seasonal period.
#' @param threshold_method Method for threshold selection:
#' \describe{
#'   \item{"fixed"}{Use threshold_value as fixed threshold}
#'   \item{"percentile"}{Use threshold_value as percentile of strength distribution}
#'   \item{"otsu"}{Use Otsu's method for bimodal separation (default)}
#' }
#' @param threshold_value Value for "fixed" or "percentile" methods.
#' @param window_size Width of sliding window for strength estimation.
#'   Default: 2 * period.
#' @param min_duration Minimum duration to confirm a change.
#'   Default: period.
#'
#' @return A list with components:
#' \describe{
#'   \item{change_points}{Data frame with time, type, strength_before, strength_after}
#'   \item{strength_curve}{Time-varying seasonal strength used for detection}
#'   \item{computed_threshold}{The threshold that was computed/used}
#' }
#'
#' @details
#' Otsu's method automatically finds the optimal threshold for separating
#' seasonal from non-seasonal regions based on the strength distribution.
#' This is particularly useful when you don't know the appropriate threshold
#' for your data.
#'
#' @export
#' @examples
#' # Signal that transitions from seasonal to non-seasonal
#' t <- seq(0, 20, length.out = 400)
#' X <- ifelse(t < 10, sin(2 * pi * t / 2), rnorm(sum(t >= 10), sd = 0.3))
#' X <- matrix(X, nrow = 1)
#' fd <- fdata(X, argvals = t)
#'
#' # Detect changes with Otsu threshold
#' changes <- detect.seasonality.changes.auto(fd, period = 2)
#' print(changes$computed_threshold)
detect.seasonality.changes.auto <- function(fdataobj, period,
                                             threshold_method = "otsu",
                                             threshold_value = NULL,
                                             window_size = NULL,
                                             min_duration = NULL) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("detect.seasonality.changes.auto not yet implemented for 2D functional data")
  }

  if (is.null(window_size)) {
    window_size <- 2 * period
  }

  if (is.null(min_duration)) {
    min_duration <- period
  }

  if (is.null(threshold_value)) {
    threshold_value <- switch(threshold_method,
                               "fixed" = 0.3,
                               "percentile" = 20,
                               0.0)  # Not used for Otsu
  }

  result <- .Call("wrap__seasonal_detect_changes_auto",
                  fdataobj$data, fdataobj$argvals,
                  as.double(period), as.character(threshold_method),
                  as.double(threshold_value), as.double(window_size),
                  as.double(min_duration))

  # Convert to data frame
  if (length(result$change_times) > 0) {
    result$change_points <- data.frame(
      time = result$change_times,
      type = result$change_types,
      strength_before = result$strength_before,
      strength_after = result$strength_after
    )
  } else {
    result$change_points <- data.frame(
      time = numeric(0),
      type = character(0),
      strength_before = numeric(0),
      strength_after = numeric(0)
    )
  }

  # Remove intermediate vectors
  result$change_times <- NULL
  result$change_types <- NULL
  result$strength_before <- NULL
  result$strength_after <- NULL

  class(result) <- "seasonality_changes_auto"
  result
}

#' Print method for seasonality_changes_auto objects
#' @param x A seasonality_changes_auto object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.seasonality_changes_auto <- function(x, ...) {
  cat("Seasonality Change Detection (Auto Threshold)\n")
  cat("----------------------------------------------\n")
  cat(sprintf("Computed threshold: %.4f\n", x$computed_threshold))
  if (nrow(x$change_points) == 0) {
    cat("No changes detected.\n")
  } else {
    cat(sprintf("Number of changes: %d\n\n", nrow(x$change_points)))
    print(x$change_points)
  }
  invisible(x)
}

# ==============================================================================
# Detrending Functions
# ==============================================================================

#' Remove Trend from Functional Data
#'
#' Removes trend from functional data using various methods. This is useful
#' for preprocessing data before seasonal analysis when the data has a
#' significant trend component.
#'
#' @param fdataobj An fdata object.
#' @param method Detrending method:
#' \describe{
#'   \item{"linear"}{Least squares linear fit (default)}
#'   \item{"polynomial"}{Polynomial regression of specified degree}
#'   \item{"diff1"}{First-order differencing}
#'   \item{"diff2"}{Second-order differencing}
#'   \item{"loess"}{Local polynomial regression (LOESS)}
#'   \item{"auto"}{Automatic selection via AIC}
#' }
#' @param degree Polynomial degree for "polynomial" method. Default: 2.
#' @param bandwidth Bandwidth as fraction of data range for "loess" method.
#'   Default: 0.3.
#' @param return_trend Logical. If TRUE, return both trend and detrended data.
#'   Default: FALSE.
#'
#' @return If return_trend = FALSE, an fdata object with detrended data.
#'   If return_trend = TRUE, a list with components:
#' \describe{
#'   \item{detrended}{fdata object with detrended data}
#'   \item{trend}{fdata object with estimated trend}
#'   \item{method}{Method used for detrending}
#'   \item{rss}{Residual sum of squares per curve}
#' }
#'
#' @details
#' For series with polynomial trends, "linear" or "polynomial" methods are
#' appropriate. For more complex trends, "loess" provides flexibility.
#' The "auto" method compares linear, polynomial (degree 2 and 3), and LOESS,
#' selecting the method with lowest AIC.
#'
#' Differencing methods ("diff1", "diff2") reduce the series length by 1 or 2
#' points respectively. The resulting fdata has correspondingly shorter argvals.
#'
#' @seealso \code{\link{decompose}} for full seasonal decomposition
#'
#' @export
#' @examples
#' # Generate data with linear trend and seasonal component
#' t <- seq(0, 10, length.out = 200)
#' X <- matrix(2 + 0.5 * t + sin(2 * pi * t / 2), nrow = 1)
#' fd <- fdata(X, argvals = t)
#'
#' # Detrend with linear method
#' fd_detrended <- detrend(fd, method = "linear")
#'
#' # Now estimate period on detrended data
#' period <- estimate.period(fd_detrended)
#' print(period$period)  # Should be close to 2
#'
#' # Get both trend and detrended data
#' result <- detrend(fd, method = "linear", return_trend = TRUE)
#' # plot(result$trend)  # Shows the linear trend
detrend <- function(fdataobj,
                    method = c("linear", "polynomial", "diff1", "diff2", "loess", "auto"),
                    degree = 2,
                    bandwidth = 0.3,
                    return_trend = FALSE) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("detrend not yet implemented for 2D functional data")
  }

  method <- match.arg(method)

  result <- .Call("wrap__seasonal_detrend",
                  fdataobj$data, fdataobj$argvals,
                  as.character(method), as.integer(degree),
                  as.double(bandwidth))

  # Handle differencing methods which reduce series length
  if (method %in% c("diff1", "diff2")) {
    order <- if (method == "diff1") 1 else 2
    new_m <- ncol(fdataobj$data) - order
    new_argvals <- fdataobj$argvals[1:new_m]
    new_rangeval <- c(new_argvals[1], new_argvals[new_m])
  } else {
    new_argvals <- fdataobj$argvals
    new_rangeval <- fdataobj$rangeval
  }

  detrended_fd <- fdata(result$detrended, argvals = new_argvals,
                        rangeval = new_rangeval)

  if (return_trend) {
    trend_fd <- fdata(result$trend, argvals = new_argvals,
                      rangeval = new_rangeval)
    list(
      detrended = detrended_fd,
      trend = trend_fd,
      method = result$method,
      rss = result$rss
    )
  } else {
    detrended_fd
  }
}

#' Seasonal-Trend Decomposition
#'
#' Decomposes functional data into trend, seasonal, and remainder components.
#' Similar to STL (Seasonal-Trend decomposition using LOESS).
#'
#' @param fdataobj An fdata object.
#' @param period Seasonal period. If NULL, estimated automatically using FFT.
#' @param method Decomposition method:
#' \describe{
#'   \item{"additive"}{data = trend + seasonal + remainder (default)}
#'   \item{"multiplicative"}{data = trend * seasonal * remainder}
#' }
#' @param trend_method Method for trend extraction: "loess" or "spline".
#'   Default: "loess".
#' @param bandwidth Bandwidth for trend extraction (fraction of range).
#'   Default: 0.3.
#' @param n_harmonics Number of Fourier harmonics for seasonal component.
#'   Default: 3.
#'
#' @return A list with components:
#' \describe{
#'   \item{trend}{fdata object with trend component}
#'   \item{seasonal}{fdata object with seasonal component}
#'   \item{remainder}{fdata object with remainder/residual}
#'   \item{period}{Period used for decomposition}
#'   \item{method}{Decomposition method ("additive" or "multiplicative")}
#' }
#'
#' @details
#' For additive decomposition: data = trend + seasonal + remainder.
#' The trend is extracted using LOESS or spline smoothing, then the seasonal
#' component is estimated by fitting Fourier harmonics to the detrended data.
#'
#' For multiplicative decomposition: data = trend * seasonal * remainder.
#' This is achieved by log-transforming the data, applying additive
#' decomposition, and back-transforming. Use this when the seasonal amplitude
#' grows with the trend level.
#'
#' @seealso \code{\link{detrend}} for simple trend removal,
#'   \code{\link{seasonal.strength}} for measuring seasonality
#'
#' @export
#' @examples
#' # Additive seasonal pattern
#' t <- seq(0, 20, length.out = 400)
#' X <- matrix(2 + 0.3 * t + sin(2 * pi * t / 2.5), nrow = 1)
#' fd <- fdata(X, argvals = t)
#'
#' result <- decompose(fd, period = 2.5, method = "additive")
#' # plot(result$trend)      # Linear trend
#' # plot(result$seasonal)   # Sinusoidal seasonal
#' # plot(result$remainder)  # Residual noise
#'
#' # Multiplicative pattern (amplitude grows with level)
#' X_mult <- matrix((2 + 0.3 * t) * (1 + 0.3 * sin(2 * pi * t / 2.5)), nrow = 1)
#' fd_mult <- fdata(X_mult, argvals = t)
#'
#' result_mult <- decompose(fd_mult, period = 2.5, method = "multiplicative")
decompose <- function(fdataobj,
                      period = NULL,
                      method = c("additive", "multiplicative"),
                      trend_method = c("loess", "spline"),
                      bandwidth = 0.3,
                      n_harmonics = 3) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("decompose not yet implemented for 2D functional data")
  }

  method <- match.arg(method)
  trend_method <- match.arg(trend_method)

  # Estimate period if not provided
  if (is.null(period)) {
    period <- estimate.period(fdataobj)$period
  }

  result <- .Call("wrap__seasonal_decompose",
                  fdataobj$data, fdataobj$argvals,
                  as.double(period), as.character(method),
                  as.character(trend_method), as.double(bandwidth),
                  as.integer(n_harmonics))

  list(
    trend = fdata(result$trend, argvals = fdataobj$argvals,
                  rangeval = fdataobj$rangeval),
    seasonal = fdata(result$seasonal, argvals = fdataobj$argvals,
                     rangeval = fdataobj$rangeval),
    remainder = fdata(result$remainder, argvals = fdataobj$argvals,
                      rangeval = fdataobj$rangeval),
    period = result$period,
    method = result$method
  )
}

#' Print method for decomposition objects
#' @param x A decomposition object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.decomposition <- function(x, ...) {
  cat("Seasonal Decomposition\n")
  cat("----------------------\n")
  cat(sprintf("Method:  %s\n", x$method))
  cat(sprintf("Period:  %.4f\n", x$period))
  cat("\nComponents: trend, seasonal, remainder\n")
  invisible(x)
}

# ==============================================================================
# Amplitude Modulation Detection
# ==============================================================================

#' Detect Amplitude Modulation in Seasonal Time Series
#'
#' Detects whether the amplitude of a seasonal pattern changes over time
#' (amplitude modulation). Uses either Hilbert transform or wavelet analysis
#' to extract the time-varying amplitude envelope.
#'
#' @param fdataobj An fdata object.
#' @param period Seasonal period in argvals units.
#' @param method Method for amplitude extraction: "hilbert" (default) or "wavelet".
#' @param modulation_threshold Coefficient of variation threshold for detecting
#'   modulation. Default: 0.15 (i.e., CV > 15% indicates modulation).
#' @param seasonality_threshold Strength threshold for seasonality detection.
#'   Default: 0.3.
#'
#' @return A list with components:
#' \describe{
#'   \item{is_seasonal}{Logical, whether seasonality is detected}
#'   \item{seasonal_strength}{Overall seasonal strength (spectral method)}
#'   \item{has_modulation}{Logical, whether amplitude modulation is detected}
#'   \item{modulation_type}{Character: "stable", "emerging", "fading",
#'     "oscillating", or "non_seasonal"}
#'   \item{modulation_score}{Coefficient of variation of amplitude (0 = stable)}
#'   \item{amplitude_trend}{Trend in amplitude (-1 to 1): negative = fading,
#'     positive = emerging}
#'   \item{amplitude_curve}{Time-varying amplitude (fdata object)}
#'   \item{time_points}{Time points for the amplitude curve}
#' }
#'
#' @details
#' The function first checks if seasonality is present using the spectral
#' method (which is robust to amplitude modulation). If seasonality is
#' detected, it extracts the amplitude envelope and analyzes its variation.
#'
#' \strong{Hilbert method}: Uses Hilbert transform to compute the analytic
#' signal and extract instantaneous amplitude. Fast but assumes narrowband
#' signals.
#'
#' \strong{Wavelet method}: Uses Morlet wavelet transform at the target
#' period. More robust to noise and non-stationarity.
#'
#' \strong{Modulation types}:
#' \itemize{
#'   \item \code{stable}: Constant amplitude (modulation_score < threshold)
#'   \item \code{emerging}: Amplitude increases over time (amplitude_trend > 0.3)
#'   \item \code{fading}: Amplitude decreases over time (amplitude_trend < -0.3)
#'   \item \code{oscillating}: Amplitude varies but no clear trend
#'   \item \code{non_seasonal}: No seasonality detected
#' }
#'
#' @export
#' @examples
#' # Generate data with emerging seasonality
#' t <- seq(0, 1, length.out = 200)
#' amplitude <- 0.2 + 0.8 * t  # Amplitude grows over time
#' X <- matrix(amplitude * sin(2 * pi * t / 0.2), nrow = 1)
#' fd <- fdata(X, argvals = t)
#'
#' result <- detect_amplitude_modulation(fd, period = 0.2)
#' print(result$modulation_type)  # "emerging"
#' print(result$amplitude_trend)  # Positive value
#'
#' # Compare Hilbert vs Wavelet methods
#' result_hilbert <- detect_amplitude_modulation(fd, period = 0.2, method = "hilbert")
#' result_wavelet <- detect_amplitude_modulation(fd, period = 0.2, method = "wavelet")
detect_amplitude_modulation <- function(fdataobj, period,
                                         method = c("hilbert", "wavelet"),
                                         modulation_threshold = 0.15,
                                         seasonality_threshold = 0.3) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("detect_amplitude_modulation not yet implemented for 2D functional data")
  }

  method <- match.arg(method)

  if (method == "hilbert") {
    result <- .Call("wrap__seasonal_detect_amplitude_modulation",
                    fdataobj$data, fdataobj$argvals, period,
                    modulation_threshold, seasonality_threshold)

    # Create fdata for amplitude curve
    if (length(result$strength_curve) > 0) {
      amplitude_curve <- fdata(matrix(result$strength_curve, nrow = 1),
                                argvals = result$time_points,
                                rangeval = fdataobj$rangeval)
    } else {
      amplitude_curve <- NULL
    }

    result$amplitude_curve <- amplitude_curve
    result$strength_curve <- NULL  # Remove raw vector

  } else {
    # Wavelet method
    result <- .Call("wrap__seasonal_detect_amplitude_modulation_wavelet",
                    fdataobj$data, fdataobj$argvals, period,
                    modulation_threshold, seasonality_threshold)

    # Create fdata for wavelet amplitude
    if (length(result$wavelet_amplitude) > 0) {
      amplitude_curve <- fdata(matrix(result$wavelet_amplitude, nrow = 1),
                                argvals = result$time_points,
                                rangeval = fdataobj$rangeval)
    } else {
      amplitude_curve <- NULL
    }

    result$amplitude_curve <- amplitude_curve
    result$wavelet_amplitude <- NULL  # Remove raw vector
  }

  class(result) <- "amplitude_modulation"
  result
}

#' Print method for amplitude_modulation objects
#' @param x An amplitude_modulation object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.amplitude_modulation <- function(x, ...) {
  cat("Amplitude Modulation Detection\n")
  cat("------------------------------\n")
  cat(sprintf("Seasonal:        %s\n", if (x$is_seasonal) "Yes" else "No"))
  cat(sprintf("Strength:        %.4f\n", x$seasonal_strength))
  cat(sprintf("Has modulation:  %s\n", if (x$has_modulation) "Yes" else "No"))
  cat(sprintf("Modulation type: %s\n", x$modulation_type))
  cat(sprintf("Modulation score (CV): %.4f\n", x$modulation_score))
  cat(sprintf("Amplitude trend: %.4f\n", x$amplitude_trend))
  invisible(x)
}

#' Plot method for amplitude_modulation objects
#' @param x An amplitude_modulation object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
plot.amplitude_modulation <- function(x, ...) {
  if (is.null(x$amplitude_curve)) {
    message("No amplitude curve available to plot")
    return(invisible(x))
  }

  plot(x$amplitude_curve,
       main = sprintf("Amplitude Envelope (%s)", x$modulation_type),
       ylab = "Amplitude",
       ...)

  invisible(x)
}
