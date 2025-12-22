#' Get estimates of time-dependent properties of models.
#'
#' \code{get_stats} estimates time-dependent properties of models
#' (e.g., variance) from ensemble time series.
#'
#' Any missing values in 'x' will cause an error.
#'
#' Bandwidths affect weights in local smoothers as follows. To get the
#' local estimate corresponding to index i, the distance to each other
#' index j is calculated as (i - j) / h, where h is the
#' bandwidth. Then that distance is plugged into the kernel function
#' to obtain a weight. The weights are normalized to sum to one for
#' each index.
#'
#' The gaussian kernel is equivalent to a standard Gaussian density
#' function. The uniform kernel is an indicator function of whether
#' the distance is less than 1. Thus selecting a uniform kernel with a
#' bandwidth of 2 is equivalent to a sliding window of length 3 that
#' is centered on the focal index. In general, if n is the greatest
#' integer that is less than the value of the bandwidth h, the window
#' includes the n nearest values on each side of the focal index.
#'
#' '"local_constant"' smoothers are local means computed with the
#' kernel weights. '"local_linear"' smoothers are the fitted values of
#' local linear regressions with the kernel weights. The linear
#' smoothers avoid biases that the one-sided kernels at the ends of
#' the time series can create for the local constant smoothers.
#'
#' See the vignette "Getting Started with spaero" for the formulas
#' used for each estimate.
#'
#' @param x A univariate or multivariate numeric time series object or
#'     a numeric vector or matrix.
#' @param center_trend Character string giving method of calculating
#'     the trend to subtract. Allowed values are '"assume_zero"',
#'     '"grand_mean"', '"ensemble_means"', '"local_constant"', and
#'     '"local_linear"'. Will be partially matched.
#' @param center_kernel Character string giving the kernel for any
#'     local detrending. Allowed values are '"gaussian"' and
#'     '"uniform"'.
#' @param center_bandwidth Bandwidth of kernel for any local detrending
#'     done. A numeric value >= 1.
#' @param stat_trend Character string giving method of smoothing
#'     estimates. Allowed values are '"local_constant"', and
#'     '"local_linear"'. Will be partially matched.
#' @param stat_kernel Character string giving the kernel for local
#'     smoothing of estimates. Allowed values are '"gaussian"' and
#'     '"uniform"'.
#' @param stat_bandwidth Bandwidth of kernel for local smoothing of
#'     estimates.  A numeric value >= 1.
#' @param lag Integer lag at which to calculate the acf. This lag is
#'     in terms of the index of \code{x} and does not account for the
#'     frequency of \code{x} if \code{x} is a time series. It should
#'     be non-negative.
#' @param backward_only Logical value (defaulting to 'FALSE') that
#'     determines whether any uniform smoothing kernels are restricted
#'     to using data before the index of the smoothed estimate.
#' @return A list with elements '"stats"', '"taus"', '"centered"',
#'     '"stat_trend"', '"stat_kernel"', '"stat_bandwidth"', and
#'     '"lag"'. "stats" is a list containing vectors of the
#'     estimates. '"taus"' is a list containing Kendall's correlation
#'     coefficient of each element of '"stats"' with
#'     time. '"centered"' is a list of the detrended time series, the
#'     trend subtracted, and the bandwidth used in the detrending. The
#'     other elements record the parameters provided to this function
#'     for future reference.
#'
#' @seealso \code{\link{acf}}, \code{\link{var}},
#' \code{\link[moments]{kurtosis}}, and
#' \code{\link[moments]{skewness}} for estimation of properties that
#' are not time-dependent. See
#' \code{\link[earlywarnings]{generic_ews}} for another approach to
#' estimation of time-dependent properties.
#' @export
#' @examples
#'
#' # A highly autocorrelated time series
#' x <- 1:10
#' get_stats(x, stat_bandwidth = 3)$stats
#'
#' # Plot log of acf
#' plot(log(get_stats(x, stat_bandwidth = 3)$stats$autocor))
#'
#' # Check estimates with AR1 simulations with lag-1 core 0.1
#' w <- rnorm(1000)
#' xnext <- function(xlast, w) 0.1 * xlast + w
#' x <- Reduce(xnext, x = w, init = 0, accumulate = TRUE)
#' acf(x, lag.max = 1, plot = FALSE)
#' head(get_stats(x, stat_bandwidth = length(x))$stats$autocor)
#'
#' # Check detrending ability
#' x2 <- x + seq(1, 10, len = length(x))
#' ans <- get_stats(x2, center_trend = "local_linear",
#'                   center_bandwidth = length(x),
#'                    stat_bandwidth = length(x))$stats
#' head(ans$autocor)
#'
#' # The simple acf estimate is inflated by the trend
#' acf(x2, lag.max = 1, plot = FALSE)
#'
#'# Check ability to estimate time-dependent autocorrelation
#' xnext <- function(xlast, w) 0.8 * xlast + w
#' xhi <- Reduce(xnext, x = w, init = 0, accumulate = TRUE)
#' acf(xhi, lag.max = 1, plot = FALSE)
#' wt <- seq(0, 1, len = length(x))
#' xdynamic <- wt * xhi + (1 - wt) * x
#' get_stats(xdynamic, stat_bandwidth = 100)$stats$autocor
get_stats <- function(x, center_trend = "grand_mean",
                      center_kernel = c("gaussian", "uniform"),
                      center_bandwidth = NULL,
                      stat_trend = c("local_constant", "local_linear"),
                      stat_kernel = c("uniform", "gaussian"),
                      stat_bandwidth = NULL, lag = 1, backward_only = FALSE){
  center_kernel <- match.arg(center_kernel)
  stat_kernel <- match.arg(stat_kernel)
  stat_trend <- match.arg(stat_trend)
  centered <- detrend(x, trend = center_trend, kernel = center_kernel,
                      bandwidth = center_bandwidth,
                      backward_only = backward_only)
  stats <- list()
  stats$variance <- get_noncentral_moments(centered$x, moment_number = 2,
                                           est = stat_trend,
                                           kernel = stat_kernel,
                                           bandwidth = stat_bandwidth,
                                           backward_only = backward_only)
  stats$variance <- stats$variance$smooth
  stats$variance[stats$variance < 0] <- 0
  stats$variance_first_diff <- c(NA, diff(stats$variance))
  stats$autocovariance <- autocor(centered$x, cortype = "covariance", lag = lag,
                                  est = stat_trend, kernel = stat_kernel,
                                  bandwidth = stat_bandwidth,
                                  backward_only = backward_only)
  stats$autocovariance <- stats$autocovariance$smooth
  if (lag > 0) {
      denom <- stats$variance[-seq_len(lag)]
      desel <- seq(length(stats$variance) - lag + 1, length(stats$variance))
      denom <- sqrt(denom * stats$variance[-desel])
      denom <- c(rep(NA, lag), denom)
  } else {
      denom <- stats$variance
  }
  stats$autocorrelation <- stats$autocovariance / denom
  ac01 <- ifelse(0 > stats$autocorrelation, 0, stats$autocorrelation)
  ac01 <- ifelse(1 > ac01, ac01, 1)
  denom <- log(ac01)
  stats$decay_time <- -lag / denom
  stats$mean <- centered$center
  stats$index_of_dispersion <- stats$variance / stats$mean
  stats$coefficient_of_variation <- sqrt(stats$variance) / stats$mean
  stats$skewness <- get_noncentral_moments(centered$x, moment_number = 3,
                                           est = stat_trend,
                                           kernel = stat_kernel,
                                           bandwidth = stat_bandwidth,
                                           backward_only = backward_only)
  stats$skewness <- stats$skewness$smooth / stats$variance ^ (3 / 2)
  stats$kurtosis <- get_noncentral_moments(centered$x, moment_number = 4,
                                           est = stat_trend,
                                           kernel = stat_kernel,
                                           bandwidth = stat_bandwidth,
                                           backward_only = backward_only)
  stats$kurtosis <- stats$kurtosis$smooth / stats$variance ^ 2
  stats$kurtosis[stats$kurtosis < 0] <- 0

  taus <- lapply(stats, get_tau)
  ret <- list(stats = stats, taus = taus, centered = centered,
              stat_trend = stat_trend, stat_kernel = stat_kernel,
              stat_bandwidth = stat_bandwidth, lag = lag)
  ret
}

get_tau <- function(x){
  if (any(is.finite(x))){
    stats::cor(x = seq_along(x), y = x, method = "kendall",
               use = "complete.obs")
  } else {
    NA
  }
}

detrend <- function(x, trend = c("grand_mean", "ensemble_means",
                                 "local_constant", "local_linear",
                               "assume_zero"), bandwidth = NULL, ...){
  trend <- match.arg(trend)
  x <- stats::na.fail(x)
  x <- as.matrix(x)
  if (!is.numeric(x)) stop("'x' must be numeric")
  rmn <- rowMeans(x)
  if (trend == "grand_mean"){
    center <- mean(rmn)
    x <- x - center
  } else if (trend == "ensemble_means"){
    center <- rmn
    x <- x - center
  } else if (trend == "local_constant"
             || trend == "local_linear"){
    samplet <- as.integer(nrow(x))
    step <- seq(1, samplet)
    data <- data.frame(step = step, rmn = rmn)
    srm <- smooth(data = data, bandwidth = bandwidth, est = trend, ...)
    center <- srm$smooth
    x <- x - center
    bandwidth <- srm$bandwidth
  } else if (trend == "assume_zero"){
    center <- rep(0, nrow(x))
  }
  list(x = x, center = center, bandwidth = bandwidth)
}

get_wls_coefs <- function(y, x, w){
    swx <- sum(w * x)
    swy <- sum(w * y)
    sw <- sum(w)
    b <- (sum(w * x * y) - swx * swy / sw) / (sum(w * x ^ 2) - swx ^ 2 / sw )
    a <- (sum(w * y) - b * sum(w * x)) / sw
    c(intercept = a, slope = b)
}

smooth <- function(data, est, kernel = "gaussian", bandwidth,
                   backward_only = FALSE){
  if (!is.numeric(bandwidth) || length(bandwidth) != 1) {
    stop("argument \"bandwidth\" must be provided as a single numeric value")
  } else if (bandwidth < 1){
    stop("argument \"bandwidth\" must be >= 1")
  }
  if (kernel == "gaussian") {
    kern <- function(ind, bw = bandwidth){
      dist <- abs(data$step - ind) / bw
      w <- stats::dnorm(dist)
      w / sum(w)
    }
  } else {
    kern <- function(ind, bw = bandwidth){
      dist <- (data$step - ind) / bw
      if (backward_only) {
          w <- dist <= 0 & dist > -1
      } else {
          w <- dist < 1 & dist > -1
      }
      w / sum(w)
    }
  }
  w <- sapply(data$step, kern)
  if (est == "local_constant"){
    smooth <- colSums(w * data$rmn)
  } else {
    wlm <- function(x){
      coefs <- get_wls_coefs(y = data$rmn, x = data$step, w = w[, x])
      fitted <- data$step * coefs["slope"] + coefs["intercept"]
      fitted[x]
    }
    smooth <- sapply(seq_along(data$step), wlm)
  }
  list(smooth = smooth, bandwidth = bandwidth)
}

autocor <- function(x, cortype = c("correlation", "covariance"), lag = 1, ...){
  cortype <- match.arg(cortype)
  x <- stats::na.fail(x)
  x <- as.matrix(x)
  if (!is.numeric(x)) stop("'x' must be numeric")
  if (lag < 0) stop("'lag' must be >= 0")

  n <- nrow(x)
  end1 <- n - lag
  start2 <- 1 + lag
  x1 <- x[1:end1, , drop = FALSE]
  x2 <- x[start2:n, , drop = FALSE]
  xx_lag <- rowMeans(x1 * x2)

  step <- seq(start2, n)
  data <- data.frame(step = step, rmn = xx_lag)
  xx_lag_sm <- smooth(data = data, ...)
  if (cortype == "correlation") {
    data <- data.frame(step = step, rmn = rowMeans(x1 * x1))
    xx1_sm <- smooth(data = data, ...)
    data <- data.frame(step = step, rmn = rowMeans(x2 * x2))
    xx2_sm <- smooth(data = data, ...)
    denom <- sqrt(xx1_sm$smooth * xx2_sm$smooth)
    ret <- list(smooth = xx_lag_sm$smooth / denom,
                bandwidth = xx1_sm$bandwidth)
  } else {
    ret <- xx_lag_sm
  }
  ret$smooth <- c(rep(NA, lag), ret$smooth)
  ret
}

get_noncentral_moments <- function(x, moment_number = 3, ...) {
  x <- stats::na.fail(x)
  x <- as.matrix(x)
  if (!is.numeric(x)) stop("'x' must be numeric")
  if (moment_number < 1) stop("'moment_number' must be >= 1")

  xpow <- rowMeans(x ^ moment_number)
  step <- seq_along(xpow)
  data <- data.frame(step = step, rmn = xpow)
  smooth(data = data, ...)
}
