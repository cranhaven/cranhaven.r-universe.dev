#' Statistical Tests for Functional Data
#'
#' Functions for hypothesis testing with functional data.

#' Test for Functional Linear Model
#'
#' Tests the goodness-of-fit for a functional linear model using
#' the projected Cramer-von Mises statistic.
#'
#' @param fdataobj An object of class 'fdata' (functional covariate).
#' @param y Response vector.
#' @param B Number of bootstrap samples for p-value computation.
#' @param ... Additional arguments.
#'
#' @return A list of class 'htest' with components:
#' \describe{
#'   \item{statistic}{The test statistic}
#'   \item{p.value}{Bootstrap p-value}
#'   \item{method}{Name of the test}
#' }
#'
#' @export
#' @examples
#' fd <- fdata(matrix(rnorm(200), 20, 10))
#' y <- rnorm(20)
#' \donttest{
#' test_result <- flm.test(fd, y, B = 100)
#' test_result$p.value
#' }
flm.test <- function(fdataobj, y, B = 500, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  n <- nrow(fdataobj$data)
  if (length(y) != n) {
    stop("Length of y must equal number of curves")
  }

  # Fit functional PC regression
  fit <- fregre.pc(fdataobj, y, ...)
  residuals <- fit$residuals

  # Compute inner product matrix for Adot
  X <- fdataobj$data
  X_centered <- scale(X, center = TRUE, scale = FALSE)

  # Lower triangle of X'X (including diagonal)
  m <- ncol(X)
  inprod <- numeric((n * n + n) / 2)
  idx <- 1
  for (j in 1:n) {
    for (i in j:n) {
      inprod[idx] <- sum(X_centered[i, ] * X_centered[j, ]) / m
      idx <- idx + 1
    }
  }

  # Compute Adot matrix
  adot_vec <- .Call("wrap__compute_adot", as.integer(n), as.numeric(inprod))

  # Compute test statistic
  stat_obs <- .Call("wrap__pcvm_statistic", as.numeric(adot_vec), as.numeric(residuals))

  # Bootstrap for p-value
  boot_stats <- numeric(B)
  for (b in seq_len(B)) {
    # Permute residuals
    perm_residuals <- sample(residuals)
    boot_stats[b] <- .Call("wrap__pcvm_statistic", as.numeric(adot_vec), as.numeric(perm_residuals))
  }

  p_value <- mean(boot_stats >= stat_obs)

  structure(
    list(
      statistic = stat_obs,
      p.value = p_value,
      boot.stats = boot_stats,
      method = "Projected Cramer-von Mises test for FLM"
    ),
    class = "htest"
  )
}

#' Test for Equality of Functional Means
#'
#' Tests whether the mean function equals a specified value.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param mu0 Hypothesized mean function (vector). If NULL, tests against zero.
#' @param B Number of bootstrap samples for p-value computation.
#' @param ... Additional arguments.
#'
#' @return A list of class 'htest' with components:
#' \describe{
#'   \item{statistic}{The test statistic}
#'   \item{p.value}{Bootstrap p-value}
#'   \item{method}{Name of the test}
#' }
#'
#' @export
#' @examples
#' fd <- fdata(matrix(rnorm(200), 20, 10))
#' \donttest{
#' test_result <- fmean.test.fdata(fd, B = 100)
#' test_result$p.value
#' }
fmean.test.fdata <- function(fdataobj, mu0 = NULL, B = 500, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  n <- nrow(fdataobj$data)
  m <- ncol(fdataobj$data)

  # Default null hypothesis: mean = 0
  if (is.null(mu0)) {
    mu0 <- rep(0, m)
  }

  if (length(mu0) != m) {
    stop("mu0 must have the same length as number of evaluation points")
  }

  # Center data around hypothesized mean
  X_centered <- sweep(fdataobj$data, 2, mu0)

  # Compute sample mean
  X_mean <- colMeans(X_centered)

  # Test statistic: L2 norm of sample mean
  stat_obs <- sqrt(sum(X_mean^2) / m) * sqrt(n)

  # Bootstrap for p-value (resample and compute statistic)
  boot_stats <- numeric(B)
  for (b in seq_len(B)) {
    # Resample indices
    idx <- sample(n, n, replace = TRUE)
    X_boot <- X_centered[idx, ]
    X_mean_boot <- colMeans(X_boot)
    boot_stats[b] <- sqrt(sum(X_mean_boot^2) / m) * sqrt(n)
  }

  # Two-sided p-value
  p_value <- mean(abs(boot_stats) >= abs(stat_obs))

  structure(
    list(
      statistic = stat_obs,
      p.value = p_value,
      boot.stats = boot_stats,
      method = "Test for functional mean"
    ),
    class = "htest"
  )
}

# Internal: supremum (L-infinity) norm for a numeric vector
.sup_norm <- function(x) max(abs(x))

#' Functional Equivalence Test (TOST)
#'
#' Tests whether two functional means are equivalent within an equivalence
#' margin \code{delta}, based on the supremum norm. Uses the approach of
#' Dette & Kokot (2021) with a simultaneous confidence band.
#'
#' @section Hypotheses:
#' \describe{
#'   \item{H0}{sup_t |mu1(t) - mu2(t)| >= delta (NOT equivalent)}
#'   \item{H1}{sup_t |mu1(t) - mu2(t)| < delta (equivalent)}
#' }
#'
#' Equivalence is declared when the entire (1-2*alpha) simultaneous confidence
#' band for the mean difference lies within \code{[-delta, delta]}.
#'
#' @param fdataobj1 An object of class \code{fdata} (first sample).
#' @param fdataobj2 An object of class \code{fdata} (second sample), or NULL
#'   for a one-sample test.
#' @param delta Equivalence margin (positive scalar). The test checks whether
#'   the sup-norm of the mean difference is less than \code{delta}.
#' @param mu0 Hypothesized mean function for one-sample test. A numeric vector,
#'   a single-row \code{fdata}, or NULL (defaults to zero).
#' @param n.boot Number of bootstrap replicates (default 1000).
#' @param alpha Significance level (default 0.05). The SCB has coverage
#'   1-2*alpha.
#' @param method Bootstrap method: \code{"multiplier"} (Gaussian multiplier,
#'   default) or \code{"percentile"} (resampling).
#' @param seed Optional random seed for reproducibility.
#'
#' @return An object of class \code{fequiv.test} with components:
#' \describe{
#'   \item{statistic}{Observed sup-norm of the mean difference}
#'   \item{delta}{Equivalence margin used}
#'   \item{critical.value}{Bootstrap critical value for the SCB}
#'   \item{scb.lower}{Lower bound of simultaneous confidence band}
#'   \item{scb.upper}{Upper bound of simultaneous confidence band}
#'   \item{diff.mean}{Estimated mean difference curve (numeric vector)}
#'   \item{reject}{Logical; TRUE if equivalence is declared}
#'   \item{p.value}{Bootstrap p-value}
#'   \item{alpha}{Significance level used}
#'   \item{method}{Bootstrap method used}
#'   \item{argvals}{Argument values (time grid)}
#'   \item{n1}{Sample size of first group}
#'   \item{n2}{Sample size of second group (NA for one-sample)}
#'   \item{boot.stats}{Vector of bootstrap sup-norm statistics}
#'   \item{data.name}{Description of input data}
#' }
#'
#' @references
#' Dette, H. and Kokot, K. (2021). Detecting relevant differences in the
#' covariance operators of functional time series. \emph{Biometrika},
#' 108(4):895--913.
#'
#' @export
#' @examples
#' set.seed(42)
#' t_grid <- seq(0, 1, length.out = 50)
#' X1 <- fdata(matrix(rnorm(30 * 50), 30, 50), argvals = t_grid)
#' X2 <- fdata(matrix(rnorm(25 * 50, mean = 0.1), 25, 50), argvals = t_grid)
#' \donttest{
#' result <- fequiv.test(X1, X2, delta = 1, n.boot = 500)
#' print(result)
#' plot(result)
#' }
fequiv.test <- function(fdataobj1, fdataobj2 = NULL, delta, mu0 = NULL,
                        n.boot = 1000, alpha = 0.05,
                        method = c("multiplier", "percentile"), seed = NULL) {
  # --- Input validation ---
  if (!inherits(fdataobj1, "fdata")) {
    stop("fdataobj1 must be of class 'fdata'")
  }
  if (missing(delta) || !is.numeric(delta) || length(delta) != 1 || delta <= 0) {
    stop("delta must be a positive scalar")
  }
  if (alpha <= 0 || alpha >= 0.5) {
    stop("alpha must be in (0, 0.5)")
  }
  method <- match.arg(method)

  if (!is.null(seed)) set.seed(seed)

  n1 <- nrow(fdataobj1$data)
  m <- ncol(fdataobj1$data)
  argvals <- fdataobj1$argvals

  # --- Two-sample vs one-sample ---
  two_sample <- !is.null(fdataobj2)

  if (two_sample) {
    if (!inherits(fdataobj2, "fdata")) {
      stop("fdataobj2 must be of class 'fdata'")
    }
    if (ncol(fdataobj2$data) != m) {
      stop("fdataobj1 and fdataobj2 must have the same number of evaluation points")
    }
    n2 <- nrow(fdataobj2$data)
    data_name <- paste(deparse(substitute(fdataobj1)), "and",
                       deparse(substitute(fdataobj2)))

    # Mean difference
    mean1 <- colMeans(fdataobj1$data)
    mean2 <- colMeans(fdataobj2$data)
    d_hat <- mean1 - mean2

    # Center residuals within each group
    centered1 <- sweep(fdataobj1$data, 2, mean1)
    centered2 <- sweep(fdataobj2$data, 2, mean2)

  } else {
    # One-sample test
    n2 <- NA
    data_name <- deparse(substitute(fdataobj1))

    if (is.null(mu0)) {
      mu0 <- rep(0, m)
    } else if (inherits(mu0, "fdata")) {
      mu0 <- as.numeric(mu0$data[1, ])
    }
    if (length(mu0) != m) {
      stop("mu0 must have the same length as number of evaluation points")
    }

    mean1 <- colMeans(fdataobj1$data)
    d_hat <- mean1 - mu0

    # Center residuals
    centered1 <- sweep(fdataobj1$data, 2, mean1)
  }

  # Observed test statistic
  T_obs <- .sup_norm(d_hat)

  # --- Bootstrap ---
  boot_stats <- numeric(n.boot)

  if (method == "multiplier") {
    # Gaussian multiplier bootstrap
    for (b in seq_len(n.boot)) {
      g1 <- rnorm(n1)
      z_boot <- colMeans(centered1 * g1) / sqrt(n1)
      if (two_sample) {
        g2 <- rnorm(n2)
        z_boot <- z_boot - colMeans(centered2 * g2) / sqrt(n2)
      }
      boot_stats[b] <- .sup_norm(z_boot)
    }
    # SCB half-width scaling
    if (two_sample) {
      scale_factor <- sqrt(1 / n1 + 1 / n2)
    } else {
      scale_factor <- 1 / sqrt(n1)
    }
  } else {
    # Percentile bootstrap
    for (b in seq_len(n.boot)) {
      idx1 <- sample(n1, n1, replace = TRUE)
      boot_mean1 <- colMeans(fdataobj1$data[idx1, , drop = FALSE])
      if (two_sample) {
        idx2 <- sample(n2, n2, replace = TRUE)
        boot_mean2 <- colMeans(fdataobj2$data[idx2, , drop = FALSE])
        boot_diff <- boot_mean1 - boot_mean2
      } else {
        boot_diff <- boot_mean1 - mu0
      }
      boot_stats[b] <- .sup_norm(boot_diff - d_hat)
    }
    scale_factor <- 1
  }

  # Critical value: (1 - 2*alpha) quantile of bootstrap sup-norms
  c_alpha <- as.numeric(quantile(boot_stats, 1 - 2 * alpha))

  # Simultaneous confidence band
  if (method == "multiplier") {
    half_width <- c_alpha * scale_factor
  } else {
    half_width <- c_alpha
  }
  scb_lower <- d_hat - half_width
  scb_upper <- d_hat + half_width

  # Decision: reject H0 (declare equivalence) if SCB inside [-delta, delta]
  reject <- (max(scb_upper) < delta) && (min(scb_lower) > -delta)

  # P-value: proportion of bootstrap stats that would lead to non-rejection
  if (method == "multiplier") {
    p_value <- mean(boot_stats * scale_factor >= delta - T_obs)
  } else {
    p_value <- mean(boot_stats >= delta - T_obs)
  }
  p_value <- min(max(p_value, 0), 1)

  result <- list(
    statistic = T_obs,
    delta = delta,
    critical.value = half_width,
    scb.lower = as.numeric(scb_lower),
    scb.upper = as.numeric(scb_upper),
    diff.mean = as.numeric(d_hat),
    reject = reject,
    p.value = p_value,
    alpha = alpha,
    method = method,
    argvals = as.numeric(argvals),
    n1 = n1,
    n2 = n2,
    boot.stats = boot_stats,
    data.name = data_name
  )
  class(result) <- "fequiv.test"
  result
}

#' Print method for fequiv.test
#' @param x An \code{fequiv.test} object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.fequiv.test <- function(x, ...) {
  cat("Functional Equivalence Test (TOST)\n")
  cat("===================================\n")
  cat("Data:", x$data.name, "\n")
  if (is.na(x$n2)) {
    cat("One-sample test, n =", x$n1, "\n")
  } else {
    cat("Two-sample test, n1 =", x$n1, ", n2 =", x$n2, "\n")
  }
  cat("Bootstrap method:", x$method, "\n")
  cat("Equivalence margin (delta):", x$delta, "\n")
  cat("Significance level (alpha):", x$alpha, "\n")
  cat("---\n")
  cat("Test statistic (sup|d_hat|):", round(x$statistic, 4), "\n")
  cat("Critical value:", round(x$critical.value, 4), "\n")
  cat("SCB range: [", round(min(x$scb.lower), 4), ",",
      round(max(x$scb.upper), 4), "]\n")
  cat("P-value:", format.pval(x$p.value, digits = 3), "\n")
  cat("---\n")
  if (x$reject) {
    cat("Decision: Reject H0 -- equivalence declared\n")
  } else {
    cat("Decision: Fail to reject H0 -- equivalence NOT declared\n")
  }
  invisible(x)
}

#' Plot method for fequiv.test
#'
#' Creates a ggplot showing the mean difference curve, simultaneous confidence
#' band, and equivalence margins.
#'
#' @param x An \code{fequiv.test} object.
#' @param ... Additional arguments (ignored).
#' @return A ggplot object (invisibly).
#' @export
plot.fequiv.test <- function(x, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot.fequiv.test")
  }

  band_color <- if (x$reject) "#2ca02c" else "#d62728"

  df <- data.frame(
    t = x$argvals,
    diff = x$diff.mean,
    lower = x$scb.lower,
    upper = x$scb.upper
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$t)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
      fill = band_color, alpha = 0.3
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$diff), linewidth = 0.8
    ) +
    ggplot2::geom_hline(yintercept = c(-x$delta, x$delta),
                        linetype = "dashed", color = "gray40") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted",
                        color = "gray60") +
    ggplot2::labs(
      title = "Functional Equivalence Test",
      subtitle = if (x$reject) "Equivalence declared" else
        "Equivalence NOT declared",
      x = "t",
      y = expression(hat(mu)[1](t) - hat(mu)[2](t))
    ) +
    ggplot2::theme_minimal()

  print(p)
  invisible(p)
}
