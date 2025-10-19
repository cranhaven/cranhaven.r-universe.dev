#' @title Bland-Altman statistics with confidence intervals
#'
#' @description
#' Computes Bland-Altman mean difference and limits of agreement (LoA)
#' between two numeric measurement vectors, including t-based confidence
#' intervals for the mean difference and for each LoA using 'C++' backend.
#'
#' Note: Lin's concordance correlation coefficient (CCC) is a complementary,
#' single-number summary of agreement (precision + accuracy). It is useful for
#' quick screening or reporting an overall CI, but may miss systematic or
#' magnitude-dependent bias; consider reporting CCC alongside Bland-Altman.
#'
#' @details
#' Given paired measurements \eqn{(x_i, y_i)}, Bland-Altman analysis uses
#' \eqn{d_i = x_i - y_i} (or \eqn{y_i - x_i} if \code{mode = 2}) and
#' \eqn{m_i = (x_i + y_i)/2}. The mean difference \eqn{\bar d} estimates bias.
#' The limits of agreement (LoA) are \eqn{\bar d \pm z \cdot s_d}, where
#' \eqn{s_d} is the sample standard deviation of \eqn{d_i} and \eqn{z}
#' (argument \code{two}) is typically 1.96 for nominal 95% LoA.
#'
#' Confidence intervals use Student's \eqn{t} distribution with \eqn{n-1}
#' degrees of freedom, with
#' \itemize{
#'   \item Mean-difference CI given by \eqn{\bar d \pm t_{n-1,\,1-\alpha/2}\,
#'   s_d/\sqrt{n}}; and
#'   \item LoA CI given by \eqn{(\bar d \pm z\, s_d) \;\pm\;
#'   t_{n-1,\,1-\alpha/2}\, s_d\,\sqrt{3/n}}.
#' }
#'
#' Assumptions include approximately normal differences and roughly constant
#' variability across the measurement range; if differences increase with
#' magnitude, consider a transformation before analysis. Missing values are
#' removed pairwise (any row with an \code{NA} in either input is dropped).
#'
#' @param group1,group2 Numeric vectors of equal length.
#' @param two Positive scalar; the multiple of SD for the LoA (default 1.96).
#' @param mode Integer; 1 uses \code{group1 - group2}, 2 uses \code{group2 - group1}.
#' @param conf_level Confidence level for CIs (default 0.95).
#' @param verbose Logical; if TRUE, prints how many OpenMP threads are used.
#'
#' @return An object of class \code{"ba"} (list) with elements:
#' \itemize{
#'   \item \code{means}, \code{diffs}: numeric vectors
#'   \item \code{groups}: data.frame used after NA removal
#'   \item \code{based.on}: integer, number of pairs used
#'   \item \code{lower.limit}, \code{mean.diffs}, \code{upper.limit}
#'   \item \code{lines}: named numeric vector (lower, mean, upper)
#'   \item \code{CI.lines}: named numeric vector for CIs of those lines
#'   \item \code{two}, \code{critical.diff}
#' }
#'
#' @seealso \code{\link{print.ba}}, \code{\link{plot.ba}},
#'  \code{\link{ccc}},\code{\link{ccc_pairwise_u_stat}},
#'  \code{\link{ccc_lmm_reml}}
#'
#' @examples
#' set.seed(1)
#' x <- rnorm(100, 100, 10)
#' y <- x + rnorm(100, 0, 8)
#' ba <- bland_altman(x, y)
#' print(ba)
#' plot(ba)
#'
#' @references
#' Bland JM, Altman DG (1986). Statistical methods for assessing agreement
#' between two methods of clinical measurement. *The Lancet*, 307-310.
#' @references
#' Bland JM, Altman DG (1999). Measuring agreement in method comparison studies.
#' *Statistical Methods in Medical Research*, 8(2), 135-160.
#'
#' @author Thiago de Paula Oliveira
#'
#' @export
bland_altman <- function(group1,
                         group2,
                         two = 1.96,
                         mode = 1L,
                         conf_level = 0.95,
                         verbose = FALSE) {
  # -- validate ---------------------------------------------------------------
  if (!is.numeric(group1) || !is.numeric(group2))
    stop("group1 and group2 must be numeric vectors.")
  if (length(group1) != length(group2))
    stop("group1 and group2 must have the same length.")
  if (!is.numeric(two) || length(two) != 1L || two <= 0)
    stop("'two' must be a positive scalar.")
  mode <- as.integer(mode)
  if (!mode %in% c(1L, 2L))
    stop("'mode' must be 1 or 2.")
  if (!(is.numeric(conf_level) && length(conf_level) == 1L &&
        conf_level > 0 && conf_level < 1))
    stop("'conf_level' must be in (0, 1).")

  called.with <- length(group1)
  if (isTRUE(verbose)) cat("Using", ba_openmp_threads(), "OpenMP threads\n")

  # -- compute in C++ ---------------------------------------------------------
  ba_out <- bland_altman_cpp(group1, group2, two, mode, conf_level)

  attr(ba_out, "method")      <- "Bland-Altman"
  attr(ba_out, "description") <- "Mean difference and limits of agreement with CIs"
  attr(ba_out, "package")     <- "matrixCorr"
  attr(ba_out, "conf.level")  <- conf_level
  attr(ba_out, "called.with") <- length(group1)

  # keep 'ba' first, and add a friendly alias 'bland_altman' as second class
  class(ba_out) <- c("ba", "bland_altman")
  ba_out
}

#' @rdname bland_altman
#' @method print ba
#' @param x A \code{"ba"} object.
#' @param digits Number of digits for estimates (default 3).
#' @param ci_digits Number of digits for CI bounds (default 3).
#' @param ... Unused.
#' @export
print.ba <- function(x, digits = 3, ci_digits = 3, ...) {
  if (!inherits(x, "ba")) stop("Object is not of class 'ba'.")

  n   <- as.integer(x$based.on)
  two <- as.numeric(x$two)
  cl  <- suppressWarnings(as.numeric(attr(x, "conf.level")))
  if (!is.finite(cl)) cl <- NA_real_

  # core numbers (as scalars)
  bias   <- as.numeric(x$mean.diffs)
  loa_lo <- as.numeric(x$lower.limit)
  loa_hi <- as.numeric(x$upper.limit)
  sd_d   <- as.numeric(x$critical.diff) / two
  width  <- loa_hi - loa_lo

  # CIs (robust extraction by name)
  cil <- function(nm) as.numeric(x$CI.lines[[nm]])
  bias_l <- cil("mean.diff.ci.lower"); bias_u <- cil("mean.diff.ci.upper")
  lo_l   <- cil("lower.limit.ci.lower"); lo_u <- cil("lower.limit.ci.upper")
  hi_l   <- cil("upper.limit.ci.lower"); hi_u <- cil("upper.limit.ci.upper")

  # header
  if (is.finite(cl)) {
    cat(sprintf("Bland-Altman (n = %d) - LoA = mean +/- %.3g * SD, %g%% CI\n\n", n, two, 100*cl))
  } else {
    cat(sprintf("Bland-Altman (n = %d) - LoA = mean +/- %.3g * SD\n\n", n, two))
  }

  # nicely aligned three-row table
  df <- data.frame(
    quantity = c("Mean difference", "Lower LoA", "Upper LoA"),
    estimate = c(bias, loa_lo, loa_hi),
    lwr      = c(bias_l, lo_l, hi_l),
    upr      = c(bias_u, lo_u, hi_u),
    check.names = FALSE,
    row.names   = NULL
  )
  df$estimate <- formatC(df$estimate, format = "f", digits = digits)
  df$lwr      <- formatC(df$lwr,      format = "f", digits = ci_digits)
  df$upr      <- formatC(df$upr,      format = "f", digits = ci_digits)

  print(df, row.names = FALSE, right = FALSE)
  cat(sprintf("\nSD(differences): %s   LoA width: %s\n",
              formatC(sd_d, format = "f", digits = digits),
              formatC(width, format = "f", digits = digits)))
  invisible(x)
}

#' @rdname bland_altman
#' @method plot ba
#' @param x A \code{"ba"} object.
#' @param title Plot title.
#' @param subtitle Optional subtitle. If NULL, shows n and LoA summary.
#' @param point_alpha Point transparency.
#' @param point_size Point size.
#' @param line_size Line width for mean/LoA.
#' @param shade_ci Logical; if TRUE, draw shaded CI bands instead of 6 dashed
#' lines.
#' @param shade_alpha Transparency of CI bands.
#' @param smoother One of "none", "loess", "lm" to visualize proportional bias.
#' @param symmetrize_y Logical; if TRUE, y-axis centered at mean difference
#' with symmetric limits.
#' @param ... Passed to \code{ggplot2::theme()} (ggplot path) or \code{plot()}.
#' @importFrom graphics abline lines par rect
#' @export
plot.ba <- function(x,
                    title = "Bland-Altman Plot",
                    subtitle = NULL,
                    point_alpha = 0.7,
                    point_size  = 2.2,
                    line_size   = 0.8,
                    shade_ci    = TRUE,
                    shade_alpha = 0.08,
                    smoother    = c("none", "loess", "lm"),
                    symmetrize_y = TRUE,
                    ...) {
  if (!inherits(x, "ba")) stop("x must be of class 'ba'.")
  smoother <- match.arg(smoother)

  means <- as.numeric(x$means)
  diffs <- as.numeric(x$diffs)

  # scalars
  md    <- as.numeric(x$mean.diffs)
  loaL  <- as.numeric(x$lower.limit)
  loaU  <- as.numeric(x$upper.limit)
  two   <- as.numeric(x$two)
  n     <- as.integer(x$based.on)
  sd_d  <- as.numeric(x$critical.diff) / two
  cl    <- suppressWarnings(as.numeric(attr(x, "conf.level")))
  ci    <- function(nm) as.numeric(x$CI.lines[[nm]])

  if (is.null(subtitle)) {
    subtitle <- if (is.finite(cl)) {
      sprintf("n = %d  *  mean diff = %.2f  *  LoA = [%.2f, %.2f]  *  %g%% CI shown",
              n, md, loaL, loaU, 100*cl)
    } else {
      sprintf("n = %d  *  mean diff = %.2f  *  LoA = [%.2f, %.2f]",
              n, md, loaL, loaU)
    }
  }

  # y limits (symmetric around md if requested)
  y_rng <- range(c(diffs, loaL, loaU), na.rm = TRUE)
  if (isTRUE(symmetrize_y)) {
    half <- max(abs(y_rng - md))
    y_rng <- c(md - half, md + half)
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    ## ---------- Base fallback ----------
    plot(means, diffs,
         xlab = "Mean of methods", ylab = "Difference between methods",
         main = title, sub = subtitle,
         pch = 16, cex = point_size / 2.2,
         col = grDevices::gray(0.2, alpha = point_alpha),
         ylim = y_rng, ...)
    # shaded CI bands (rectangles)
    if (shade_ci) {
      usr <- par("usr")
      rect(xleft = usr[1], xright = usr[2],
           ybottom = ci("mean.diff.ci.lower"), ytop = ci("mean.diff.ci.upper"),
           border = NA, col = grDevices::gray(0.2, alpha = shade_alpha))
      rect(xleft = usr[1], xright = usr[2],
           ybottom = ci("lower.limit.ci.lower"), ytop = ci("lower.limit.ci.upper"),
           border = NA, col = grDevices::gray(0.2, alpha = shade_alpha))
      rect(xleft = usr[1], xright = usr[2],
           ybottom = ci("upper.limit.ci.lower"), ytop = ci("upper.limit.ci.upper"),
           border = NA, col = grDevices::gray(0.2, alpha = shade_alpha))
    } else {
      abline(h = c(ci("mean.diff.ci.lower"), ci("mean.diff.ci.upper")), lty = 2)
      abline(h = c(ci("lower.limit.ci.lower"), ci("lower.limit.ci.upper")), lty = 2)
      abline(h = c(ci("upper.limit.ci.lower"), ci("upper.limit.ci.upper")), lty = 2)
    }
    # reference & main lines
    abline(h = 0,   col = "grey70", lty = 3)
    abline(h = md,  lwd = line_size * 1.4)
    abline(h = loaL, lwd = line_size * 1.2)
    abline(h = loaU, lwd = line_size * 1.2)

    # optional smoother
    if (smoother != "none") {
      fit <- if (smoother == "lm") stats::lm(diffs ~ means) else stats::loess(diffs ~ means)
      xs <- seq(min(means), max(means), length.out = 200)
      ys <- stats::predict(fit, newdata = data.frame(means = xs))
      lines(xs, ys, lty = 1, col = "grey40")
    }
    return(invisible(NULL))
  }

  ## ---------- ggplot path ----------
  df <- data.frame(means = means, diffs = diffs)
  xr <- range(means, na.rm = TRUE)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = means, y = diffs))

  if (shade_ci) {
    p <- p +
      ggplot2::annotate("rect", xmin = -Inf, xmax = Inf,
                        ymin = ci("mean.diff.ci.lower"), ymax = ci("mean.diff.ci.upper"),
                        alpha = shade_alpha) +
      ggplot2::annotate("rect", xmin = -Inf, xmax = Inf,
                        ymin = ci("lower.limit.ci.lower"), ymax = ci("lower.limit.ci.upper"),
                        alpha = shade_alpha) +
      ggplot2::annotate("rect", xmin = -Inf, xmax = Inf,
                        ymin = ci("upper.limit.ci.lower"), ymax = ci("upper.limit.ci.upper"),
                        alpha = shade_alpha)
  } else {
    p <- p +
      ggplot2::geom_hline(yintercept = c(ci("mean.diff.ci.lower"),
                                         ci("mean.diff.ci.upper"),
                                         ci("lower.limit.ci.lower"),
                                         ci("lower.limit.ci.upper"),
                                         ci("upper.limit.ci.lower"),
                                         ci("upper.limit.ci.upper")),
                          linetype = "dashed")
  }

  p <- p +
    ggplot2::geom_point(alpha = point_alpha, size = point_size) +
    ggplot2::geom_hline(yintercept = 0, size = 0.4, linetype = "dotted", color = "grey40") +
    ggplot2::geom_hline(yintercept = md,   size = line_size) +
    ggplot2::geom_hline(yintercept = loaL, size = line_size) +
    ggplot2::geom_hline(yintercept = loaU, size = line_size)

  if (smoother == "lm") {
    p <- p + ggplot2::geom_smooth(method = "lm", se = FALSE, linewidth = 0.7)
  } else if (smoother == "loess") {
    p <- p + ggplot2::geom_smooth(method = "loess", se = FALSE, linewidth = 0.7, span = 0.9)
  }

  p <- p +
    ggplot2::coord_cartesian(ylim = y_rng, expand = TRUE) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), ...) +
    ggplot2::labs(title = title, subtitle = subtitle,
                  x = "Mean of methods", y = "Difference between methods")

  p
}
