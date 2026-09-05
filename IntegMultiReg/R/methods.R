## S3 methods for objects of class "imr".

## Internal: marginal posterior inclusion probability (mPIP) matrix for one
## platform, with subgroup (row) and feature (column) names attached.
#' @keywords internal
#' @noRd
.imr_mpip <- function(object, platform) {
  m <- object$gam_mean[[platform]]
  rn <- object$model_bitstrings[object$platform_models[[platform]]]
  cn <- object$feature_names[[platform]]
  if (!is.null(rn) && length(rn) == nrow(m)) rownames(m) <- rn
  if (!is.null(cn) && length(cn) == ncol(m)) colnames(m) <- cn
  m
}

## Internal: generic platform labels used to decode availability bitstrings.
#' @keywords internal
#' @noRd
.imr_platform_codes <- function(n_platform) {
  paste0("P", seq_len(n_platform))
}


## Internal: translate a bitstring into generic platform labels.  The right-most
## bit corresponds to P1, matching the convention used throughout the package.
#' @keywords internal
#' @noRd
.imr_bitstring_codes <- function(bitstring, n_platform) {
  bits <- strsplit(as.character(bitstring), "", fixed = TRUE)[[1]]
  if (length(bits) < n_platform) {
    bits <- c(rep("0", n_platform - length(bits)), bits)
  }
  present <- which(rev(bits) == "1")
  if (!length(present)) return("none")
  paste(.imr_platform_codes(n_platform)[present], collapse = " + ")
}


#' Marginal Posterior Inclusion Probabilities of an IMR Fit
#'
#' Extracts the posterior mean variable-selection probabilities (the marginal
#' posterior inclusion probabilities, mPIP) of a fitted model.
#'
#' @param object A fitted object of class `"imr"`.
#' @param ... Unused; present for S3 compatibility.
#' @return A named list with one matrix per platform.  Rows are the subgroups
#'   containing that platform (labelled by their availability bitstrings) and
#'   columns are the platform features.
#' @seealso [imr()]
#' @export
coef.imr <- function(object, ...) {
  out <- lapply(seq_len(object$n_platform), function(l) .imr_mpip(object, l))
  names(out) <- object$platform_names
  out
}


#' Print Method for IMR Fits
#'
#' Prints a compact overview of the fit, including a platform key (`P1`,
#' `P2`, ...) that decodes the availability-subgroup bitstrings.  Optionally,
#' it can also print the top-ranked features per platform, ranked by each
#' feature's maximum mPIP over availability subgroups containing that platform.
#'
#' @param x A fitted object of class `"imr"`.
#' @param threshold Inclusion-probability threshold used to count selected
#'   features (default `0.5`).
#' @param rank Logical; if `TRUE`, print a short ranked feature table for each
#'   platform (default `FALSE`).
#' @param top Integer; when `rank = TRUE`, the number of top-ranked features to
#'   show per platform (default `5`).
#' @param ... Unused; present for S3 compatibility.
#' @return `x`, invisibly.
#' @export
print.imr <- function(x, threshold = 0.5, rank = FALSE, top = 5, ...) {
  threshold <- .imr_check_numeric_vector(
    threshold, "threshold", length = 1, nonnegative = TRUE
  )
  if (threshold > 1) {
    .imr_abort("`threshold` must be between 0 and 1.")
  }
  .imr_check_flag(rank, "rank")
  top <- .imr_check_integer_scalar(top, "top", min = 1)
  cat("Integrative Bayesian Multi-Platform Regression (IMR)\n")
  cat("----------------------------------------------------\n")
  if (!is.null(x$call)) {
    cat("Call:\n  ")
    print(x$call)
  }
  cat(sprintf("\nOutcome type : %s\n", x$type_outcome))
  cat(sprintf("Method       : %s\n", x$method))
  cat(sprintf("Platforms    : %d (%s)\n", x$n_platform,
              paste(x$platform_names, collapse = ", ")))
  cat(sprintf("MCMC         : %d retained draws after %d burn-in\n",
              x$sample_mcmc[["total"]], x$sample_mcmc[["burnin"]]))

  codes <- .imr_platform_codes(x$n_platform)
  cat("\nPlatform key:\n")
  key <- paste(sprintf("  %s = %s", codes, x$platform_names), collapse = "\n")
  cat(key, "\n", sep = "")

  cat("\nAvailability subgroups modelled (bitstring : platforms : size):\n")
  subgroup_codes <- vapply(
    x$model_bitstrings, .imr_bitstring_codes, character(1),
    n_platform = x$n_platform
  )
  st <- paste(sprintf("  %-*s : %-*s : %d",
                      max(nchar(x$model_bitstrings)), x$model_bitstrings,
                      max(nchar(subgroup_codes)), subgroup_codes,
                      as.integer(x$sample_size)),
              collapse = "\n")
  cat(st, "\n", sep = "")

  cat(sprintf("\nFeatures with mPIP > %.2f (in any subgroup):\n", threshold))
  for (l in seq_len(x$n_platform)) {
    m <- x$gam_mean[[l]]
    sel <- if (nrow(m) > 0 && ncol(m) > 0) sum(apply(m, 2, max) > threshold) else 0L
    cat(sprintf("  %-12s : %d of %d\n", x$platform_names[l], sel, ncol(m)))
  }
  if (rank) {
    cat(sprintf("\nTop %d ranked features by maximum subgroup mPIP:\n", top))
    for (l in seq_len(x$n_platform)) {
      m <- .imr_mpip(x, l)
      cat(sprintf("  %s\n", x$platform_names[l]))
      if (nrow(m) == 0 || ncol(m) == 0) {
        cat("    (no selectable features)\n")
        next
      }
      maxp <- apply(m, 2, max)
      which_sg <- rownames(m)[apply(m, 2, which.max)]
      ord <- order(maxp, decreasing = TRUE)
      ord <- ord[seq_len(min(top, length(ord)))]
      tab <- data.frame(
        feature = colnames(m)[ord],
        max_mpip = round(maxp[ord], 3),
        subgroup = which_sg[ord],
        row.names = NULL,
        stringsAsFactors = FALSE
      )
      lines <- utils::capture.output(print(tab, row.names = FALSE))
      cat(paste0("    ", lines), sep = "\n")
      cat("\n")
    }
  }
  invisible(x)
}


#' Summarize an IMR Fit
#'
#' Produces a per-platform summary of the selected features (those whose
#' marginal posterior inclusion probability exceeds `threshold` in at least one
#' subgroup), ranked by their maximum inclusion probability.
#'
#' @param object A fitted object of class `"imr"`.
#' @param threshold Inclusion-probability threshold for selection (default
#'   `0.5`).
#' @param ... Unused; present for S3 compatibility.
#' @return An object of class `"summary.imr"`: a list with the run metadata and,
#'   for each platform, a data frame of selected features with their maximum
#'   mPIP and the subgroup achieving it.
#' @export
summary.imr <- function(object, threshold = 0.5, ...) {
  if (!inherits(object, "imr")) {
    .imr_abort("`object` must be an `imr` object returned by `imr()`.")
  }
  threshold <- .imr_check_numeric_vector(
    threshold, "threshold", length = 1, nonnegative = TRUE
  )
  if (threshold > 1) {
    .imr_abort("`threshold` must be between 0 and 1.")
  }
  selected <- vector("list", object$n_platform)
  names(selected) <- object$platform_names
  for (l in seq_len(object$n_platform)) {
    m <- .imr_mpip(object, l)
    if (nrow(m) == 0 || ncol(m) == 0) {
      selected[[l]] <- data.frame(feature = character(0), max_mpip = numeric(0),
                                  subgroup = character(0))
      next
    }
    maxp <- apply(m, 2, max)
    which_sg <- rownames(m)[apply(m, 2, which.max)]
    keep <- which(maxp > threshold)
    ord <- keep[order(maxp[keep], decreasing = TRUE)]
    selected[[l]] <- data.frame(
      feature = colnames(m)[ord],
      max_mpip = round(maxp[ord], 3),
      subgroup = which_sg[ord],
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  }
  out <- list(
    call = object$call,
    type_outcome = object$type_outcome,
    method = object$method,
    threshold = threshold,
    sample_size = object$sample_size,
    model_bitstrings = object$model_bitstrings,
    platform_names = object$platform_names,
    selected = selected
  )
  class(out) <- "summary.imr"
  out
}

#' @rdname summary.imr
#' @param x A `"summary.imr"` object.
#' @export
print.summary.imr <- function(x, ...) {
  cat("Integrative Bayesian Multi-Platform Regression (IMR) -- summary\n")
  cat("--------------------------------------------------------------\n")
  cat(sprintf("Outcome type : %s   Method: %s\n", x$type_outcome, x$method))
  cat(sprintf("Selection threshold (mPIP) : %.2f\n\n", x$threshold))
  for (l in seq_along(x$selected)) {
    df <- x$selected[[l]]
    cat(sprintf("Platform '%s': %d selected feature(s)\n",
                x$platform_names[l], nrow(df)))
    if (nrow(df) > 0) {
      print(df, row.names = FALSE)
    }
    cat("\n")
  }
  invisible(x)
}


#' Plot Method for IMR Fits
#'
#' @description
#' Visualizes a fitted `"imr"` object.  Three plot types are available:
#' \describe{
#'   \item{`"selection"`}{Heatmap of the marginal posterior inclusion
#'     probabilities (mPIP), one panel per platform, with features on the
#'     horizontal axis, availability subgroups on the vertical axis and a small
#'     intensity legend showing that darker values are closer to 1.}
#'   \item{`"theta"`}{Heatmap of the posterior mean MRF interaction parameters
#'     between availability subgroups, one panel per platform.}
#'   \item{`"trace"`}{Trace plot of the log-posterior across MCMC iterations.}
#' }
#' See [plot_top_features()] and [plot_subgroup_sizes()] for two further ready
#' made displays.
#'
#' @param x A fitted object of class `"imr"`.
#' @param type Character; one of `"selection"` (default), `"theta"` or
#'   `"trace"`.
#' @param platform Optional integer vector selecting which platforms to display
#'   for the `"selection"` and `"theta"` plots; defaults to all platforms.
#' @param base_cex Overall text-size multiplier. The `cex_*` arguments default
#'   to values derived from this multiplier (default `1`).
#' @param cex_axis Axis-label size multiplier. If `NULL`, a plot-specific
#'   default derived from `base_cex` is used.
#' @param cex_lab Axis-title size multiplier. If `NULL`, a plot-specific
#'   default derived from `base_cex` is used.
#' @param cex_main Main-title size multiplier. If `NULL`, a plot-specific
#'   default derived from `base_cex` is used.
#' @param col Optional colours. For heatmaps this is the colour scale; for
#'   trace plots this is the line colour.
#' @param palette Optional heatmap palette name used when `col = NULL`.
#'   Selection plots default to `"grey"` to preserve the mPIP intensity scale;
#'   theta plots default to the muted grey-blue `"heatmap"` palette.
#' @param legend Logical; for `"selection"` plots, should the mPIP intensity
#'   legend be drawn (default `TRUE`)?
#' @param legend_width Relative width of the intensity-legend panel for
#'   `"selection"` plots (default `0.28`).
#' @param mar,mgp Optional graphical margin and axis-title placement vectors
#'   passed to [graphics::par()] for finer layout control.
#' @param ... Further graphical parameters passed to the underlying plotting
#'   functions.
#' @return `NULL`, invisibly; called for the side effect of producing a plot.
#' @seealso [imr()], [plot_top_features()], [plot_subgroup_sizes()]
#' @export
plot.imr <- function(x, type = c("selection", "theta", "trace"),
                     platform = NULL, base_cex = 1, cex_axis = NULL,
                     cex_lab = NULL, cex_main = NULL, col = NULL,
                     palette = NULL,
                     legend = TRUE, legend_width = 0.28,
                     mar = NULL, mgp = NULL, ...) {
  if (!inherits(x, "imr")) {
    .imr_abort("`x` must be an `imr` object returned by `imr()`.")
  }
  type <- match.arg(type)
  dots <- list(...)
  cex_defaults <- if (type == "selection") {
    list(axis = 1.05, lab = 1.3, main = 1.4,
         names = 1, legend = 1, values = 1)
  } else {
    list(axis = 0.95, lab = 1.1, main = 1.15,
         names = 1, legend = 1, values = 1)
  }
  sz <- .imr_plot_cex(
    dots, base_cex = base_cex, cex_axis = cex_axis,
    cex_lab = cex_lab, cex_main = cex_main,
    defaults = cex_defaults
  )
  cex_axis <- sz$axis
  cex_lab <- sz$lab
  cex_main <- sz$main
  dots <- sz$dots
  .imr_check_flag(legend, "legend")
  legend_width <- .imr_check_numeric_vector(
    legend_width, "legend_width", length = 1, positive = TRUE
  )
  if (!is.null(platform)) {
    if (!is.numeric(platform) || length(platform) == 0L ||
        any(!is.finite(platform)) || any(platform != as.integer(platform)) ||
        any(platform < 1L) || any(platform > x$n_platform)) {
      .imr_abort(sprintf(
        "`platform` must contain whole-number indices between 1 and %d.",
        x$n_platform
      ))
    }
    platform <- as.integer(platform)
  }

  op <- graphics::par(no.readonly = TRUE)
  on.exit({
    if (type == "selection") try(graphics::layout(1), silent = TRUE)
    try(graphics::par(op), silent = TRUE)
  }, add = TRUE)

  if (type == "trace") {
    lp <- x$log_posterior
    pp <- .imr_plot_par(mar, mgp, default_mar = c(4.8, 4.8, 3, 1))
    graphics::par(mar = pp$mar, mgp = pp$mgp)
    trace_col <- if (is.null(col)) .imr_plot_trace_colour() else col
    do.call(graphics::plot, c(list(
      x = seq_along(lp), y = lp, type = "l",
      xlab = "MCMC iteration", ylab = "Log-posterior",
      main = "Log-posterior trace", cex.axis = cex_axis,
      cex.lab = cex_lab, cex.main = cex_main, col = trace_col
    ), dots))
    graphics::abline(v = x$sample_mcmc[["burnin"]], lty = 2, col = "grey50")
    return(invisible(NULL))
  }

  plats <- if (is.null(platform)) seq_len(x$n_platform) else platform
  heat_palette <- if (is.null(palette)) {
    if (type == "selection") "grey" else "heatmap"
  } else {
    palette
  }
  heat_col <- if (is.null(col)) {
    .imr_plot_palette(64, palette = heat_palette)
  } else {
    .imr_plot_palette(length(col), col = col)
  }

  if (type == "selection") {
    layout_widths <- rep(1, length(plats))
    if (legend) layout_widths <- c(layout_widths, legend_width)
    graphics::layout(
      matrix(seq_along(layout_widths), nrow = 1L),
      widths = layout_widths
    )
  } else if (length(plats) > 1) {
    graphics::par(mfrow = c(1, length(plats)))
  }

  panel_par <- .imr_plot_par(
    mar, mgp,
    default_mar = if (type == "selection") {
      c(5.7, 4.8, 2.9, 0.5)
    } else {
      c(4.8, 4.8, 3, 1)
    },
    default_mgp = if (type == "selection") c(3.6, 0.9, 0) else c(2.7, 0.8, 0)
  )
  for (l in plats) {
    if (type == "selection") {
      m <- .imr_mpip(x, l)
      main <- sprintf("mPIP: %s", x$platform_names[l])
      xlab <- "Features"; ylab <- "Availability subgroups"
      rlab <- rownames(m)
    } else {
      m <- x$theta_mean[[l]]
      rlab <- x$model_bitstrings[x$platform_models[[l]]]
      if (!is.null(rlab) && length(rlab) == nrow(m)) {
        rownames(m) <- colnames(m) <- rlab
      }
      main <- sprintf("Theta: %s", x$platform_names[l])
      xlab <- "Availability subgroups"; ylab <- "Availability subgroups"
    }
    graphics::par(mar = panel_par$mar, mgp = panel_par$mgp)
    if (nrow(m) == 0 || ncol(m) == 0) {
      graphics::plot.new()
      graphics::title(main = paste(main, "(empty)"), cex.main = cex_main)
      next
    }
    zlim <- if (type == "selection") c(0, 1) else range(m, na.rm = TRUE)
    do.call(graphics::image, c(list(
      x = seq_len(ncol(m)), y = seq_len(nrow(m)), z = t(m),
      col = heat_col, zlim = zlim, axes = FALSE,
      xlab = xlab, ylab = ylab, main = main,
      cex.lab = cex_lab, cex.main = cex_main
    ), dots))
    if (!is.null(colnames(m)) && ncol(m) <= 40) {
      graphics::axis(1, at = seq_len(ncol(m)), labels = colnames(m),
                     las = 2, cex.axis = cex_axis)
    } else {
      graphics::axis(1, cex.axis = cex_axis)
    }
    graphics::axis(2, at = seq_len(nrow(m)), labels = rlab, las = 2,
                   cex.axis = cex_axis)
    graphics::box()
  }
  if (type == "selection" && legend) {
    legend_par <- .imr_plot_par(
      NULL, mgp, default_mar = c(5.7, 0.1, 2.9, 3.0),
      default_mgp = c(3.6, 0.9, 0)
    )
    graphics::par(mar = legend_par$mar, mgp = legend_par$mgp)
    graphics::plot.new()
    graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
    yb <- seq(0, 1, length.out = length(heat_col) + 1L)
    graphics::rect(0.18, yb[-length(yb)], 0.52, yb[-1L],
                   col = heat_col, border = NA)
    graphics::axis(4, at = c(0, 0.25, 0.5, 0.75, 1),
                   labels = c("0", "0.25", "0.5", "0.75", "1"),
                   las = 1, cex.axis = cex_axis, tck = -0.18)
    graphics::mtext("mPIP", side = 3, line = 0.1, at = 0.35,
                    cex = 0.75 * cex_main)
    graphics::box(bty = "n")
  }
  invisible(NULL)
}
