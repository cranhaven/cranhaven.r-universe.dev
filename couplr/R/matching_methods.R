# ==============================================================================
# S3 methods for matching_result
# ==============================================================================
# print/summary/plot for the object match_couples() returns. The engine that
# builds it lives in R/matching_core.R.
# ==============================================================================

#' Print method for matching results
#'
#' @param x A matching_result object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input object `x`.
#' @export
#' @method print matching_result
print.matching_result <- function(x, ...) {
  cat("Matching Result\n")
  cat("===============\n\n")

  cat("Method:", x$info$method, "\n")
  if (!is.null(x$info$strategy)) {
    cat("Strategy:", x$info$strategy, "\n")
  }
  cat("Pairs matched:", x$info$n_matched, "\n")

  if (!is.null(x$info$n_blocks) && x$info$n_blocks > 1) {
    cat("Blocks:", x$info$n_blocks, "\n")
  }

  if (!is.null(x$unmatched)) {
    cat("Unmatched (left):", length(x$unmatched$left), "\n")
    cat("Unmatched (right):", length(x$unmatched$right), "\n")
  }

  cat("Total distance:", sprintf("%.4f", x$info$total_distance), "\n")
  if (!is.null(x$status)) {
    cat("Status:", x$status, "\n")
  }
  if (!is.null(x$certificate) && is.null(x$cardinality)) {
    cat("Certified optimal:", x$certificate$certified_optimal, "\n")
  }
  if (!is.null(x$cardinality)) {
    print(x$cardinality)
  }
  if (!is.null(x$search)) {
    cat(sprintf("Pairs generated: %s of %s (%.4g%%), %s rounds\n",
                format(x$search$candidate_edges, big.mark = ",",
                       scientific = FALSE),
                format(x$search$possible_edges, big.mark = ",",
                       scientific = FALSE),
                100 * x$search$candidate_edges / x$search$possible_edges,
                x$search$n_rounds))
  }

  if (nrow(x$pairs) > 0) {
    cat("\nMatched pairs:\n")
    print(x$pairs, n = 10)
  }

  invisible(x)
}

#' Summary method for matching results
#'
#' @param object A matching_result object
#' @param ... Additional arguments (ignored)
#'
#' @return A list containing summary statistics (invisibly)
#' @export
#' @method summary matching_result
summary.matching_result <- function(object, ...) {
  n_matched <- object$info$n_matched
  total_dist <- object$info$total_distance
  mean_dist <- if (n_matched > 0) total_dist / n_matched else NA_real_
  distances <- object$pairs$distance

  # Match rate: the share of focal (left) units that kept at least one
  # partner. n_matched counts pairs, so under ratio > 1 or replacement it
  # exceeds the number of units and cannot serve as the numerator.
  n_left <- object$info$n_left %||% NA_integer_
  n_right <- object$info$n_right %||% NA_integer_
  n_left_matched <- if (!is.null(object$pairs$left_id)) {
    length(unique(object$pairs$left_id))
  } else {
    n_matched
  }
  match_rate <- if (!is.na(n_left) && n_left > 0) {
    n_left_matched / n_left
  } else if (!is.na(n_right) && n_right > 0) {
    min(n_matched, n_right) / n_right
  } else {
    NA_real_
  }

  # Distance percentiles
  distance_percentiles <- if (length(distances) > 0) {
    stats::quantile(distances, c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95),
                    na.rm = TRUE)
  } else {
    NULL
  }

  # Build summary list
  out <- list(
    method = object$info$method,
    strategy = object$info$strategy,
    n_matched = n_matched,
    n_blocks = object$info$n_blocks %||% 1L,
    total_distance = total_dist,
    mean_distance = mean_dist,
    match_rate = match_rate,
    distance_stats = if (length(distances) > 0) {
      list(
        min = min(distances, na.rm = TRUE),
        q1 = stats::quantile(distances, 0.25, na.rm = TRUE),
        median = stats::median(distances, na.rm = TRUE),
        q3 = stats::quantile(distances, 0.75, na.rm = TRUE),
        max = max(distances, na.rm = TRUE),
        sd = stats::sd(distances, na.rm = TRUE)
      )
    } else NULL,
    distance_percentiles = distance_percentiles,
    n_unmatched_left = if (!is.null(object$unmatched)) length(object$unmatched$left) else NA_integer_,
    n_unmatched_right = if (!is.null(object$unmatched)) length(object$unmatched$right) else NA_integer_,
    replace = object$info$replace %||% FALSE,
    ratio = object$info$ratio %||% 1L
  )

  class(out) <- "summary.matching_result"
  out
}

#' @export
print.summary.matching_result <- function(x, ...) {
  cat("Matching Result Summary\n")
  cat("=======================\n\n")

  cat("Method:", x$method)
  if (!is.null(x$strategy)) cat(" (", x$strategy, ")", sep = "")
  cat("\n")

  cat("Pairs matched:", x$n_matched, "\n")
  if (x$n_blocks > 1) cat("Blocks:", x$n_blocks, "\n")

  if (!is.na(x$match_rate)) {
    cat("Match rate:", sprintf("%.1f%%", x$match_rate * 100), "\n")
  }

  if (isTRUE(x$replace)) cat("Replacement: yes\n")
  if (!is.null(x$ratio) && x$ratio > 1) cat("Ratio:", x$ratio, ":1\n")

  if (!is.na(x$n_unmatched_left)) {
    cat("Unmatched: ", x$n_unmatched_left, " left, ",
        x$n_unmatched_right, " right\n", sep = "")
  }

  cat("\nDistance Statistics:\n")
  cat("  Total:", sprintf("%.4f", x$total_distance), "\n")
  cat("  Mean:", sprintf("%.4f", x$mean_distance), "\n")

  if (!is.null(x$distance_stats)) {
    ds <- x$distance_stats
    cat("  Min:", sprintf("%.4f", ds$min), "\n")
    cat("  Q1:", sprintf("%.4f", ds$q1), "\n")
    cat("  Median:", sprintf("%.4f", ds$median), "\n")
    cat("  Q3:", sprintf("%.4f", ds$q3), "\n")
    cat("  Max:", sprintf("%.4f", ds$max), "\n")
    cat("  SD:", sprintf("%.4f", ds$sd), "\n")
  }

  if (!is.null(x$distance_percentiles)) {
    cat("\nDistance Percentiles:\n")
    pct_names <- names(x$distance_percentiles)
    for (i in seq_along(x$distance_percentiles)) {
      cat(sprintf("  %s: %.4f\n", pct_names[i], x$distance_percentiles[i]))
    }
  }

  invisible(x)
}

#' Plot method for matching results
#'
#' Produces a histogram of pairwise distances from a matching result.
#'
#' @param x A matching_result object
#' @param type Type of plot: "histogram" (default), "density", or "ecdf"
#' @param ... Additional arguments passed to plotting functions
#'
#' @return The matching_result object (invisibly)
#' @export
#' @method plot matching_result
plot.matching_result <- function(x, type = c("histogram", "density", "ecdf"), ...) {
  type <- match.arg(type)
  distances <- x$pairs$distance

  if (length(distances) == 0) {
    message("No matched pairs to plot")
    return(invisible(x))
  }

  main_title <- paste0("Matching Distances (n=", length(distances), ")")


  switch(type,
    histogram = {
      graphics::hist(distances,
                     main = main_title,
                     xlab = "Distance",
                     col = "steelblue",
                     border = "white",
                     ...)
    },
    density = {
      d <- stats::density(distances)
      graphics::plot(d,
                     main = main_title,
                     xlab = "Distance",
                     ...)
      graphics::polygon(d, col = "steelblue", border = "steelblue")
    },
    ecdf = {
      graphics::plot(stats::ecdf(distances),
                     main = main_title,
                     xlab = "Distance",
                     ylab = "Cumulative Proportion",
                     ...)
    }
  )

  invisible(x)
}
