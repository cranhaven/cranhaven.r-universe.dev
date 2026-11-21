# DIAGNOSTIC TEST ACCURACY AND CONFUSION MATRIX HELPERS
# Evaluates binary index tests against reference standards (sensitivity, specificity, predictive values).
# Preserves unrounded metrics, 2x2 confusion counts, and provides console, tabular, and graphical renderers.

#########
# METRIC LABELS AND LEVEL RESOLUTION
# Label mapping and positive classification level resolution for index test and reference standard.

#' Lookup named vector of clinical metric labels
#' @keywords internal
#' @noRd
.diag_metric_labels <- function() {
  c(
    sensitivity = "Sensitivity",
    specificity = "Specificity",
    ppv = "Pos Pred Value (PPV)",
    npv = "Neg Pred Value (NPV)",
    accuracy = "Accuracy",
    prevalence = "Prevalence",
    lr_pos = "Likelihood Ratio +",
    lr_neg = "Likelihood Ratio -",
    youden_j = "Youden Index",
    f1 = "F1 Score",
    dor = "Diagnostic Odds Ratio",
    kappa = "Cohen's Kappa"
  )
}

#' Resolve positive classification level for the reference standard
#' @keywords internal
#' @noRd
.resolve_pos_level <- function(value, levs, candidates, var_label, arg_name) {
  if (is.null(value)) {
    matched <- intersect(levs, candidates)
    if (length(matched) > 0L) {
      message(sprintf(
        "Auto-detected %s positive level: '%s'", var_label, matched[1L]
      ))
      return(matched[1L])
    }

    last <- levs[length(levs)]
    message(sprintf(
      "Using last %s level as positive: '%s'. Specify '%s' if incorrect.",
      var_label, last, arg_name
    ))
    return(last)
  }

  value_chr <- as.character(value)
  if (value_chr %in% levs) {
    return(value_chr)
  }

  idx <- suppressWarnings(as.integer(value))
  if (!is.na(idx) && idx >= 1L && idx <= length(levs)) {
    message(sprintf(
      "Using %s level %d as positive: '%s'", var_label, idx, levs[idx]
    ))
    return(levs[idx])
  }

  simtab_abort_input(c(
    "Positive level {.val {value_chr}} not found in {var_label} levels.",
    "i" = "Levels available: {.val {levs}}.",
    "v" = "Name one of the listed levels, or pass its position as a number."
  ))
}

#' Resolve positive classification level for the index diagnostic test
#' @keywords internal
#' @noRd
.resolve_pos_level_test <- function(value, levs_test, pos_ref, candidates) {
  if (is.null(value)) {
    if (pos_ref %in% levs_test) {
      return(pos_ref)
    }

    matched <- intersect(levs_test, candidates)
    if (length(matched) > 0L) {
      message(sprintf("Auto-detected test positive level: '%s'", matched[1L]))
      return(matched[1L])
    }

    simtab_abort_input(c(
      "Cannot auto-detect the test positive level.",
      "i" = "The reference standard uses {.val {pos_ref}}, which is not among the
             index-test levels {.val {levs_test}}.",
      "v" = "Name it explicitly with {.arg test_positive}."
    ))
  }

  value_chr <- as.character(value)
  if (value_chr %in% levs_test) {
    return(value_chr)
  }

  idx <- suppressWarnings(as.integer(value))
  if (!is.na(idx) && idx >= 1L && idx <= length(levs_test)) {
    message(sprintf(
      "Using test level %d as positive: '%s'", idx, levs_test[idx]
    ))
    return(levs_test[idx])
  }

  simtab_abort_input(c(
    "Test positive level {.val {value_chr}} not found in the index-test levels.",
    "i" = "Levels available: {.val {levs_test}}.",
    "v" = "Name one of the listed levels, or pass its position as a number."
  ))
}

#########
# DISPLAY AND TABULAR FORMATTING
# Terminal summary and tabular presentation builders for diagnostic accuracy evidence.

#' Validate diagnostic test result class and inheritance
#' @keywords internal
#' @noRd
.diag_validate_result <- function(x) {
  x <- validate_simtab_result(x)
  if (!inherits(x, "simtab_diag")) {
    simtab_abort_input(c(
      "{.arg x} must be a {.fn diag_test} result.",
      "i" = "Received an object of class {.cls {class(x)[[1]]}}.",
      "v" = "Build one with {.code diag_test(data, test = rapid, ref = gold)}."
    ))
  }
  x
}

#' Resolve journal typography and formatting options for diagnostic results
#' @keywords internal
#' @noRd
.diag_style_spec <- function(x) {
  .resolve_table_style(x$meta$style %||% x$spec$style %||% "default")
}

#' Extract raw confusion matrix attribute from result object
#' @keywords internal
#' @noRd
.diag_confusion_matrix_attr <- function(x) {
  x$data$confusion_matrix
}

#' Assemble formatted presentation data frame of diagnostic metrics
#' @keywords internal
#' @noRd
.diag_display_frame <- function(x) {
  style_spec <- .diag_style_spec(x)
  metrics <- x$data$metrics
  labels <- x$meta$metric_labels[rownames(metrics)]
  digits <- x$spec$fmt$d %||% style_spec$digits_est
  as_percent <- isTRUE(x$meta$percent)
  prop_rows <- c("sensitivity", "specificity", "ppv", "npv", "accuracy", "prevalence")
  row_names <- rownames(metrics)

  lp <- substr(style_spec$ci_parens, 1, 1)
  rp <- substr(style_spec$ci_parens, 2, 2)

  fmt_value <- function(value, scale) {
    if (is.na(value)) {
      return(NA_character_)
    }
    if (isTRUE(scale)) {
      paste0(.fmt_fixed(value * 100, digits), "%")
    } else {
      .fmt_fixed(value, digits)
    }
  }

  estimate_chr <- character(nrow(metrics))
  ci_chr <- character(nrow(metrics))
  for (i in seq_len(nrow(metrics))) {
    scale_i <- as_percent && row_names[i] %in% prop_rows
    est <- metrics[i, "estimate"]
    estimate_chr[i] <- if (is.na(est)) "-" else fmt_value(est, scale_i)

    lo <- metrics[i, "conf.low"]
    hi <- metrics[i, "conf.high"]
    ci_chr[i] <- if (is.na(lo) || is.na(hi)) {
      ""
    } else {
      bounds <- c(fmt_value(lo, scale_i), fmt_value(hi, scale_i))
      paste0(lp, bounds[1], .ci_sep_safe(style_spec$ci_sep, bounds), bounds[2], rp)
    }
  }

  df <- data.frame(
    Metric = unname(labels),
    Estimate = estimate_chr,
    CI = ci_chr
  )

  attr(df, "confusion_matrix") <- .diag_confusion_matrix_attr(x)
  df
}

#' Assemble raw numeric tidy data frame of diagnostic metrics
#' @keywords internal
#' @noRd
.diag_tidy_frame <- function(x) {
  metrics <- x$data$metrics
  out <- data.frame(
    metric = unname(x$meta$metric_labels[rownames(metrics)]),
    estimate = metrics[, "estimate"],
    conf.low = metrics[, "conf.low"],
    conf.high = metrics[, "conf.high"]
  )
  attr(out, "confusion_matrix") <- .diag_confusion_matrix_attr(x)
  out
}

#' Console display printer for diagnostic accuracy evaluations
#' @keywords internal
#' @noRd
.diag_print <- function(x, digits = NULL, ...) {
  x <- .diag_validate_result(x)
  style_spec <- .diag_style_spec(x)
  if (is.null(digits)) {
    digits <- x$spec$fmt$d %||% style_spec$digits_est
  }
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) || digits < 0) {
    digits <- style_spec$digits_est
  }
  digits <- as.integer(digits)

  sep_major <- strrep("=", 60L)
  sep_minor <- strrep("-", 60L)
  display <- .diag_display_frame(x)
  ci_label <- sprintf("%.0f%%", x$meta$conf.level * 100)

  cat("\n", sep_major, "\n", sep = "")
  cat("  DIAGNOSTIC TEST EVALUATION\n")
  cat(sep_major, "\n\n", sep = "")

  cat(sprintf("  Sample size      : %d\n", x$meta$sample_size))
  cat(sprintf("  Confidence level : %s\n", ci_label))
  cat(sprintf("  CI method        : %s\n\n", x$meta$ci))

  cat("  Reference standard (gold standard):\n")
  cat(sprintf(
    "    Positive = '%s'   |   Negative = '%s'\n\n",
    x$meta$positive$ref,
    x$meta$negative$ref
  ))

  cat("  Diagnostic test:\n")
  cat(sprintf(
    "    Positive = '%s'   |   Negative = '%s'\n\n",
    x$meta$positive$test,
    x$meta$negative$test
  ))

  cat(sep_minor, "\n  Confusion Matrix\n", sep_minor, "\n", sep = "")
  print(x$data$confusion_matrix)
  cat("\n")

  cat(sep_major, "\n", sep = "")
  cat(sprintf("  Performance Metrics  (%s CI)\n", ci_label))
  cat(sep_major, "\n", sep = "")

  metric_width <- max(nchar(display$Metric))
  estimate_width <- max(nchar(c("Estimate", display$Estimate)))
  for (i in seq_len(nrow(display))) {
    if (i == 7L) {
      cat(sep_minor, "\n", sep = "")
    }
    metric <- formatC(display$Metric[i], width = -metric_width, flag = "-")
    estimate <- formatC(display$Estimate[i], width = estimate_width)
    cat(metric, " :  ", estimate, sep = "")
    if (nzchar(display$CI[i])) {
      cat("  ", display$CI[i], sep = "")
    }
    cat("\n")
  }
  cat("\n")

  invisible(x)
}

#' Convert diagnostic result to wide presentation or long tidy data frame
#' @keywords internal
#' @noRd
.diag_as_data_frame <- function(x, row.names = NULL, optional = FALSE, tidy = FALSE, ...) {
  x <- .diag_validate_result(x)
  if (isTRUE(tidy)) {
    return(.diag_tidy_frame(x))
  }
  .diag_display_frame(x)
}

#' Convert diagnostic result into a formatted flextable
#' @keywords internal
#' @noRd
.diag_as_flextable <- function(x, footnotes = NULL, ...) {
  x <- .diag_validate_result(x)
  .require_pkg("flextable")

  df <- .diag_display_frame(x)
  ft <- flextable::flextable(df, ...)

  matrix_lines <- c(
    sprintf(
      "Confusion Matrix (%s vs %s)",
      x$meta$test_var,
      x$meta$ref_var
    ),
    sprintf(
      "Test + (%s): TP = %d, FP = %d",
      x$meta$positive$test,
      x$data$confusion_matrix[1, 1],
      x$data$confusion_matrix[1, 2]
    ),
    sprintf(
      "Test - (%s): FN = %d, TN = %d",
      x$meta$negative$test,
      x$data$confusion_matrix[2, 1],
      x$data$confusion_matrix[2, 2]
    )
  )

  ft <- flextable::add_header_lines(ft, values = matrix_lines)
  ft <- flextable::align(ft, align = "left", part = "header")
  ft <- flextable::align(ft, j = 1, align = "left", part = "body")
  ft <- flextable::align(ft, j = 2:3, align = "center", part = "body")

  ft <- .flex_add_footnotes(ft, footnotes)
  .diag_style_spec(x)$flex(ft)
}

#########
# GRAPHICAL VISUALIZATION
# Fourfold displays and ggplot2 metric forest/heatmap diagnostic plots.

#' Plot diagnostic test results
#'
#' Draws a fourfold display of the retained confusion matrix with sensitivity and
#' specificity annotated on the bottom margin.
#'
#' @param x A `simtab_diag` result.
#' @param col Character vector of length 2. Fill colours for the negative and
#'   positive quadrants respectively. Default: `c("#ffcccc", "#ccffcc")`.
#' @param main Character. Plot title. Default: `"Confusion Matrix"`.
#' @param ... Additional arguments passed to [graphics::fourfoldplot()].
#' @return Invisibly returns `x`.
#' @examples
#' d <- diag_test(epitabl, poc_hstn_positive, adjudicated_acs,
#'                positive = "Yes", test_positive = "Positive")
#' plot(d)
#' @export
plot.simtab_diag <- function(
    x,
    col = c("#ffcccc", "#ccffcc"),
    main = "Confusion Matrix",
    ...
) {
  x <- .diag_validate_result(x)
  metrics <- x$data$metrics
  sens <- metrics["sensitivity", "estimate"]
  spec <- metrics["specificity", "estimate"]

  graphics::fourfoldplot(
    x$data$confusion_matrix,
    color = col,
    conf.level = 0,
    margin = 1L,
    main = main,
    ...
  )
  graphics::mtext(
    sprintf("Sensitivity: %.2f   |   Specificity: %.2f", sens, spec),
    side = 1L,
    line = 1L
  )

  invisible(x)
}

#' Plot a diagnostic accuracy result with ggplot2
#'
#' `type = "matrix"` draws the retained confusion matrix as a labelled heatmap;
#' `type = "metrics"` draws accuracy measures as points with their stored
#' confidence intervals. Both read `$data` only - no accuracy measure, interval,
#' or count is recomputed at plot time. Calibration curves are deliberately not
#' offered: a binary index test produces no risk scale to calibrate.
#'
#' @param object A computed `diag_test()` result.
#' @param type Either `"matrix"` (default) or `"metrics"`.
#' @param metrics Character vector of metric rows to draw when
#'   `type = "metrics"`. Defaults to the interval-carrying accuracy measures.
#' @param ... Unused.
#' @return A `ggplot` object carrying recommended export dimensions.
#' @keywords internal
#' @noRd
.diag_autoplot <- function(object,
                           type = c("matrix", "metrics"),
                           metrics = c("sensitivity", "specificity", "ppv", "npv", "accuracy"),
                           ...) {
  object <- .diag_validate_result(object)
  .require_pkg("ggplot2", "autoplot() on a diagnostic result")
  type <- match.arg(type)

  if (identical(type, "matrix")) {
    .diag_autoplot_matrix(object)
  } else {
    .diag_autoplot_metrics(object, metrics)
  }
}

#' Render ggplot2 tile heatmap of diagnostic confusion matrix
#' @keywords internal
#' @noRd
.diag_autoplot_matrix <- function(object) {
  cm <- object$data$confusion_matrix
  frame <- as.data.frame(as.table(cm))
  names(frame) <- c("test", "reference", "n")

  # Correct classifications sit on the matrix diagonal: positive/positive and
  # negative/negative. Shading them apart from the errors is the whole point of
  # showing the matrix rather than a table of four numbers.
  test_levels <- rownames(cm)
  ref_levels <- colnames(cm)
  frame$correct <- (frame$test == test_levels[[1]] & frame$reference == ref_levels[[1]]) |
    (frame$test == test_levels[[2]] & frame$reference == ref_levels[[2]])
  frame$test <- factor(frame$test, levels = rev(test_levels))
  frame$reference <- factor(frame$reference, levels = ref_levels)

  plot <- ggplot2::ggplot(
    frame,
    ggplot2::aes(x = .data[["reference"]], y = .data[["test"]])
  ) +
    ggplot2::geom_tile(
      ggplot2::aes(fill = .data[["correct"]]),
      colour = "white",
      linewidth = 1.2,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data[["n"]]),
      size = 5,
      fontface = "bold"
    ) +
    ggplot2::scale_fill_manual(values = c("TRUE" = "#cfe6d4", "FALSE" = "#f2d5d5")) +
    ggplot2::labs(
      x = sprintf("Reference standard (%s)", object$meta$ref_var %||% "reference"),
      y = sprintf("Index test (%s)", object$meta$test_var %||% "test"),
      title = "Confusion Matrix",
      subtitle = .diag_accuracy_subtitle(object)
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_text(size = 9, colour = "grey30")
    )

  .with_export_dim(plot, width = 5.5, height = 4.5)
}

#' Render ggplot2 point-range forest plot of accuracy metrics
#' @keywords internal
#' @noRd
.diag_autoplot_metrics <- function(object, metrics) {
  mat <- object$data$metrics
  keep <- intersect(metrics, rownames(mat))
  if (length(keep) == 0) {
    simtab_abort_input(c(
      "None of the requested metrics are available on this result.",
      "i" = "Requested: {.val {metrics}}.",
      "v" = "Choose from {.val {rownames(mat)}}."
    ))
  }

  labels <- object$meta$metric_labels %||% .diag_metric_labels()
  frame <- data.frame(
    metric = keep,
    label = unname(labels[keep]),
    estimate = as.numeric(mat[keep, "estimate"]),
    conf.low = as.numeric(mat[keep, "conf.low"]),
    conf.high = as.numeric(mat[keep, "conf.high"])
  )
  frame$label <- factor(frame$label, levels = rev(frame$label))

  plot <- ggplot2::ggplot(
    frame,
    ggplot2::aes(x = .data[["estimate"]], y = .data[["label"]])
  ) +
    ggplot2::geom_pointrange(
      ggplot2::aes(xmin = .data[["conf.low"]], xmax = .data[["conf.high"]]),
      orientation = "y",
      linewidth = 0.5,
      size = 0.45,
      na.rm = TRUE
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
    ggplot2::labs(
      x = sprintf("Estimate (%d%% CI)", as.integer(object$meta$conf_pct %||% 95)),
      y = NULL,
      title = "Diagnostic Accuracy",
      subtitle = .diag_accuracy_subtitle(object)
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_text(size = 9, colour = "grey30")
    )

  .with_export_dim(plot, width = 6.5, height = 1.6 + 0.45 * nrow(frame))
}

#' Format sensitivity, specificity, and sample size for plot subtitles
#' @keywords internal
#' @noRd
.diag_accuracy_subtitle <- function(object) {
  style <- .diag_style_spec(object)
  mat <- object$data$metrics
  digits <- object$spec$fmt$d %||% style$digits_est
  fmt <- function(row) sprintf(paste0("%.", digits, "f"), as.numeric(mat[row, "estimate"]))
  sprintf(
    "Sensitivity %s | Specificity %s | N = %d",
    fmt("sensitivity"),
    fmt("specificity"),
    as.integer(object$meta$sample_size %||% sum(object$data$confusion_matrix))
  )
}

#########
# RENDERER REGISTRATION AND SPECIFICATION VALIDATION
# Engine renderer bindings and specification contract checks for diagnostic evaluations.

#' Dispatch list of renderer functions for diagnostic accuracy results
#' @keywords internal
#' @noRd
.diag_renderers <- function() {
  list(
    print = .diag_print,
    as_data_frame = .diag_as_data_frame,
    as_flextable = .diag_as_flextable,
    autoplot = .diag_autoplot,
    as_methods = .methods_as_diag
  )
}

#' Validate required role bindings in diagnostic specification before computation
#' @keywords internal
#' @noRd
.validate_accuracy <- function(spec) {
  test_var <- .resolve_single_role(spec, "test")
  ref_var <- .resolve_single_role(spec, "ref_std")

  if (is.null(test_var)) {
    simtab_abort_engine(c(
      "An accuracy spec requires a {.val test} role.",
      "i" = "No index-test column was recorded before compute.",
      "v" = "Use {.code diag_test(data, test = rapid, ref = gold)}."
    ))
  }
  if (is.null(ref_var)) {
    simtab_abort_engine(c(
      "An accuracy spec requires a {.val ref} role.",
      "i" = "No reference-standard column was recorded before compute.",
      "v" = "Use {.code diag_test(data, test = rapid, ref = gold)}."
    ))
  }

  invisible(spec)
}
