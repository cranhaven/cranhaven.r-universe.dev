# ROC CURVE AND AUC RESULT RENDERERS
# Formats empirical ROC coordinates, AUC summaries, optimal cutpoint tables,
# ggplot2 diagnostic curves, and paired DeLong contrasts.

#########
# ROC DISPLAY MATRIX FORMATTING
# Extracts tidy and formatted rectangular displays for AUC and diagnostic cutpoints.

#' Validate that result inherits the simtab_roc class
#' @keywords internal
#' @noRd
.roc_validate_result <- function(x) {
  x <- validate_simtab_result(x)
  if (!inherits(x, "simtab_roc")) {
    simtab_abort_input(c(
      "{.arg x} must be a {.fn roc} result.",
      "i" = "Received an object of class {.cls {class(x)[[1]]}}.",
      "v" = "Build one with {.code roc(data, marker = value, outcome = event)}."
    ))
  }
  x
}

#' Resolve presentation journal style specification for ROC output
#' @keywords internal
#' @noRd
.roc_style_spec <- function(x) {
  .resolve_table_style(x$meta$style %||% x$spec$style %||% "default")
}

#' Format numeric values to specified decimal precision string
#' @keywords internal
#' @noRd
.roc_fmt_num <- function(x, digits) {
  ifelse(is.na(x), "", sprintf(paste0("%.", digits, "f"), x))
}

#' Construct unformatted tidy data frame merging AUC and cutpoint metrics
#' @keywords internal
#' @noRd
.roc_tidy_frame <- function(x) {
  auc <- x$data$auc
  cut <- x$data$cutpoints
  if (is.data.frame(cut) && nrow(cut) > 0) {
    out <- merge(auc, cut, by = "marker", all.x = TRUE, sort = FALSE)
    out <- out[match(auc$marker, out$marker), , drop = FALSE]
  } else {
    out <- auc
    out$threshold <- NA_real_
    out$sensitivity <- NA_real_
    out$specificity <- NA_real_
    out$ppv <- NA_real_
    out$npv <- NA_real_
    out$youden_j <- NA_real_
  }
  rownames(out) <- NULL
  out[c(
    "marker", "auc", "conf.low", "conf.high", "ci_method",
    "n", "n_pos", "n_neg", "direction_used", "threshold",
    "sensitivity", "specificity", "ppv", "npv", "youden_j"
  )]
}

#' Map marker/outcome column names to publication labels set with label()
#' @keywords internal
#' @noRd
.roc_display_names <- function(x, names) {
  labels <- x$spec$fmt$labels %||% character()
  out <- as.character(names)
  hit <- !is.na(out) & out %in% names(labels)
  out[hit] <- unname(labels[out[hit]])
  out
}

#' Assemble formatted publication display data frame for ROC results
#' @keywords internal
#' @noRd
.roc_display_frame <- function(x) {
  x <- .roc_validate_result(x)
  spec <- .roc_style_spec(x)
  # `spec` here is the resolved *style*; `x$spec` is the simtab_spec.
  d <- x$spec$fmt$d %||% spec$digits_est
  as_percent <- isTRUE(x$meta$percent)
  fmt_prop <- function(v) {
    if (as_percent) {
      ifelse(is.na(v), "", paste0(sprintf(paste0("%.", d, "f"), v * 100), "%"))
    } else {
      .roc_fmt_num(v, d)
    }
  }
  tidy <- .roc_tidy_frame(x)
  ci_label <- sprintf("%s%% CI", x$meta$conf_pct)

  data.frame(
    Marker = .roc_display_names(x, tidy$marker),
    `AUC (95% CI)` = paste0(
      .roc_fmt_num(tidy$auc, d),
      " (",
      .roc_fmt_num(tidy$conf.low, d),
      spec$ci_sep,
      .roc_fmt_num(tidy$conf.high, d),
      ")"
    ),
    Direction = tidy$direction_used,
    Cutpoint = .roc_fmt_num(tidy$threshold, d),
    Sensitivity = fmt_prop(tidy$sensitivity),
    Specificity = fmt_prop(tidy$specificity),
    PPV = fmt_prop(tidy$ppv),
    NPV = fmt_prop(tidy$npv),
    check.names = FALSE
  ) |>
    stats::setNames(c(
      "Marker", sprintf("AUC (%s)", ci_label), "Direction", "Cutpoint",
      "Sensitivity", "Specificity", "PPV", "NPV"
    ))
}

#########
# ENGINE RENDERER DICTIONARY AND CONTRACT METHODS
# Implements console print, data frame coercion, tidy, glance, and flextable renderers.

#' Print formatted ROC curve summaries and DeLong comparisons to console
#' @keywords internal
#' @noRd
.roc_print <- function(x, ...) {
  x <- .roc_validate_result(x)
  cat("\nROC Curve Analysis\n")
  cat("==================\n")
  cat(sprintf("Outcome: %s (positive = '%s')\n", .roc_display_names(x, x$meta$outcome), x$meta$positive))
  cat(sprintf("CI method: %s | Direction: %s\n\n", x$meta$ci, x$meta$direction))
  print(noquote(as.data.frame(x)), row.names = FALSE)

  comps <- x$data$comparisons
  if (is.data.frame(comps) && nrow(comps) > 0) {
    spec <- .roc_style_spec(x)
    d <- x$spec$fmt$d %||% spec$digits_est
    cat("\nPaired DeLong comparisons\n")
    cat("-------------------------\n")
    for (i in seq_len(nrow(comps))) {
      cat(sprintf(
        "%s: AUC diff %s, z %s, p %s\n",
        if (all(c("marker_1", "marker_2") %in% names(comps))) {
          paste(
            .roc_display_names(x, comps$marker_1[i]), "vs",
            .roc_display_names(x, comps$marker_2[i])
          )
        } else {
          comps$pair[i]
        },
        .roc_fmt_num(comps$auc_diff[i], d),
        .roc_fmt_num(comps$z[i], d),
        .fmt_p(comps$p.value[i], spec)
      ))
    }
  }

  .print_advice(x)
  invisible(x)
}

#' Coerce ROC result to display or tidy data frame
#' @keywords internal
#' @noRd
.roc_as_data_frame <- function(x, row.names = NULL, optional = FALSE, tidy = FALSE, ...) {
  x <- .roc_validate_result(x)
  if (isTRUE(tidy)) {
    return(.roc_tidy_frame(x))
  }
  .roc_display_frame(x)
}

#' Coerce ROC result to tidy data frame
#' @keywords internal
#' @noRd
.roc_tidy <- function(x, ...) {
  as.data.frame(x, tidy = TRUE, ...)
}

#' Construct one-row summary glance data frame for ROC analysis
#' @keywords internal
#' @noRd
.roc_glance <- function(x, ...) {
  x <- .roc_validate_result(x)
  auc <- x$data$auc
  data.frame(
    n = unique(auc$n)[1],
    n_pos = unique(auc$n_pos)[1],
    n_neg = unique(auc$n_neg)[1],
    markers = nrow(auc),
    ci_method = x$meta$ci,
    cutpoint = x$meta$cutpoint,
    comparisons = nrow(x$data$comparisons)
  )
}

#' Synthesize manuscript methods sentence for ROC analysis
#' @keywords internal
#' @noRd
.roc_as_methods <- function(x, ...) {
  x <- .roc_validate_result(x)
  ci <- x$meta$conf_pct %||% round((x$meta$conf.level %||% 0.95) * 100)
  markers <- paste(x$meta$marker %||% x$data$auc$marker, collapse = ", ")
  ci_sentence <- sprintf("%d%% DeLong confidence intervals were reported for AUC (DeLong et al. 1988).", ci)
  cutpoint_sentence <- if (!identical(x$meta$cutpoint %||% "youden", "none")) {
    "Data-driven Youden cutpoints were reported; these can be optimistic when selected and evaluated in the same data (Ewald 2006)."
  } else {
    "No data-driven cutpoint was reported."
  }
  comparison_sentence <- if (is.data.frame(x$data$comparisons) && nrow(x$data$comparisons) > 0) {
    "Paired marker AUC comparisons used DeLong's test (DeLong et al. 1988)."
  } else {
    ""
  }

  paste(
    sprintf("ROC analysis for %s estimated AUC using pROC (Robin et al. 2011).", markers),
    ci_sentence,
    cutpoint_sentence,
    comparison_sentence
  )
}

#' Render publication-styled flextable for ROC results
#' @keywords internal
#' @noRd
.roc_as_flextable <- function(x, footnotes = NULL, ...) {
  x <- .roc_validate_result(x)
  .require_pkg("flextable")

  df <- as.data.frame(x)
  ft <- flextable::flextable(df, ...)
  ft <- flextable::add_header_lines(
    ft,
    values = sprintf("ROC analysis: %s positive = '%s'", x$meta$outcome, x$meta$positive)
  )
  ft <- flextable::align(ft, align = "center", part = "all")
  ft <- flextable::align(ft, j = 1, align = "left", part = "body")
  ft <- .flex_add_footnotes(ft, footnotes)
  .roc_style_spec(x)$flex(ft)
}

#########
# GRAPHICAL VISUALIZATION AND SPECIFICATION VALIDATION
# Implements ggplot2 autoplot for ROC curves and validates required specification roles.

#' Generate ggplot2 ROC curve with sensitivity, specificity, and cutpoints
#' @keywords internal
#' @noRd
.roc_autoplot <- function(object, ...) {
  object <- .roc_validate_result(object)
  .require_pkg("ggplot2", "autoplot() on a roc result")

  curve <- object$data$curve
  curve$false_positive_rate <- 1 - curve$specificity
  plot <- ggplot2::ggplot(
    curve,
    ggplot2::aes(
      x = .data$false_positive_rate,
      y = .data$sensitivity,
      colour = .data$marker,
      linetype = .data$marker
    )
  ) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey60") +
    ggplot2::geom_path(linewidth = 0.9) +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    ggplot2::scale_colour_viridis_d(end = 0.85) +
    ggplot2::labs(
      x = "1 - specificity",
      y = "Sensitivity",
      colour = "Marker",
      linetype = "Marker",
      title = "ROC Curve"
    ) +
    ggplot2::theme_minimal()

  cut <- object$data$cutpoints
  if (is.data.frame(cut) && nrow(cut) > 0) {
    cut$false_positive_rate <- 1 - cut$specificity
    plot <- plot +
      ggplot2::geom_point(
        data = cut,
        ggplot2::aes(x = .data$false_positive_rate, y = .data$sensitivity, colour = .data$marker),
        inherit.aes = FALSE,
        size = 2
      )
  }

  plot
}

#' Autoplot a computed SimtablR result
#'
#' Dispatches to the engine's registered `autoplot` renderer.
#'
#' @param object A `simtab_result`.
#' @param ... Passed to the engine's autoplot renderer.
#' @return A `ggplot` object.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   r <- roc(epitabl, poc_hstn_value, adjudicated_acs)
#'   ggplot2::autoplot(r)
#' }
#' @exportS3Method ggplot2::autoplot
autoplot.simtab_result <- function(object, ...) {
  object <- validate_simtab_result(object)
  fn <- .engine_renderer(object, "autoplot")
  if (is.null(fn)) {
    simtab_abort_render(c(
      "No {.fn autoplot} renderer is registered for engine
       {.val {object$meta$engine %||% '?'}}.",
      "i" = "Plotting dispatches through the engine's registered renderers.",
      "v" = "Register one with
             {.code register_engine(..., renderers = list(autoplot = ...))}."
    ))
  }
  fn(object, ...)
}

#' Plot a computed SimtablR spec
#'
#' @param object A `simtab_spec`.
#' @param ... Passed to the computed result's autoplot method.
#' @return A `ggplot` object when the computed result supports autoplot.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   sp <- regtab(epitabl, "adjudicated_acs", ~ age + sex, family = binomial())$spec
#'   ggplot2::autoplot(sp)
#' }
#' @exportS3Method ggplot2::autoplot
autoplot.simtab_spec <- function(object, ...) {
  .require_pkg("ggplot2", "autoplot()")
  ggplot2::autoplot(evaluate(object), ...)
}

#' Engine vtable renderer dictionary for ROC analysis
#' @keywords internal
#' @noRd
.roc_renderers <- function() {
  list(
    print = .roc_print,
    as_data_frame = .roc_as_data_frame,
    tidy = .roc_tidy,
    glance = .roc_glance,
    as_methods = .roc_as_methods,
    as_flextable = .roc_as_flextable,
    autoplot = .roc_autoplot
  )
}

#' Validate required marker and outcome roles in ROC specification
#' @keywords internal
#' @noRd
.validate_roc <- function(spec) {
  markers <- .resolve_tidyselect_role(spec, "describe")
  outcome <- .resolve_single_role(spec, "ref_std")

  if (length(markers) == 0) {
    simtab_abort_engine(c(
      "A ROC spec requires at least one {.val marker} role.",
      "i" = "No marker column was recorded before compute.",
      "v" = "Use {.code roc(data, marker = marker, outcome = outcome)}."
    ))
  }
  if (is.null(outcome)) {
    simtab_abort_engine(c(
      "A ROC spec requires an {.val outcome} role.",
      "i" = "No outcome/reference column was recorded before compute.",
      "v" = "Use {.code roc(data, marker = marker, outcome = outcome)}."
    ))
  }

  invisible(spec)
}
