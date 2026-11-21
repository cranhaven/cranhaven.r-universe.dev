# Forest Plot Visualization
#
# Forest plot construction and dimension calculation for effect-carrying
# SimtablR results across regression, survival, and bivariate tables.

#########
# DATA FRAME EXTRACTION FOR FOREST PLOTS
# Extract panel, label, estimate, and confidence limits across result types.

#' Create empty schema-conforming forest plot data frame
#' @keywords internal
#' @noRd
.empty_effect_forest_frame <- function() {
  data.frame(
    panel = character(),
    label = character(),
    estimate = numeric(),
    conf.low = numeric(),
    conf.high = numeric(),
    p.value = numeric(),
    is_reference = logical()
  )
}

#' Dispatch forest plot data frame extraction across SimtablR result classes
#' @keywords internal
#' @noRd
.effect_forest_frame <- function(x) {
  x <- validate_simtab_result(x)

  if (inherits(x, "simtab_regtab")) {
    return(.effect_forest_frame_regtab(x))
  }
  if (inherits(x, "simtab_tb")) {
    return(.effect_forest_frame_tb(x))
  }
  if (inherits(x, "simtab_table1")) {
    return(.effect_forest_frame_table1(x))
  }
  if (inherits(x, "simtab_cox")) {
    return(.effect_forest_frame_cox(x))
  }

  .empty_effect_forest_frame()
}

#' Extract forest plot data frame from regression result
#' @keywords internal
#' @noRd
.effect_forest_frame_regtab <- function(x) {
  tidy <- generics::tidy(x)
  if (nrow(tidy) == 0) {
    return(.empty_effect_forest_frame())
  }
  .new_effect_forest_frame(
    panel = tidy$outcome,
    label = tidy$term,
    estimate = tidy$estimate,
    conf.low = tidy$conf.low,
    conf.high = tidy$conf.high,
    p.value = tidy$p.value,
    is_reference = rep(FALSE, nrow(tidy))
  )
}

#' Extract forest plot data frame from bivariate table result
#' @keywords internal
#' @noRd
.effect_forest_frame_tb <- function(x) {
  # Read the stored ratio evidence directly: one row per exposure level (or per
  # stratum and the pooled estimate). The tidy frame repeats each ratio under
  # every outcome level, which would draw the same estimate in a panel for the
  # non-event outcome.
  ratios <- x$data$mh %||% x$data$ratios
  if (!is.data.frame(ratios) || nrow(ratios) == 0 ||
      !all(c("estimate", "lower_ci", "upper_ci") %in% names(ratios))) {
    return(.empty_effect_forest_frame())
  }
  ratios <- ratios[!is.na(ratios$estimate), , drop = FALSE]
  if (nrow(ratios) == 0) {
    return(.empty_effect_forest_frame())
  }
  label_base <- x$meta$row_label %||% ratios$variable
  label <- paste(label_base, ratios$level)
  if (!is.null(ratios$stratum)) {
    label <- paste0(label, " (", ratios$stratum, ")")
  }
  ref <- ratios$ref %in% TRUE
  .new_effect_forest_frame(
    panel = rep(x$meta$col_label %||% "Effect", nrow(ratios)),
    label = label,
    estimate = ratios$estimate,
    conf.low = ratios$lower_ci,
    conf.high = ratios$upper_ci,
    p.value = ratios$p_value %||% rep(NA_real_, nrow(ratios)),
    is_reference = ref | (ratios$estimate == 1 & is.na(ratios$lower_ci) & is.na(ratios$upper_ci))
  )
}

#' Extract forest plot data frame from descriptive Table 1 result
#' @keywords internal
#' @noRd
.effect_forest_frame_table1 <- function(x) {
  meta <- x$meta
  if (is.null(meta$effect)) {
    return(.empty_effect_forest_frame())
  }

  parts <- list()
  for (v in meta$vars %||% names(x$data)) {
    rec <- x$data[[v]]
    if (is.null(rec)) {
      next
    }
    effect <- rec$adjusted %||% rec$crude
    if (is.null(effect) || nrow(effect) == 0) {
      next
    }
    label_base <- unname((meta$labels %||% character())[v])
    if (is.na(label_base) || !nzchar(label_base)) {
      label_base <- v
    }
    level <- effect$level
    label <- ifelse(is.na(level) | !nzchar(level), label_base, paste(label_base, level))
    ref <- effect$ref %||% rep(FALSE, nrow(effect))
    parts[[length(parts) + 1L]] <- .new_effect_forest_frame(
      panel = rep("Effect", nrow(effect)),
      label = label,
      estimate = effect$estimate,
      conf.low = ifelse(ref, NA_real_, effect$lower),
      conf.high = ifelse(ref, NA_real_, effect$upper),
      p.value = effect$p %||% rep(NA_real_, nrow(effect)),
      is_reference = ref | (!is.na(effect$estimate) & effect$estimate == 1 & is.na(effect$lower) & is.na(effect$upper))
    )
  }

  if (length(parts) == 0) {
    return(.empty_effect_forest_frame())
  }
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out
}

#' Extract forest plot data frame from Cox proportional hazards result
#' @keywords internal
#' @noRd
.effect_forest_frame_cox <- function(x) {
  tidy <- generics::tidy(x)
  if (nrow(tidy) == 0) {
    return(.empty_effect_forest_frame())
  }
  .new_effect_forest_frame(
    panel = rep("Model", nrow(tidy)),
    label = tidy$term,
    estimate = tidy$estimate,
    conf.low = tidy$conf.low,
    conf.high = tidy$conf.high,
    p.value = tidy$p.value,
    is_reference = rep(FALSE, nrow(tidy))
  )
}

#' Construct standard forest plot data frame with typed columns
#' @keywords internal
#' @noRd
.new_effect_forest_frame <- function(panel, label, estimate, conf.low, conf.high, p.value, is_reference) {
  data.frame(
    panel = as.character(panel),
    label = as.character(label),
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    p.value = as.numeric(p.value),
    is_reference = as.logical(is_reference)
  )
}

#########
# GGPLOT2 FOREST PLOT BUILDER
# Build point-range visualization, scale coordinates, and calculate canvas height.

#' Build ggplot2 forest plot with point-ranges and reference guidelines
#' @keywords internal
#' @noRd
.forest_plot <- function(x, ...) {
  .require_pkg("ggplot2", "autoplot()")

  frame <- .effect_forest_frame(x)
  if (nrow(frame) == 0) {
    simtab_abort_input(c(
      "No effect estimates are available to plot.",
      "i" = "The result carries no estimate column, or every estimate is missing.",
      "v" = "Add {.code measure = \"OR\"} (or another effect measure) before plotting."
    ))
  }

  scale <- .forest_scale(x)
  frame <- frame[!is.na(frame$estimate), , drop = FALSE]
  if (identical(scale$type, "log")) {
    # A ratio at or below zero cannot be placed on a log axis; a difference can.
    frame <- frame[frame$estimate > 0, , drop = FALSE]
  }
  if (nrow(frame) == 0) {
    simtab_abort_input(c(
      "No plottable effect estimates remain.",
      "i" = "A {.val {scale$measure}} forest plot uses a log axis, which cannot show non-positive estimates.",
      "v" = "Check the fitted model, or plot a difference-scale measure instead."
    ))
  }
  frame$label <- factor(frame$label, levels = rev(unique(frame$label)))
  frame$panel <- factor(frame$panel, levels = unique(frame$panel))

  plot <- ggplot2::ggplot(frame, ggplot2::aes(x = .data[["estimate"]], y = .data[["label"]])) +
    ggplot2::geom_vline(xintercept = scale$ref, linetype = 2, linewidth = 0.4, colour = "grey45") +
    ggplot2::geom_pointrange(
      data = frame[!frame$is_reference, , drop = FALSE],
      ggplot2::aes(
        xmin = .data[["conf.low"]],
        xmax = .data[["conf.high"]]
      ),
      orientation = "y",
      na.rm = TRUE,
      linewidth = 0.45,
      size = 0.45
    ) +
    ggplot2::geom_point(
      data = frame[frame$is_reference, , drop = FALSE],
      size = 1.8,
      na.rm = TRUE
    ) +
    ggplot2::labs(
      x = .forest_axis_label(x),
      y = NULL,
      title = .forest_title(x)
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      axis.text.y = ggplot2::element_text(hjust = 1)
    )

  if (identical(scale$type, "log")) {
    ranges <- c(frame$estimate, frame$conf.low, frame$conf.high)
    ranges <- ranges[is.finite(ranges) & ranges > 0]
    plot <- plot + ggplot2::scale_x_log10(breaks = .forest_log_breaks(ranges))
  }

  n_panels <- length(unique(frame$panel))
  if (n_panels > 1) {
    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(.data[["panel"]]), scales = "free_y")
  }

  .with_export_dim(
    plot,
    width = 7,
    height = .forest_export_height(nrow(frame), n_panels)
  )
}

#' Determine coordinate scale and reference intercept for effect measure
#' @keywords internal
#' @noRd
.forest_scale <- function(x) {
  measure <- .forest_measure(x)
  ratio <- c("OR", "RR", "PR", "HR", "IRR", "SR", "RATIO")
  type <- if (toupper(measure) %in% ratio) "log" else "linear"
  list(
    type = type,
    ref = if (identical(type, "log")) 1 else 0,
    measure = measure
  )
}

#' Resolve display label for effect measure in forest plot
#' @keywords internal
#' @noRd
.forest_measure <- function(x) {
  if (inherits(x, "simtab_cox")) {
    return("HR")
  }
  if (inherits(x, "simtab_regtab")) {
    if (!isTRUE(x$meta$exponentiate)) {
      return("Estimate")
    }
    return(switch(
      x$meta$family %||% "",
      binomial = "OR",
      poisson = "IRR",
      "Estimate"
    ))
  }
  x$spec$effect$measure %||% x$meta$effect %||% "Effect estimate"
}

#' Calculate calibrated canvas height in inches for forest plot export
#' @keywords internal
#' @noRd
.forest_export_height <- function(n_rows, n_panels = 1L) {
  rows_per_panel <- if (n_panels > 1) ceiling(n_rows / n_panels) else n_rows
  base <- 1.6 + 0.28 * rows_per_panel
  if (n_panels > 1) {
    base <- base * min(n_panels, 3L)
  }
  max(3, min(base, 20))
}

#' Calculate logarithmic tick mark breaks for ratio scale
#' @keywords internal
#' @noRd
.forest_log_breaks <- function(x) {
  candidates <- c(0.05, 0.1, 0.25, 0.5, 1, 2, 4, 10, 20)
  if (length(x) == 0) {
    return(c(0.5, 1, 2))
  }
  lo <- min(x, na.rm = TRUE)
  hi <- max(x, na.rm = TRUE)
  out <- candidates[candidates >= lo / 1.2 & candidates <= hi * 1.2]
  if (!1 %in% out) {
    out <- sort(unique(c(out, 1)))
  }
  out
}

#' Format forest plot horizontal axis label with effect measure and CI level
#' @keywords internal
#' @noRd
.forest_axis_label <- function(x) {
  measure <- .forest_measure(x)
  ci <- x$meta$conf_pct %||% round((x$spec$effect$conf.level %||% 0.95) * 100)
  sprintf("%s (%d%% CI)", measure, as.integer(ci))
}

#' Return default title for forest plot
#' @keywords internal
#' @noRd
.forest_title <- function(x) {
  "Forest Plot"
}
