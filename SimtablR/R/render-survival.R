# COX SURVIVAL MODEL RESULT RENDERERS
# Formats hazard ratios, Wald confidence intervals, Schoenfeld diagnostics,
# console summaries, and flextables for survtab survival results.

#########
# SURVIVAL DISPLAY MATRIX FORMATTING
# Extracts tidy parameter estimates and publication-ready hazard ratio display tables.

#' Validate that result inherits the simtab_cox class
#' @keywords internal
#' @noRd
.cox_validate_result <- function(x) {
  x <- validate_simtab_result(x)
  if (!inherits(x, "simtab_cox")) {
    simtab_abort_input(c(
      "{.arg x} must be a {.fn survtab} result.",
      "i" = "Received an object of class {.cls {class(x)[[1]]}}.",
      "v" = "Build one with {.code survtab(data, time = t, event = e, predictors = ~ x)}."
    ))
  }
  x
}

#' Format numeric survival parameter estimates to fixed decimal precision
#' @keywords internal
#' @noRd
.surv_fmt_num <- function(x, digits) {
  ifelse(is.na(x), "", sprintf(paste0("%.", digits, "f"), x))
}

#' Coerce Cox model result to tidy data frame of hazard ratio estimates
#' @keywords internal
#' @noRd
.cox_tidy <- function(x, ...) {
  x <- .cox_validate_result(x)
  data.frame(
    term = x$data$terms$term,
    estimate = x$data$terms$estimate,
    conf.low = x$data$terms$lower,
    conf.high = x$data$terms$upper,
    p.value = x$data$terms$p
  )
}

#' Coerce Cox model result to formatted display or tidy data frame
#' @keywords internal
#' @noRd
.cox_as_data_frame <- function(x, row.names = NULL, optional = FALSE, tidy = FALSE, ...) {
  x <- .cox_validate_result(x)
  if (isTRUE(tidy)) {
    return(.cox_tidy(x, ...))
  }
  d <- x$meta$d %||% 2L
  labels <- x$spec$fmt$labels %||% NULL
  # "default" keeps the established survtab text (", " separator, `d` digits).
  journal <- .result_journal(x$meta$style %||% x$spec$style, legacy = "default")
  hr_text <- if (is.null(journal)) {
    paste0(
      .surv_fmt_num(x$data$terms$estimate, d), " (",
      .surv_fmt_num(x$data$terms$lower, d), ", ",
      .surv_fmt_num(x$data$terms$upper, d), ")"
    )
  } else {
    vapply(
      seq_len(nrow(x$data$terms)),
      function(i) .fmt_est(x$data$terms$estimate[i], x$data$terms$lower[i], x$data$terms$upper[i], journal),
      character(1)
    )
  }
  data.frame(
    Term = vapply(
      x$data$terms$term,
      .simtab_model_term_label,
      character(1),
      data = x$spec$data_src$ref$data,
      labels = labels
    ),
    `HR (95% CI)` = hr_text,
    `p-value` = vapply(
      x$data$terms$p,
      function(p) .fmt_p(p, .resolve_table_style(x$meta$style %||% "default")),
      character(1)
    ),
    check.names = FALSE
  )
}

#########
# ENGINE RENDERER DICTIONARY AND CONTRACT METHODS
# Implements console print, glance, flextable, and methods sentence renderers for Cox models.

#' Print formatted Cox model summary table and advice to console
#' @keywords internal
#' @noRd
.cox_print <- function(x, ...) {
  x <- .cox_validate_result(x)
  cat("\nCox Proportional Hazards Model\n")
  cat("==============================\n")
  cat(sprintf("Time: %s | Event: %s | Events: %d/%d\n", x$meta$time, x$meta$event, x$meta$n_events, x$meta$n))
  if (!is.null(x$meta$strata)) {
    cat(sprintf("Stratified by: %s (separate baseline hazards)\n", x$meta$strata))
  }
  cat("\n")
  print(noquote(as.data.frame(x)), row.names = FALSE)
  .print_advice(x)
  invisible(x)
}

#' Construct one-row model summary glance data frame for Cox regression
#' @keywords internal
#' @noRd
.cox_glance <- function(x, ...) {
  x <- .cox_validate_result(x)
  data.frame(
    n = x$meta$n,
    events = x$meta$n_events,
    concordance = x$meta$concordance,
    lr_p = x$meta$lr_p,
    ties = x$meta$ties
  )
}

#' Render publication-styled flextable for Cox survival results
#' @keywords internal
#' @noRd
.cox_as_flextable <- function(x, footnotes = NULL, ...) {
  x <- .cox_validate_result(x)
  .require_pkg("flextable")
  ft <- flextable::flextable(as.data.frame(x), ...)
  ft <- flextable::align(ft, align = "center", part = "all")
  ft <- flextable::align(ft, j = 1, align = "left", part = "body")
  ft <- .flex_add_footnotes(ft, footnotes)
  .resolve_table_style(x$meta$style %||% "default")$flex(ft)
}

#' Synthesize manuscript methods sentence for Cox survival analysis
#' @keywords internal
#' @noRd
.cox_as_methods <- function(x, ...) {
  x <- .cox_validate_result(x)
  sprintf(
    "Time-to-event associations were estimated with Cox proportional hazards models (Cox, 1972)%s; hazard ratios with %d%% Wald CI were reported and proportional hazards were assessed with the Grambsch-Therneau Schoenfeld residual test (1994).",
    if (is.null(x$meta$strata)) "" else sprintf(", stratified by %s", x$meta$strata),
    x$meta$conf_pct %||% 95
  )
}

#' Engine vtable renderer dictionary for Cox survival models
#' @keywords internal
#' @noRd
.cox_renderers <- function() {
  list(
    print = .cox_print,
    as_data_frame = .cox_as_data_frame,
    tidy = .cox_tidy,
    glance = .cox_glance,
    as_flextable = .cox_as_flextable,
    autoplot = .forest_plot,
    as_methods = .cox_as_methods
  )
}
