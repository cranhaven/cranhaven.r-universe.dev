# REGRESSION AND GLM RESULT RENDERERS
# Formats multi-outcome regression tables, confidence intervals, p-values,
# collinearity metrics, and console layouts for regtab results.

#########
# VARIABLE AND TERM LABEL FORMATTING
# Resolves outcome names, predictor interaction labels, and display strings.

#' Map outcome variable names to formatted publication display labels
#' @keywords internal
#' @noRd
.regtab_outcome_labels <- function(x) {
  meta <- x$meta
  data <- x$spec$data_src$ref$data
  successful <- meta$model_info$outcome[!meta$model_info$failed]
  stats::setNames(
    vapply(
      successful,
      function(outcome) {
        if (!is.null(meta$labels) && outcome %in% names(meta$labels)) {
          meta$labels[[outcome]]
        } else {
          .simtab_model_column_label(data, outcome)
        }
      },
      character(1)
    ),
    successful
  )
}

#' Map model predictor terms to formatted publication display labels
#' @keywords internal
#' @noRd
.regtab_term_labels <- function(x) {
  labels <- x$meta$predictor_labels
  # Labels set later through label()/fmt() win over constructor labels.
  fmt_labels <- x$spec$fmt$labels
  data <- x$spec$data_src$ref$data
  terms <- x$meta$term_order
  stats::setNames(
    vapply(
      terms,
      function(term) {
        if (!is.null(fmt_labels) && term %in% names(fmt_labels)) {
          unname(fmt_labels[[term]])
        } else if (!is.null(labels) && term %in% names(labels)) {
          labels[[term]]
        } else {
          .simtab_model_term_label(data, term, fmt_labels)
        }
      },
      character(1)
    ),
    terms
  )
}

#' Extract variable label from source data column attributes or overrides
#' @keywords internal
#' @noRd
.simtab_model_column_label <- function(data, variable, labels = NULL) {
  if (!is.null(labels) && variable %in% names(labels)) {
    return(unname(labels[[variable]]))
  }
  label <- attr(data[[variable]], "label", exact = TRUE)
  if (is.character(label) && length(label) == 1L && !is.na(label) && nzchar(label)) {
    return(label)
  }
  variable
}

#' Format regression model term and interaction symbols for publication display
#' @keywords internal
#' @noRd
.simtab_model_term_label <- function(data, term, labels = NULL) {
  if (!is.null(labels) && term %in% names(labels)) {
    return(unname(labels[[term]]))
  }
  if (identical(term, "(Intercept)")) {
    return(term)
  }
  components <- strsplit(term, ":", fixed = TRUE)[[1]]
  labelled <- vapply(components, function(component) {
    variables <- names(data)[order(nchar(names(data)), decreasing = TRUE)]
    for (variable in variables) {
      if (!identical(component, variable) && !startsWith(component, variable)) {
        next
      }
      suffix <- substr(component, nchar(variable) + 1L, nchar(component))
      values <- data[[variable]]
      levels <- if (is.factor(values)) levels(values) else unique(as.character(values[!is.na(values)]))
      if (!identical(component, variable) && (!nzchar(suffix) || !suffix %in% levels)) {
        next
      }
      label <- .simtab_model_column_label(data, variable, labels)
      if (identical(label, variable)) {
        return(component)
      }
      return(if (nzchar(suffix)) paste(label, suffix, sep = ": ") else label)
    }
    component
  }, character(1))
  paste(labelled, collapse = " \u00d7 ")
}

#########
# REGRESSION DISPLAY MATRIX CONSTRUCTION
# Assembles multi-outcome display frames, confidence intervals, p-values, and VIF metrics.

#' Format point estimate and confidence interval bounds into display string
#' @keywords internal
#' @noRd
.format_regtab_interval <- function(estimate, lower, upper, d) {
  fmt <- function(x) format(round(x, d), nsmall = d, trim = TRUE)
  bounds <- c(fmt(lower), fmt(upper))
  paste0(fmt(estimate), " (", bounds[1], .ci_sep_safe(" - ", bounds), bounds[2], ")")
}

#' Format regression p-value string with threshold formatting
#' @keywords internal
#' @noRd
.format_regtab_p <- function(p, d) {
  if (is.na(p)) {
    return("")
  }
  if (p < 0.001) {
    return("<0.001")
  }
  format(round(p, max(3L, d)), nsmall = max(3L, d), trim = TRUE)
}

#' Resolve the journal preset applied to a regression result
#'
#' `"default"` keeps the established regtab formatting (which honours `d`);
#' any other preset or `journal_style()` object, set via `style()` on the
#' result or on the specification, drives estimate/CI and p-value text.
#' @keywords internal
#' @noRd
.regtab_journal <- function(x) {
  .result_journal(x$meta$style %||% x$spec$style, legacy = "default")
}

#' Assemble formatted multi-outcome regression display data frame
#' @keywords internal
#' @noRd
.build_regtab_display <- function(x, vif = FALSE) {
  x <- validate_simtab_result(x)
  outcome_labels <- .regtab_outcome_labels(x)
  term_labels <- .regtab_term_labels(x)
  d <- x$meta$d %||% 2L
  journal <- .regtab_journal(x)
  fmt_interval <- if (is.null(journal)) {
    function(est, lo, hi) .format_regtab_interval(est, lo, hi, d)
  } else {
    function(est, lo, hi) .fmt_est(est, lo, hi, journal)
  }
  fmt_p <- if (is.null(journal)) {
    function(p) .format_regtab_p(p, d)
  } else {
    function(p) .fmt_p(p, journal)
  }

  display <- data.frame(
    Variable = unname(term_labels)
  )

  for (outcome in names(outcome_labels)) {
    label <- outcome_labels[[outcome]]
    rows <- x$data[x$data$outcome == outcome, , drop = FALSE]
    idx <- match(names(term_labels), rows$term)

    estimates <- rep("", length(term_labels))
    estimates[!is.na(idx)] <- vapply(
      idx[!is.na(idx)],
      function(i) fmt_interval(rows$estimate[i], rows$lower[i], rows$upper[i]),
      character(1)
    )
    display[[label]] <- estimates

    if (isTRUE(x$meta$p_values)) {
      p_col <- paste0(label, " p-value")
      p_values <- rep("", length(term_labels))
      p_values[!is.na(idx)] <- vapply(
        idx[!is.na(idx)],
        function(i) fmt_p(rows$p[i]),
        character(1)
      )
      display[[p_col]] <- p_values
    }

    if (isTRUE(vif) && "vif" %in% names(rows)) {
      vif_col <- paste0(label, " VIF")
      vif_values <- rep("", length(term_labels))
      vif_values[!is.na(idx)] <- vapply(
        idx[!is.na(idx)],
        function(i) {
          if (is.na(rows$vif[i])) {
            ""
          } else {
            format(round(rows$vif[i], d), nsmall = d, trim = TRUE)
          }
        },
        character(1)
      )
      display[[vif_col]] <- vif_values
    }
  }

  n_row <- display[1, , drop = FALSE]
  n_row[1, ] <- ""
  n_row$Variable <- "N"
  for (outcome in names(outcome_labels)) {
    label <- outcome_labels[[outcome]]
    n_row[[label]][1] <- as.character(x$meta$model_n[[outcome]])
  }

  rbind(display, n_row)
}

#########
# CONSOLE FORMATTING AND WIDTH MANAGEMENT
# Controls line wrapping, column width budgeting, and console print layouts.

#' Abbreviate character values that exceed a target display width
#'
#' Used to keep each row of a printed regression table on one line: rather
#' than let base `print.data.frame` stack columns into separate vertical
#' blocks when the table is wider than `options(width)`, the long `Variable`
#' labels are shortened first.
#' @keywords internal
#' @noRd
.abbreviate_to_width <- function(x, width) {
  width <- max(as.integer(width), 1L)
  out <- x
  too_long <- !is.na(x) & nchar(x) > width
  out[too_long] <- paste0(substr(x[too_long], 1L, max(width - 1L, 1L)), "\u2026")
  out
}

#' Print a regression display table with each row kept on one line
#'
#' `print.data.frame` stacks columns into separate blocks when a table is
#' wider than `getOption("width")`, which visually disconnects a term's
#' label from its estimate. This instead abbreviates the `Variable` column
#' (the one column whose content is free text rather than fixed-format
#' numeric evidence) just enough to fit, and always prints one row per line.
#' @keywords internal
#' @noRd
.print_regtab_table <- function(df, width = getOption("width", 80L)) {
  if (nrow(df) == 0 || ncol(df) == 0) {
    print(df, row.names = FALSE)
    return(invisible(NULL))
  }
  # `format()` left-pads character columns to a common per-column width even
  # with trim = TRUE (trim only suppresses numeric padding), so the result
  # must be trimmed again before this function's own width budget applies.
  chr <- as.data.frame(lapply(df, function(col) trimws(format(col, trim = TRUE))))
  names(chr) <- names(df)

  widths <- vapply(seq_along(chr), function(i) {
    max(nchar(names(chr)[[i]]), nchar(chr[[i]]))
  }, integer(1))
  gap <- 2L
  total <- sum(widths) + gap * (length(widths) - 1L)

  if (total > width && "Variable" %in% names(chr)) {
    var_idx <- which(names(chr) == "Variable")
    budget <- max(width - (total - widths[[var_idx]]), 8L)
    if (budget < widths[[var_idx]]) {
      chr$Variable <- .abbreviate_to_width(chr$Variable, budget)
      widths[[var_idx]] <- max(nchar("Variable"), max(nchar(chr$Variable)))
    }
  }

  fmt_row <- function(values) {
    cells <- vapply(seq_along(values), function(i) formatC(values[[i]], width = -widths[[i]]), character(1))
    sub(" +$", "", paste(cells, collapse = strrep(" ", gap)))
  }

  cat(fmt_row(names(chr)), "\n", sep = "")
  for (r in seq_len(nrow(chr))) {
    cat(fmt_row(vapply(chr, `[[`, character(1), r)), "\n", sep = "")
  }
  invisible(NULL)
}

#' Print formatted regression table and model diagnostics to console
#' @keywords internal
#' @noRd
.regtab_print <- function(x, details = FALSE, ...) {
  x <- validate_simtab_result(x)
  details <- .validate_print_details(details)
  if (details) {
    header <- sprintf(
      "Regression Table [%d outcome%s | %s(%s) | %s SE | %d%% CI]",
      x$meta$n_succeeded,
      if (x$meta$n_succeeded == 1) "" else "s",
      x$meta$family,
      x$meta$link,
      if (isTRUE(x$meta$robust)) sprintf("Robust %s", x$meta$vcov %||% "HC0") else "Model-based",
      x$meta$conf_pct
    )
    cat(header, "\n")
    if (!is.null(x$call)) {
      cat("Call: ", paste(deparse(x$call), collapse = " "), "\n", sep = "")
    }
    cat("\n")
  }
  .print_regtab_table(as.data.frame(x, tidy = FALSE), width = getOption("width", 80L))

  if (isTRUE(x$meta$n_failed > 0)) {
    failed <- x$meta$model_info[x$meta$model_info$failed, "outcome", drop = TRUE]
    cat("\nFailed outcomes:", paste(failed, collapse = ", "), "\n")
  }
  .print_advice(x)
  invisible(x)
}

#########
# ENGINE RENDERER DICTIONARY AND CONTRACT METHODS
# Implements data frame coercion, tidy, glance, and flextable renderers for regtab.

#' Coerce regression result to tidy or formatted display data frame
#' @keywords internal
#' @noRd
.regtab_as_data_frame <- function(x, row.names = NULL, optional = FALSE, tidy = FALSE, ..., vif = FALSE) {
  x <- validate_simtab_result(x)
  if (isTRUE(tidy)) {
    out <- x$data
    names(out)[names(out) == "lower"] <- "conf.low"
    names(out)[names(out) == "upper"] <- "conf.high"
    names(out)[names(out) == "p"] <- "p.value"
    return(out[c("outcome", "term", "estimate", "conf.low", "conf.high", "p.value")])
  }

  .build_regtab_display(x, vif = vif)
}

#' Coerce regression result to tidy long data frame
#' @keywords internal
#' @noRd
.regtab_tidy <- function(x, ...) {
  as.data.frame(x, tidy = TRUE, ...)
}

#' Construct model summary glance data frame with fit statistics
#' @keywords internal
#' @noRd
.regtab_glance <- function(x, ..., vif = FALSE) {
  x <- validate_simtab_result(x)
  out <- x$meta$model_info
  out$n_succeeded <- x$meta$n_succeeded
  out$n_failed <- x$meta$n_failed
  out <- out[c(
    "outcome", "n", "family", "link", "robust", "vcov", "method", "dispersion", "events", "exponentiate",
    "converged", "boundary", "failed", "error", "n_succeeded", "n_failed"
  )]

  if (isTRUE(vif) && "vif" %in% names(x$data)) {
    max_vif <- rep(NA_real_, nrow(out))
    max_vif_term <- rep(NA_character_, nrow(out))
    for (i in seq_len(nrow(out))) {
      rows <- x$data[x$data$outcome == out$outcome[[i]] & !is.na(x$data$vif), , drop = FALSE]
      if (nrow(rows) == 0) {
        next
      }
      j <- which.max(rows$vif)
      max_vif[[i]] <- rows$vif[[j]]
      max_vif_term[[i]] <- rows$vif_term[[j]] %||% rows$term[[j]]
    }
    out$max_vif <- max_vif
    out$max_vif_term <- max_vif_term
  }

  out
}

#' Render publication-styled flextable for regression results
#' @keywords internal
#' @noRd
.regtab_as_flextable <- function(x, footnotes = NULL, ...) {
  .require_pkg("flextable")

  df <- as.data.frame(x, tidy = FALSE)
  ft <- flextable::flextable(df, ...)
  if (isTRUE(x$meta$n_failed > 0)) {
    failed <- x$meta$model_info[x$meta$model_info$failed, "outcome", drop = TRUE]
    ft <- flextable::add_footer_lines(
      ft,
      values = paste0("Failed outcomes: ", paste(failed, collapse = ", "))
    )
    ft <- flextable::align(ft, part = "footer", align = "left")
  }
  ft <- .flex_add_footnotes(ft, footnotes)
  journal <- .regtab_journal(x)
  if (is.null(journal)) simtab_theme(ft) else journal$flex(ft)
}

#' Engine vtable renderer dictionary for regtab GLM models
#' @keywords internal
#' @noRd
.regtab_renderers <- function() {
  list(
    print = .regtab_print,
    as_data_frame = .regtab_as_data_frame,
    tidy = .regtab_tidy,
    glance = .regtab_glance,
    as_flextable = .regtab_as_flextable,
    autoplot = .forest_plot,
    as_methods = .methods_as_regtab
  )
}
