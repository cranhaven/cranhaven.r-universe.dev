# E-VALUE SENSITIVITY CALCULATOR FOR UNMEASURED CONFOUNDING
# Evaluates VanderWeele-Ding E-values for ratio estimates (RR, OR, HR) and confidence
# limits, quantifying the minimum confounding strength needed to explain away effects.

#########
# S3 GENERICS AND METHOD DISPATCH
# S3 method dispatch and result construction for ratio-scale E-values.

#' Compute E-values for SimtablR ratio estimates
#'
#' Computes VanderWeele-Ding E-values for ratio-scale estimates and confidence
#' limits. Risk ratios are used directly. For common outcomes, odds ratios use
#' the square-root approximation and hazard ratios use the VanderWeele-Ding
#' conversion `(1 - 0.5^sqrt(HR)) / (1 - 0.5^sqrt(1/HR))`; with `rare = TRUE`
#' the supplied ratio is treated as a rare-outcome risk-ratio approximation.
#'
#' @param x A computed SimtablR result containing ratio estimates.
#' @param measure Optional ratio measure override: `"RR"`, `"OR"`, or `"HR"`.
#' @param rare Logical. Treat OR/HR estimates as rare-outcome approximations to
#'   risk ratios instead of applying the square-root approximation.
#' @param ... Reserved for future options.
#' @return A `simtab_e_value` result with raw E-value numerics.
#' @details
#' E-values require positive, finite ratio estimates and confidence limits from
#' a supported SimtablR result; unsupported scales fail with a classed input
#' condition. Missing or non-finite source estimates are not converted into
#' evidence. An E-value is a sensitivity-analysis threshold, not proof that
#' uncontrolled confounding is absent, and must be interpreted with the
#' identification assumptions of the parent analysis.
#' @references VanderWeele, T. J., & Ding, P. (2017). Sensitivity analysis in
#'   observational research: introducing the E-value. \emph{Annals of Internal
#'   Medicine}, 167(4), 268--274. \doi{10.7326/M16-2607}.
#' @examples
#' ratio_result <- tb(epitabl, diabetes, adjudicated_acs, or, ref = "No")
#' e_value(ratio_result)
#' @export
e_value <- function(x, ...) {
  UseMethod("e_value")
}

#' @rdname e_value
#' @export
e_value.simtab_result <- function(x, measure = NULL, rare = FALSE, ...) {
  x <- validate_simtab_result(x)
  rows <- .e_value_source_rows(x, measure = measure)
  if (nrow(rows) == 0) {
    simtab_abort_input(c(
      "No ratio-scale estimates were found for {.fn e_value}.",
      "i" = "E-values quantify unmeasured confounding for a ratio measure (RR, OR, HR).",
      "v" = "Compute a ratio-scale effect first, or name one with {.arg measure}."
    ))
  }

  vals <- lapply(seq_len(nrow(rows)), function(i) {
    .e_value_row(
      rows$measure[[i]],
      estimate = rows$estimate[[i]],
      lower = rows$conf.low[[i]],
      upper = rows$conf.high[[i]],
      rare = rare
    )
  })

  out <- cbind(
    rows[c("source", "outcome", "term", "measure", "estimate", "conf.low", "conf.high")],
    do.call(rbind, vals)
  )
  rownames(out) <- NULL

  spec <- x$spec
  spec$engine <- "e_value"
  meta <- list(
    engine = "e_value",
    parent_engine = x$meta$engine %||% x$spec$engine,
    rare = isTRUE(rare),
    d = x$meta$d %||% x$spec$fmt$d %||% 2L
  )
  advice <- list(.e_value_interpretation_advice())
  new_simtab_result(
    spec,
    data = out,
    meta = meta,
    used = x$used,
    advice = advice,
    call = match.call(),
    subclass = "simtab_e_value"
  )
}

#' Construct unmeasured confounding advisory notice for E-values
#' @keywords internal
#' @noRd
.e_value_interpretation_advice <- function() {
  list(
    id = "evalue_interpretation",
    severity = 1L,
    message = "E-values summarise the minimum unmeasured-confounding strength needed to explain away a ratio estimate.",
    citation = "VanderWeele & Ding 2017",
    fix = "Interpret E-values alongside design quality, measured confounding control, and outcome prevalence.",
    version = paste0(.simtab_ruleset_version(), "-evalue"),
    why = "An E-value is a sensitivity-analysis aid, not proof that unmeasured confounding is absent."
  )
}

#########
# SOURCE EXTRACTION AND ESTIMAND HARMONIZATION
# Extracts ratio estimates across regtab, survtab, and table1 results.

#' Extract ratio estimates and confidence limits from a SimtablR result
#' @keywords internal
#' @noRd
.e_value_source_rows <- function(x, measure = NULL) {
  if (inherits(x, "simtab_regtab")) {
    inferred <- .e_value_regtab_measure(x)
    measure <- .normalise_e_value_measure(measure %||% inferred)
    return(data.frame(
      source = "regtab",
      outcome = x$data$outcome,
      term = x$data$term,
      measure = measure,
      estimate = x$data$estimate,
      conf.low = x$data$lower,
      conf.high = x$data$upper
    ))
  }

  if (inherits(x, "simtab_cox")) {
    terms <- x$data$terms
    return(data.frame(
      source = "survtab",
      outcome = NA_character_,
      term = terms$term,
      measure = .normalise_e_value_measure(measure %||% "HR"),
      estimate = terms$estimate,
      conf.low = terms$lower,
      conf.high = terms$upper
    ))
  }

  if (inherits(x, "simtab_table1")) {
    return(.e_value_table1_rows(x, measure = measure))
  }

  ratios <- x$data$ratios %||% NULL
  mh <- x$data$mh %||% NULL
  if (is.null(ratios) && is.data.frame(mh) && "row_type" %in% names(mh)) {
    # A stratified table reports the Mantel-Haenszel pooled estimate; the
    # stratum-specific rows are descriptive, not the adjusted association.
    ratios <- mh[mh$row_type == "pooled", , drop = FALSE]
    ratios$level <- paste0(ratios$level, " (Mantel-Haenszel)")
  }
  if (is.data.frame(ratios)) {
    est_col <- .e_value_first_col(ratios, c("estimate", "ratio", "value"))
    low_col <- .e_value_first_col(ratios, c("lower", "lower_ci", "conf.low"))
    high_col <- .e_value_first_col(ratios, c("upper", "upper_ci", "conf.high"))
    type_col <- .e_value_first_col(ratios, c("type", "measure"))
    if (!anyNA(c(est_col, low_col, high_col, type_col))) {
      # Reference rows (fixed at 1, no interval) carry no association to bound.
      if ("ref" %in% names(ratios)) {
        ratios <- ratios[!ratios$ref, , drop = FALSE]
      }
    }
    if (!anyNA(c(est_col, low_col, high_col, type_col)) && nrow(ratios) > 0) {
      term <- ratios$variable %||% ratios$term %||% rep(NA_character_, nrow(ratios))
      if (!is.null(ratios$level)) {
        term <- ifelse(is.na(ratios$level), term, paste(term, ratios$level, sep = ": "))
      }
      return(data.frame(
        source = x$meta$engine %||% x$spec$engine %||% "simtab",
        outcome = ratios$outcome %||% NA_character_,
        term = term,
        measure = vapply(ratios[[type_col]], function(m) .normalise_e_value_measure(measure %||% m), character(1)),
        estimate = ratios[[est_col]],
        conf.low = ratios[[low_col]],
        conf.high = ratios[[high_col]]
      ))
    }
  }

  data.frame(
    source = character(), outcome = character(), term = character(),
    measure = character(), estimate = numeric(), conf.low = numeric(), conf.high = numeric()
  )
}

#' Extract crude and adjusted ratio rows from a Table 1 result
#' @keywords internal
#' @noRd
.e_value_table1_rows <- function(x, measure = NULL) {
  inferred <- x$meta$effect %||% x$spec$effect$measure
  measure <- .normalise_e_value_measure(measure %||% inferred)
  outcome <- x$meta$strat_var %||% NA_character_
  rows <- list()

  for (variable in names(x$data)) {
    record <- x$data[[variable]]
    for (kind in c("crude", "adjusted")) {
      estimates <- record[[kind]] %||% NULL
      if (!is.data.frame(estimates) ||
          !all(c("estimate", "lower", "upper", "ref") %in% names(estimates))) {
        next
      }
      keep <- !estimates$ref &
        is.finite(estimates$estimate) &
        is.finite(estimates$lower) &
        is.finite(estimates$upper)
      if (!any(keep)) {
        next
      }
      level <- estimates$level[keep] %||% rep(NA_character_, sum(keep))
      term <- ifelse(
        is.na(level) | !nzchar(level),
        variable,
        paste(variable, level, sep = ": ")
      )
      rows[[length(rows) + 1L]] <- data.frame(
        source = paste0("table1_", kind),
        outcome = outcome,
        term = term,
        measure = measure,
        estimate = estimates$estimate[keep],
        conf.low = estimates$lower[keep],
        conf.high = estimates$upper[keep]
      )
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      source = character(), outcome = character(), term = character(),
      measure = character(), estimate = numeric(), conf.low = numeric(), conf.high = numeric()
    ))
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Find the first existing matching column name in data frame
#' @keywords internal
#' @noRd
.e_value_first_col <- function(data, candidates) {
  found <- intersect(candidates, names(data))
  if (length(found) == 0) {
    return(NA_character_)
  }
  found[[1]]
}

#' Infer ratio scale from GLM family and link specification
#' @keywords internal
#' @noRd
.e_value_regtab_measure <- function(x) {
  if (identical(x$meta$family, "binomial") && identical(x$meta$link, "logit")) {
    return("OR")
  }
  if (identical(x$meta$family, "binomial") && identical(x$meta$link, "log")) {
    return("RR")
  }
  simtab_abort_input(c(
    "{.fn e_value} for {.fn regtab} supports binomial logit (OR) and binomial log (RR) models.",
    "i" = "This model is {.val {x$meta$family}} with a {.val {x$meta$link}} link.",
    "v" = "Refit with {.code family = binomial(\"logit\")} or {.code binomial(\"log\")}."
  ))
}

#' Validate and normalize user or inferred ratio measure name
#' @keywords internal
#' @noRd
.normalise_e_value_measure <- function(measure) {
  bad_measure <- function(received) {
    simtab_abort_input(c(
      "{.arg measure} must be one of {.val RR}, {.val OR}, or {.val HR}.",
      "i" = "Received: {.val {received}}.",
      "v" = "E-values are defined for ratio measures only."
    ))
  }
  if (!is.character(measure) || length(measure) != 1 || !nzchar(measure)) {
    bad_measure(measure)
  }
  measure <- toupper(measure)
  if (!measure %in% c("RR", "OR", "HR")) {
    bad_measure(measure)
  }
  measure
}

#########
# VANDERWEELE-DING MATHEMATICAL PRIMITIVES
# Computes point estimate and confidence bound E-values and scale conversions.

#' Compute VanderWeele-Ding E-value for a risk ratio
#' @keywords internal
#' @noRd
.e_value_rr <- function(rr) {
  if (!is.finite(rr) || is.na(rr)) {
    return(NA_real_)
  }
  if (rr < 1) {
    rr <- 1 / rr
  }
  if (rr <= 1) {
    return(1)
  }
  rr + sqrt(rr * (rr - 1))
}

#' Convert hazard ratio to risk ratio scale under VanderWeele-Ding formula
#' @keywords internal
#' @noRd
.e_value_hr_to_rr <- function(hr) {
  # VanderWeele & Ding (2017) common-outcome conversion, as in EValue::evalues.HR:
  # RR = (1 - 0.5^sqrt(HR)) / (1 - 0.5^sqrt(1/HR)).
  if (!is.finite(hr) || is.na(hr)) {
    return(NA_real_)
  }
  (1 - 0.5^sqrt(hr)) / (1 - 0.5^sqrt(1 / hr))
}

#' Identify the confidence limit closest to the null on the ratio scale
#' @keywords internal
#' @noRd
.e_value_ci_bound <- function(lower, upper) {
  if (!is.finite(lower) || !is.finite(upper) || is.na(lower) || is.na(upper)) {
    return(NA_real_)
  }
  if (lower <= 1 && upper >= 1) {
    return(1)
  }
  if (lower > 1) {
    return(lower)
  }
  1 / upper
}

#' Compute E-value metrics and approximation indicators for a single ratio estimate
#' @keywords internal
#' @noRd
.e_value_row <- function(measure, estimate, lower, upper, rare = FALSE) {
  measure <- .normalise_e_value_measure(measure)
  approximation <- measure %in% c("OR", "HR") && !isTRUE(rare)
  transform <- if (!approximation) {
    identity
  } else if (identical(measure, "OR")) {
    sqrt
  } else {
    .e_value_hr_to_rr
  }
  rr_est <- transform(estimate)
  rr_lower <- transform(lower)
  rr_upper <- transform(upper)
  rr_bound <- .e_value_ci_bound(rr_lower, rr_upper)

  data.frame(
    e_value = .e_value_rr(rr_est),
    e_value_ci = .e_value_rr(rr_bound),
    approximation = approximation,
    rare = isTRUE(rare)
  )
}

#########
# ENGINE RENDERERS AND PRESENTATION
# Implements console printing, data frame coercion, and methods sentence generation.

#' Extract E-value evidence data frame
#' @keywords internal
#' @noRd
.e_value_as_data_frame <- function(x, row.names = NULL, optional = FALSE, tidy = FALSE, ...) {
  validate_simtab_result(x)$data
}

#' Print formatted E-value summary table and advice to console
#' @keywords internal
#' @noRd
.e_value_print <- function(x, ...) {
  x <- validate_simtab_result(x)
  cat("E-values\n\n")
  print(noquote(as.data.frame(x)), row.names = FALSE)
  .print_advice(x)
  invisible(x)
}

#' Synthesize epidemiological methods statement for E-value sensitivity analysis
#' @keywords internal
#' @noRd
.e_value_as_methods <- function(x, ...) {
  "E-values were computed using the VanderWeele-Ding closed-form sensitivity analysis for ratio estimates."
}

#' Engine vtable renderer dictionary for E-value results
#' @keywords internal
#' @noRd
.e_value_renderers <- function() {
  list(
    print = .e_value_print,
    as_data_frame = .e_value_as_data_frame,
    tidy = .e_value_as_data_frame,
    as_methods = .e_value_as_methods
  )
}
