# DIAGNOSTIC 2X2 CONTINGENCY MATRIX STATISTICAL ENGINE
# Computes clinical diagnostic accuracy measures and 2x2 agreement statistics.
# Generates sensitivity, specificity, predictive values, likelihood ratios, DOR, Youden J, F1, and Kappa.

#########
# DIAGNOSTIC ENGINE DISPATCH
# Assembles 2x2 confusion matrix and calculates point estimates with confidence intervals.

#' Execute diagnostic accuracy engine on a specification and dataset
#' @keywords internal
#' @noRd
.engine_diag <- function(spec, data) {
  args <- .diag_args_from_spec(spec)
  test_var <- args$test
  ref_var <- args$ref

  if (!test_var %in% names(data)) {
    simtab_abort_binding(c(
      "{.arg test} variable {.val {test_var}} not found in data.",
      "i" = "Columns available: {.val {names(data)}}.",
      "v" = "Check the spelling, or select an existing column."
    ))
  }
  if (!ref_var %in% names(data)) {
    simtab_abort_binding(c(
      "{.arg ref} variable {.val {ref_var}} not found in data.",
      "i" = "Columns available: {.val {names(data)}}.",
      "v" = "Check the spelling, or select an existing column."
    ))
  }

  val_test <- data[[test_var]]
  val_ref <- data[[ref_var]]

  ok <- !is.na(val_test) & !is.na(val_ref)
  n_missing <- sum(!ok)
  if (n_missing > 0) {
    message(sprintf(
      "Removed %d observation(s) with missing values (%.1f%%).",
      n_missing, 100 * n_missing / length(val_test)
    ))
  }

  val_test <- val_test[ok]
  val_ref <- val_ref[ok]

  if (length(val_test) == 0) {
    simtab_abort_engine(c(
      "No valid observations after removing missing values.",
      "i" = "Every row was missing the index test, the reference standard, or both.",
      "v" = "Check the two columns for all-missing data before estimating accuracy."
    ))
  }

  test_continuous_candidate <- is.numeric(val_test) && !is.factor(val_test)

  if (!is.factor(val_test)) {
    val_test <- factor(val_test)
  }
  if (!is.factor(val_ref)) {
    val_ref <- factor(val_ref)
  }

  levs_test <- levels(val_test)
  levs_ref <- levels(val_ref)

  if (length(levs_test) != 2) {
    nudge <- if (isTRUE(test_continuous_candidate) || length(levs_test) > 10L) {
      "This looks like a continuous marker; use {.fn roc} instead of {.fn diag_test}."
    } else {
      "Collapse the index test to a positive/negative pair before estimating accuracy."
    }
    simtab_abort_binding(c(
      "{.arg test} must have exactly 2 levels, but has {length(levs_test)}.",
      "i" = "Levels found: {.val {levs_test}}.",
      "v" = nudge
    ))
  }
  if (length(levs_ref) != 2) {
    simtab_abort_binding(c(
      "{.arg ref} must have exactly 2 levels, but has {length(levs_ref)}.",
      "i" = "Levels found: {.val {levs_ref}}.",
      "v" = "Collapse the reference standard to a diseased/non-diseased pair."
    ))
  }

  candidates_ref <- c(
    "1", "Sim", "Yes", "Positivo", "Positive",
    "Doente", "Disease", "Case", "Event", "S", "Y",
    "TRUE", "True"
  )
  candidates_test <- c(
    "1", "Sim", "Yes", "Positivo", "Positive",
    "Reagente", "Detected", "S", "Y", "TRUE", "True"
  )

  pos_ref <- .resolve_pos_level(
    args$positive,
    levs_ref,
    candidates_ref,
    "reference",
    "positive"
  )
  pos_test <- .resolve_pos_level_test(
    args$test_positive,
    levs_test,
    pos_ref,
    candidates_test
  )
  neg_ref <- setdiff(levs_ref, pos_ref)[1]
  neg_test <- setdiff(levs_test, pos_test)[1]

  val_ref_ord <- factor(val_ref, levels = c(pos_ref, neg_ref))
  val_test_ord <- factor(val_test, levels = c(pos_test, neg_test))
  tab <- table(Test = val_test_ord, Ref = val_ref_ord)

  tp <- unname(tab[1, 1])
  fp <- unname(tab[1, 2])
  fn <- unname(tab[2, 1])
  tn <- unname(tab[2, 2])
  total <- tp + fp + fn + tn

  if (tp + fn == 0L) {
    warning("No positive cases in reference (TP + FN = 0).", call. = FALSE)
  }
  if (tn + fp == 0L) {
    warning("No negative cases in reference (TN + FP = 0).", call. = FALSE)
  }
  if (tp + fp == 0L) {
    warning("No positive test results (TP + FP = 0).", call. = FALSE)
  }
  if (tn + fn == 0L) {
    warning("No negative test results (TN + FN = 0).", call. = FALSE)
  }

  metrics <- rbind(
    sensitivity = .diag_prop_metric(tp, tp + fn, args$conf.level, args$ci),
    specificity = .diag_prop_metric(tn, tn + fp, args$conf.level, args$ci),
    ppv = .diag_prop_metric(tp, tp + fp, args$conf.level, args$ci),
    npv = .diag_prop_metric(tn, tn + fn, args$conf.level, args$ci),
    accuracy = .diag_prop_metric(tp + tn, total, args$conf.level, args$ci),
    prevalence = .diag_prop_metric(tp + fn, total, args$conf.level, args$ci),
    lr_pos = .diag_lr_metric(tp, fp, fn, tn, args$conf.level, "pos"),
    lr_neg = .diag_lr_metric(tp, fp, fn, tn, args$conf.level, "neg"),
    youden_j = .diag_scalar_metric(.youden_j(tp, fp, fn, tn)),
    f1 = .diag_scalar_metric(.f1_score(tp, fp, fn, tn)),
    dor = .diag_dor_metric(tp, fp, fn, tn, args$conf.level),
    kappa = .diag_kappa_metric(tp, fp, fn, tn, args$conf.level)
  )

  confusion_matrix <- matrix(
    c(tp, fp, fn, tn),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(
      Test = c(pos_test, neg_test),
      Ref = c(pos_ref, neg_ref)
    )
  )

  list(
    data = list(
      confusion_matrix = confusion_matrix,
      metrics = metrics
    ),
    meta = list(
      test_var = test_var,
      ref_var = ref_var,
      positive = list(ref = pos_ref, test = pos_test),
      negative = list(ref = neg_ref, test = neg_test),
      sample_size = as.integer(total),
      conf.level = args$conf.level,
      conf_pct = round(args$conf.level * 100),
      ci = args$ci,
      percent = isTRUE(args$percent),
      metric_labels = .diag_metric_labels(),
      style = spec$style,
      engine = "accuracy"
    )
  )
}

#########
# SPECIFICATION EXTRACTION AND PROPORTION METRICS
# Extracts arguments and computes binomial proportion metrics with exact or Wilson intervals.

#' Extract diagnostic engine arguments from specification object
#' @keywords internal
#' @noRd
.diag_args_from_spec <- function(spec) {
  diag_opts <- spec$engine_opts$accuracy
  if (is.null(diag_opts)) {
    diag_opts <- list()
  }

  list(
    test = .resolve_single_role(spec, "test"),
    ref = .resolve_single_role(spec, "ref_std"),
    positive = diag_opts$positive %||% NULL,
    test_positive = diag_opts$test_positive %||% NULL,
    ci = diag_opts$ci %||% "exact",
    conf.level = diag_opts$conf.level %||% 0.95,
    percent = diag_opts$percent %||% spec$fmt$percent %||% FALSE
  )
}

#' Compute binomial proportion point estimate and confidence interval
#' @keywords internal
#' @noRd
.diag_prop_metric <- function(x, n, conf.level, ci) {
  if (n == 0L) {
    return(c(
      estimate = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      numerator = x,
      denominator = n
    ))
  }

  estimate <- x / n
  interval <- if (identical(ci, "wilson")) {
    .wilson_ci(x, n, conf.level)
  } else {
    stats::binom.test(x, n, conf.level = conf.level)$conf.int
  }

  c(
    estimate = estimate,
    conf.low = interval[1],
    conf.high = interval[2],
    numerator = as.numeric(x),
    denominator = as.numeric(n)
  )
}

#' Calculate score (Wilson) confidence interval for binomial proportions
#' @keywords internal
#' @noRd
.wilson_ci <- function(x, n, conf.level) {
  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  p <- x / n
  denom <- 1 + (z ^ 2) / n
  centre <- (p + (z ^ 2) / (2 * n)) / denom
  half <- (z * sqrt((p * (1 - p) / n) + (z ^ 2) / (4 * n ^ 2))) / denom
  c(centre - half, centre + half)
}

#' Wrap point estimate without confidence bounds into metric vector
#' @keywords internal
#' @noRd
.diag_scalar_metric <- function(estimate) {
  c(
    estimate = estimate,
    conf.low = NA_real_,
    conf.high = NA_real_,
    numerator = NA_real_,
    denominator = NA_real_
  )
}

#########
# RATIOS AND AGREEMENT METRICS
# Likelihood ratios, diagnostic odds ratios, and chance-corrected Cohen's kappa statistics.

#' Compute positive or negative likelihood ratio with log-method confidence bounds
#' @keywords internal
#' @noRd
.diag_lr_metric <- function(tp, fp, fn, tn, conf.level, which = c("pos", "neg")) {
  which <- match.arg(which)
  estimate <- if (identical(which, "pos")) {
    .lr_pos(tp, fp, fn, tn)
  } else {
    .lr_neg(tp, fp, fn, tn)
  }

  ci <- c(NA_real_, NA_real_)
  if (all(c(tp, fp, fn, tn) > 0)) {
    katz <- if (identical(which, "pos")) {
      .calc_pr_katz(tp, tp + fn, fp, fp + tn, conf.level)
    } else {
      .calc_pr_katz(fn, tp + fn, tn, fp + tn, conf.level)
    }
    ci <- c(katz$lower, katz$upper)
  }

  c(
    estimate = estimate,
    conf.low = ci[1],
    conf.high = ci[2],
    numerator = NA_real_,
    denominator = NA_real_
  )
}

#' Compute Cohen's kappa agreement statistic with large-sample variance
#' @keywords internal
#' @noRd
.diag_kappa_metric <- function(tp, fp, fn, tn, conf.level) {
  na_out <- c(
    estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_,
    numerator = NA_real_, denominator = NA_real_
  )
  n <- tp + fp + fn + tn
  if (n <= 0) {
    return(na_out)
  }

  # rows = index test (+, -); columns = reference standard (+, -)
  p <- matrix(c(tp, fp, fn, tn), nrow = 2, byrow = TRUE) / n
  row_sum <- rowSums(p)
  col_sum <- colSums(p)
  p_o <- p[1, 1] + p[2, 2]
  p_e <- row_sum[1] * col_sum[1] + row_sum[2] * col_sum[2]
  if (!is.finite(p_e) || isTRUE(all.equal(unname(p_e), 1))) {
    return(na_out)
  }

  kappa <- unname((p_o - p_e) / (1 - p_e))

  a <- 0
  for (i in 1:2) {
    a <- a + p[i, i] * ((1 - p_e) - (row_sum[i] + col_sum[i]) * (1 - p_o))^2
  }
  b <- 0
  for (i in 1:2) {
    for (j in 1:2) {
      if (i == j) next
      b <- b + p[i, j] * (col_sum[i] + row_sum[j])^2
    }
  }
  b <- b * (1 - p_o)^2
  cterm <- (p_o * p_e - 2 * p_e + p_o)^2
  var_k <- unname((a + b - cterm) / (n * (1 - p_e)^4))

  half <- if (is.finite(var_k) && var_k > 0) {
    stats::qnorm(1 - (1 - conf.level) / 2) * sqrt(var_k)
  } else {
    NA_real_
  }

  c(
    estimate = kappa,
    conf.low = max(-1, kappa - half),
    conf.high = min(1, kappa + half),
    numerator = NA_real_,
    denominator = NA_real_
  )
}

#' Compute diagnostic odds ratio with asymptotic Wald confidence interval
#' @keywords internal
#' @noRd
.diag_dor_metric <- function(tp, fp, fn, tn, conf.level) {
  if (any(c(tp, fp, fn, tn) < 0)) {
    simtab_abort_engine(c(
      "Confusion-matrix cells must be non-negative.",
      "i" = "Received TP = {tp}, FP = {fp}, FN = {fn}, TN = {tn}.",
      "v" = "Supply counts, not proportions or differences."
    ))
  }

  estimate <- if ((fp * fn) == 0) {
    if ((tp * tn) == 0) NA_real_ else Inf
  } else {
    (tp * tn) / (fp * fn)
  }

  if (all(c(tp, fp, fn, tn) > 0) && is.finite(estimate)) {
    z <- stats::qnorm(1 - (1 - conf.level) / 2)
    se <- sqrt((1 / tp) + (1 / fp) + (1 / fn) + (1 / tn))
    limits <- exp(log(estimate) + c(-1, 1) * z * se)
  } else {
    limits <- c(NA_real_, NA_real_)
  }

  c(
    estimate = estimate,
    conf.low = limits[1],
    conf.high = limits[2],
    numerator = as.numeric(tp * tn),
    denominator = as.numeric(fp * fn)
  )
}

#########
# POINT ESTIMATE CLINICAL CALCULATORS
# Direct arithmetic formulas for sensitivity/specificity ratios, Youden index, and F1 score.

#' Compute positive likelihood ratio from confusion matrix counts
#' @keywords internal
#' @noRd
.lr_pos <- function(tp, fp, fn, tn) {
  sens <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
  spec <- if ((tn + fp) == 0) NA_real_ else tn / (tn + fp)
  if (is.na(sens) || is.na(spec)) {
    return(NA_real_)
  }
  denominator <- 1 - spec
  if (denominator == 0 && sens == 0) {
    return(NA_real_)
  }
  sens / denominator
}

#' Compute negative likelihood ratio from confusion matrix counts
#' @keywords internal
#' @noRd
.lr_neg <- function(tp, fp, fn, tn) {
  sens <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
  spec <- if ((tn + fp) == 0) NA_real_ else tn / (tn + fp)
  if (is.na(sens) || is.na(spec)) {
    return(NA_real_)
  }
  numerator <- 1 - sens
  if (spec == 0 && numerator == 0) {
    return(NA_real_)
  }
  numerator / spec
}

#' Compute Youden J index summarizing diagnostic discriminatory performance
#' @keywords internal
#' @noRd
.youden_j <- function(tp, fp, fn, tn) {
  sens <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
  spec <- if ((tn + fp) == 0) NA_real_ else tn / (tn + fp)
  if (is.na(sens) || is.na(spec)) {
    return(NA_real_)
  }
  sens + spec - 1
}

#' Compute harmonic mean F1 score between precision and recall
#' @keywords internal
#' @noRd
.f1_score <- function(tp, fp, fn, tn) {
  precision <- if ((tp + fp) == 0) NA_real_ else tp / (tp + fp)
  recall <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
  if (is.na(precision) || is.na(recall) || (precision + recall) == 0) {
    return(NA_real_)
  }
  2 * precision * recall / (precision + recall)
}
