# EPIDEMIOLOGICAL ASSOCIATION AND HYPOTHESIS TESTING MEASURES
# Implements crude effect measure estimators (prevalence ratio, risk ratio,
# odds ratio), Campbell N-1 and Fisher-Irwin tests, Mantel-Haenszel pooling,
# and Breslow-Day tests of effect measure homogeneity.

#########
# CONTINGENCY TESTS AND DECISION POLICIES
# Evaluates N-1 chi-squared, Fisher-Irwin exact tests, and Campbell expected count rules.

#' N-1 chi-squared test for 2x2 association
#'
#' Computes Pearson's chi-squared statistic without Yates correction, multiplied
#' by `(N - 1) / N`, with one degree of freedom.
#'
#' @param tab A 2x2 contingency table.
#' @return An `htest`-shaped list with raw statistic, df, p-value, and method.
#' @references Campbell, I. (2007). Chi-squared and Fisher-Irwin tests of
#'   two-by-two tables with small sample recommendations. \emph{Statistics in
#'   Medicine}, 26(19), 3661-3675. \doi{10.1002/sim.2832}.
#' @keywords internal
#' @noRd
.chisq_n_minus_1 <- function(tab) {
  tab <- as.matrix(tab)
  if (!.is_strict_2x2(tab)) {
    simtab_abort_engine(c(
      "N-1 chi-squared is defined for 2x2 tables only.",
      "i" = "Received a table with dimensions {.val {dim(tab)}}.",
      "v" = "Larger tables use the classical Pearson statistic; route through
             {.fn .chisq_campbell_or_pearson}."
    ))
  }
  n <- sum(tab)
  if (!is.finite(n) || n <= 0) {
    simtab_abort_engine(c(
      "N-1 chi-squared requires a non-empty table.",
      "i" = "The table's counts sum to {.val {n}}.",
      "v" = "Check the tabulated variables for all-missing data."
    ))
  }

  pearson <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
  statistic <- unname(pearson$statistic) * (n - 1) / n
  out <- list(
    statistic = stats::setNames(statistic, "X-squared"),
    parameter = stats::setNames(1, "df"),
    p.value = stats::pchisq(statistic, df = 1, lower.tail = FALSE),
    method = "N-1 chi-squared",
    data.name = pearson$data.name,
    expected = pearson$expected,
    observed = pearson$observed,
    residuals = pearson$residuals,
    stdres = pearson$stdres
  )
  class(out) <- "htest"
  out
}

#' Chi-squared test with Campbell 2x2 policy
#'
#' Uses N-1 chi-squared for 2x2 tables and classical Pearson chi-squared for
#' larger contingency tables.
#'
#' @param tab A contingency table.
#' @return An `htest` object.
#' @keywords internal
#' @noRd
.chisq_campbell_or_pearson <- function(tab) {
  tab <- as.matrix(tab)
  if (.is_strict_2x2(tab)) {
    .chisq_n_minus_1(tab)
  } else {
    stats::chisq.test(tab, correct = FALSE)
  }
}

#' Table strictly 2x2?
#'
#' The check is deliberately exact rather than `all(dim(tab) == c(2, 2))`, which recycles
#' silently against higher-dimensional arrays instead of rejecting them.
#'
#' @param tab A table or matrix.
#' @return `TRUE` for a 2x2 table with all-finite counts, otherwise `FALSE`.
#' @keywords internal
#' @noRd
.is_strict_2x2 <- function(tab) {
  identical(dim(tab), c(2L, 2L)) && all(is.finite(tab))
}

#' Auto-test decision for Campbell expected-cell boundary
#'
#' For 2x2 tables, Campbell's boundary is Fisher-Irwin only when any expected
#' count is below 1. Expected counts from 1 to below 5 keep N-1 chi-squared and
#' emit advice. Larger tables keep Pearson chi-squared.
#'
#' @param tab A contingency table.
#' @return A list with `method` (`"fisher"` or `"chisq"`) and optional `note`.
#' @keywords internal
#' @noRd
.campbell_expected_decision <- function(tab) {
  tab <- as.matrix(tab)
  expected <- tryCatch(
    suppressWarnings(stats::chisq.test(tab, correct = FALSE)$expected),
    error = function(e) NULL
  )
  if (!.is_strict_2x2(tab) || is.null(expected)) {
    return(list(method = "chisq", note = NULL, expected = expected))
  }

  min_expected <- min(expected)
  note <- NULL
  method <- "chisq"
  if (is.finite(min_expected) && min_expected < 1) {
    method <- "fisher"
    note <- .campbell_expected_note("fisher_low_expected", expected)
  } else if (is.finite(min_expected) && min_expected < 5) {
    note <- .campbell_expected_note("small_expected_cells", expected)
  }
  list(method = method, note = note, expected = expected)
}

#' Construct diagnostic advisory payload for small expected cell counts
#' @keywords internal
#' @noRd
.campbell_expected_note <- function(id, expected, variable = NULL) {
  list(
    id = id,
    variable = variable,
    min_expected = unname(min(expected)),
    n_small = unname(sum(expected < 5)),
    n_cells = length(expected)
  )
}

#' Attach variable context to Campbell expected cell advisory record
#' @keywords internal
#' @noRd
.with_campbell_variable <- function(note, variable) {
  if (is.null(note)) {
    return(NULL)
  }
  note$variable <- variable
  note
}

#########
# CRUDE EFFECT RATIO CALCULATORS
# Computes Katz risk/prevalence ratios, Woolf odds ratios, and Haldane-Anscombe corrections.
#' Detect a zero cell in a crude 2x2 comparison
#'
#' Any of the four cells of the index/reference by event/
#' non-event table. Used to trigger the Haldane-Anscombe continuity
#' correction shared by `.calc_pr_katz()` and `.calc_or_woolf()`.
#'
#' @param a_index Events in the index group.
#' @param n_index Total observations in the index group.
#' @param a_ref Events in the reference group.
#' @param n_ref Total observations in the reference group.
#' @return `TRUE` if any of the four cells is exactly zero, `FALSE` otherwise
#'   (including when the table is degenerate/not evaluable).
#' @keywords internal
#' @noRd
.has_zero_2x2_cell <- function(a_index, n_index, a_ref, n_ref) {
  if (
    is.na(a_index) ||
      is.na(n_index) ||
      is.na(a_ref) ||
      is.na(n_ref) ||
      n_index <= 0 ||
      n_ref <= 0 ||
      a_index < 0 ||
      a_ref < 0 ||
      a_index > n_index ||
      a_ref > n_ref
  ) {
    return(FALSE)
  }
  a_index == 0 || a_ref == 0 || (n_index - a_index) == 0 || (n_ref - a_ref) == 0
}

#' Prevalence / Risk Ratio with delta-method (log) confidence interval
#'
#' Computes the ratio of two proportions with the Katz log confidence interval.
#' The Prevalence Ratio (cross-sectional) and Risk Ratio (cohort) share this
#' computation and only the interpretation differs. When any of the 4 2x2
#' cells is zero, a Haldane-Anscombe +0.5 continuity correction is applied to
#' all four cells so the ratio and CI remain estimable (`corrected = TRUE` is
#' returned so callers can announce the correction).
#'
#' @param a_index Events in the index group.
#' @param n_index Total observations in the index group.
#' @param a_ref Events in the reference group.
#' @param n_ref Total observations in the reference group.
#' @param conf.level Confidence level (0-1).
#' @return A list with `estimate`, `lower`, `upper`, `p`, and `corrected` (all
#'   numerics `NA_real_` when the ratio is not estimable, e.g. an empty group).
#' @references Katz, D., Baptista, J., Azen, S. P., & Pike, M. C. (1978).
#'   Obtaining confidence intervals for the risk ratio in cohort studies.
#'   \emph{Biometrics}, 34(3), 469-474. \doi{10.2307/2530610}.
#' @references Haldane, J. B. S. (1956). The estimation and significance of
#'   the logarithm of a ratio of frequencies. \emph{Annals of Human Genetics},
#'   20(4), 309-311. \doi{10.1111/j.1469-1809.1955.tb01285.x}.
#' @references Anscombe, F. J. (1956). On estimating binomial response
#'   relations. \emph{Biometrika}, 43(3/4), 461-464.
#'   \doi{10.1093/biomet/43.3-4.461}.
#' @keywords internal
#' @noRd
.calc_pr_katz <- function(a_index, n_index, a_ref, n_ref, conf.level = 0.95) {
  na_out <- list(
    estimate = NA_real_,
    lower = NA_real_,
    upper = NA_real_,
    p = NA_real_,
    corrected = FALSE
  )
  if (
    is.na(a_index) ||
      is.na(n_index) ||
      is.na(a_ref) ||
      is.na(n_ref) ||
      n_index <= 0 ||
      n_ref <= 0 ||
      a_index < 0 ||
      a_ref < 0 ||
      a_index > n_index ||
      a_ref > n_ref
  ) {
    return(na_out)
  }

  corrected <- .has_zero_2x2_cell(a_index, n_index, a_ref, n_ref) # detect zero cell for Haldane-Anscombe correction
  ai <- a_index
  ni <- n_index
  ar <- a_ref
  nr <- n_ref
  if (corrected) {
    ai <- a_index + 0.5
    ni <- n_index + 1
    ar <- a_ref + 0.5
    nr <- n_ref + 1
  }

  risk_index <- ai / ni
  risk_ref <- ar / nr
  est <- risk_index / risk_ref
  se_log <- sqrt(
    (1 / ai - 1 / ni) + (1 / ar - 1 / nr)
  )
  if (is.na(se_log) || se_log <= 0) {
    return(na_out)
  }
  z_crit <- qnorm(1 - (1 - conf.level) / 2)
  z_stat <- log(est) / se_log
  list(
    estimate = est,
    lower = exp(log(est) - z_crit * se_log),
    upper = exp(log(est) + z_crit * se_log),
    p = 2 * pnorm(-abs(z_stat)),
    corrected = corrected
  )
}

#' Odds Ratio with logit confidence interval
#'
#' When any of the four 2x2 cells is zero, a Haldane-Anscombe continuity
#' correction is applied to all 4 cells so the odds ratio and CI remain
#' estimable (`corrected = TRUE` is returned so callers can announce it).
#'
#' @param a_index Events in the index group.
#' @param n_index Total observations in the index group.
#' @param a_ref Events in the reference group.
#' @param n_ref Total observations in the reference group.
#' @param conf.level Confidence level (0-1).
#' @return A list with `estimate`, `lower`, `upper`, `p`, and `corrected` (all
#'   numerics `NA_real_` when the odds ratio is not estimable, e.g. an empty
#'   group).
#' @references Woolf, B. (1955). On estimating the relation between blood group
#'   and disease. \emph{Annals of Human Genetics}, 19(4), 251-253.
#'   \doi{10.1111/j.1469-1809.1955.tb01348.x}.
#' @references Haldane, J. B. S. (1956). The estimation and significance of
#'   the logarithm of a ratio of frequencies. \emph{Annals of Human Genetics},
#'   20(4), 309-311. \doi{10.1111/j.1469-1809.1955.tb01285.x}.
#' @references Anscombe, F. J. (1956). On estimating binomial response
#'   relations. \emph{Biometrika}, 43(3/4), 461-464.
#'   \doi{10.1093/biomet/43.3-4.461}.
#' @keywords internal
#' @noRd
.calc_or_woolf <- function(a_index, n_index, a_ref, n_ref, conf.level = 0.95) {
  na_out <- list(
    estimate = NA_real_,
    lower = NA_real_,
    upper = NA_real_,
    p = NA_real_,
    corrected = FALSE
  )
  if (is.na(n_index) || is.na(n_ref) || n_index <= 0 || n_ref <= 0) {
    return(na_out)
  }
  a_i <- a_index
  b_i <- n_index - a_index
  a_1 <- a_ref
  b_1 <- n_ref - a_ref
  if (
    is.na(a_i) ||
      is.na(a_1) ||
      is.na(b_i) ||
      is.na(b_1) ||
      a_i < 0 ||
      b_i < 0 ||
      a_1 < 0 ||
      b_1 < 0
  ) {
    return(na_out)
  }

  corrected <- a_i == 0 || b_i == 0 || a_1 == 0 || b_1 == 0
  if (corrected) {
    a_i <- a_i + 0.5
    b_i <- b_i + 0.5
    a_1 <- a_1 + 0.5
    b_1 <- b_1 + 0.5
  }

  est <- (a_i * b_1) / (b_i * a_1)
  se_log <- sqrt(1 / a_i + 1 / b_i + 1 / a_1 + 1 / b_1)
  if (is.na(se_log) || se_log <= 0) {
    return(na_out)
  }

  z_crit <- qnorm(1 - (1 - conf.level) / 2)
  z_stat <- log(est) / se_log
  list(
    estimate = est,
    lower = exp(log(est) - z_crit * se_log),
    upper = exp(log(est) + z_crit * se_log),
    p = 2 * pnorm(-abs(z_stat)),
    corrected = corrected
  )
}

#########
# STRATIFIED MANTEL-HAENSZEL POOLING AND HOMOGENEITY TESTS
# Evaluates common odds ratios, Greenland-Robins risk ratios, and Breslow-Day tests.

#' Mantel-Haenszel Odds Ratio with base-R CMH oracle
#'
#' @param strata List of 2x2 matrices. Rows are index/reference exposure; columns
#'   are event/non-event outcome.
#' @param conf.level Confidence level (0-1).
#' @param correct Passed to [stats::mantelhaen.test()].
#' @return A list with raw numeric estimate, CI, CMH statistic, df, and p-value.
#' @keywords internal
#' @noRd
.calc_mh_or <- function(strata, conf.level = 0.95, correct = TRUE) {
  na_out <- list(
    estimate = NA_real_,
    lower = NA_real_,
    upper = NA_real_,
    statistic = NA_real_,
    parameter = NA_real_,
    p = NA_real_,
    method = "Mantel-Haenszel common odds ratio"
  )
  arr <- .mh_strata_array(strata)
  if (is.null(arr)) {
    return(na_out)
  }

  # A completely empty slice contains no comparative information and causes the function to return an otherwise avoidable all-NA result.
  informative <- vapply(
    seq_len(dim(arr)[3]),
    function(i) sum(arr[,, i]) > 0,
    logical(1)
  )
  arr <- arr[,, informative, drop = FALSE]
  if (dim(arr)[3] == 0) {
    return(na_out)
  }

  ref <- tryCatch(
    stats::mantelhaen.test(arr, conf.level = conf.level, correct = correct),
    error = function(e) NULL
  )
  if (is.null(ref)) {
    return(na_out)
  }

  list(
    estimate = unname(ref$estimate),
    lower = unname(ref$conf.int[1]),
    upper = unname(ref$conf.int[2]),
    statistic = unname(ref$statistic),
    parameter = unname(ref$parameter),
    p = ref$p.value,
    method = ref$method
  )
}

#' Mantel-Haenszel Risk Ratio with Greenland-Robins CI
#'
#' @details The variance of the log common risk ratio is the Greenland-Robins
#'   estimator: `Var(log RR) = sum((n1*n0*(a+c) - a*c*n)/n^2) / (R*S)` with
#'   `R = sum(a*n0/n)` and `S = sum(c*n1/n)`. Reference: Greenland S, Robins JM
#'   (1985). Estimation of a common effect parameter from sparse follow-up
#'   data. *Biometrics* 41(1), 55-68. \doi{10.2307/2530643}.
#' @param strata List of 2x2 matrices. Rows are index/reference exposure; columns
#'   are event/non-event outcome.
#' @param conf.level Confidence level (0-1).
#' @return A list with raw numeric estimate, CI, Wald z statistic, and p-value.
#' @keywords internal
#' @noRd
.calc_mh_rr <- function(strata, conf.level = 0.95) {
  na_out <- list(
    estimate = NA_real_,
    lower = NA_real_,
    upper = NA_real_,
    statistic = NA_real_,
    parameter = NA_real_,
    p = NA_real_,
    method = "Mantel-Haenszel common risk ratio"
  )
  tabs <- .mh_strata_list(strata)
  if (length(tabs) == 0) {
    return(na_out)
  }

  r_terms <- numeric(length(tabs))
  s_terms <- numeric(length(tabs))
  v_terms <- numeric(length(tabs))
  for (i in seq_along(tabs)) {
    tab <- tabs[[i]]
    a <- tab[1, 1]
    b <- tab[1, 2]
    c <- tab[2, 1]
    d <- tab[2, 2]
    n_index <- a + b
    n_ref <- c + d
    n <- n_index + n_ref
    if (n <= 0 || n_index <= 0 || n_ref <= 0) {
      # A stratum without both exposure arms contributes zero to R, S, and the Greenland-Robins variance numerator. Ignore it rather than
      next
    }
    r_terms[i] <- a * n_ref / n
    s_terms[i] <- c * n_index / n
    v_terms[i] <- (n_index * n_ref * (a + c) - a * c * n) / n^2
  }

  sum_r <- sum(r_terms)
  sum_s <- sum(s_terms)
  if (!is.finite(sum_r) || !is.finite(sum_s) || sum_r <= 0 || sum_s <= 0) {
    return(na_out)
  }

  est <- sum_r / sum_s
  var_log <- sum(v_terms) / (sum_r * sum_s)
  if (!is.finite(var_log) || var_log <= 0) {
    return(na_out)
  }

  se_log <- sqrt(var_log)
  z_crit <- stats::qnorm(1 - (1 - conf.level) / 2)
  z_stat <- log(est) / se_log
  list(
    estimate = est,
    lower = exp(log(est) - z_crit * se_log),
    upper = exp(log(est) + z_crit * se_log),
    statistic = z_stat,
    parameter = NA_real_,
    p = 2 * stats::pnorm(abs(z_stat), lower.tail = FALSE),
    method = "Mantel-Haenszel common risk ratio"
  )
}

#' Breslow-Day homogeneity test for stratified odds ratios
#'
#' Evaluates homogeneity of odds ratios across strata with optional Tarone adjustment.
#'
#' @param strata List of 2x2 matrices. Rows are index/reference exposure; columns
#'   are event/non-event outcome.
#' @param OR Optional common odds ratio. Defaults to the MH odds ratio.
#' @param correct Logical; apply Tarone's correction.
#' @return A list with raw numeric chi-square statistic, df, p-value, and method.
#' @keywords internal
#' @noRd
.breslow_day <- function(strata, OR = NA_real_, correct = FALSE) {
  na_out <- list(
    statistic = NA_real_,
    parameter = NA_real_,
    p = NA_real_,
    method = if (isTRUE(correct)) {
      "Breslow-Day homogeneity test with Tarone correction"
    } else {
      "Breslow-Day homogeneity test"
    }
  )
  tabs <- .mh_strata_list(strata)
  if (length(tabs) < 2) {
    return(na_out)
  }

  theta <- OR
  if (is.na(theta)) {
    theta <- .calc_mh_or(tabs)$estimate
  }
  if (!is.finite(theta) || theta <= 0) {
    return(na_out)
  }

  obs_a <- expected_a <- var_a <- numeric(0)
  for (tab in tabs) {
    row_tot <- rowSums(tab)
    col_tot <- colSums(tab)
    if (any(row_tot <= 0) || any(col_tot <= 0)) {
      next
    }

    coef <- c(
      -row_tot[1] * col_tot[1] * theta,
      col_tot[2] - row_tot[1] + theta * (col_tot[1] + row_tot[1]),
      1 - theta
    )
    roots <- Re(polyroot(coef))
    root <- roots[roots > 0 & roots <= min(row_tot[1], col_tot[1])]
    if (length(root) == 0 || !is.finite(root[1])) {
      next
    }

    ea <- root[1]
    eb <- row_tot[1] - ea
    ec <- col_tot[1] - ea
    ed <- row_tot[2] - ec
    if (any(c(ea, eb, ec, ed) <= 0)) {
      next
    }

    obs_a <- c(obs_a, tab[1, 1])
    expected_a <- c(expected_a, ea)
    var_a <- c(var_a, (1 / ea + 1 / eb + 1 / ec + 1 / ed)^-1)
  }

  if (length(obs_a) < 2 || any(!is.finite(var_a)) || any(var_a <= 0)) {
    return(na_out)
  }

  stat <- sum((obs_a - expected_a)^2 / var_a)
  if (isTRUE(correct)) {
    stat <- stat - (sum(obs_a) - sum(expected_a))^2 / sum(var_a)
  }
  stat <- max(0, as.numeric(stat))
  df <- length(obs_a) - 1
  list(
    statistic = stat,
    parameter = df,
    p = stats::pchisq(stat, df, lower.tail = FALSE),
    method = na_out$method
  )
}

#' Cochran Q homogeneity test for stratified risk or prevalence ratios
#'
#' Breslow-Day tests odds-ratio homogeneity, so it does not answer whether a
#' risk or prevalence ratio is constant across strata. This is the
#' inverse-variance Q statistic on the stratum log ratios, using the Katz
#' variance (with the same Haldane-Anscombe correction as the crude ratios).
#' Strata with no events, or without both exposure groups, carry no
#' information about the ratio and are skipped.
#'
#' @param strata List of 2x2 matrices. Rows are index/reference exposure;
#'   columns are event/non-event outcome.
#' @return A list with raw numeric Q statistic, df, p-value, and method.
#' @keywords internal
#' @noRd
.rr_homogeneity_q <- function(strata) {
  na_out <- list(
    statistic = NA_real_,
    parameter = NA_real_,
    p = NA_real_,
    method = "Cochran Q homogeneity test of log ratios"
  )
  tabs <- .mh_strata_list(strata)
  log_rr <- var_log <- numeric(0)
  for (tab in tabs) {
    a_i <- tab[1, 1]
    n_i <- sum(tab[1, ])
    a_r <- tab[2, 1]
    n_r <- sum(tab[2, ])
    if (n_i <= 0 || n_r <= 0 || (a_i + a_r) == 0) {
      next
    }
    if (.has_zero_2x2_cell(a_i, n_i, a_r, n_r)) {
      a_i <- a_i + 0.5
      n_i <- n_i + 1
      a_r <- a_r + 0.5
      n_r <- n_r + 1
    }
    v <- (1 / a_i - 1 / n_i) + (1 / a_r - 1 / n_r)
    if (!is.finite(v) || v <= 0) {
      next
    }
    log_rr <- c(log_rr, log((a_i / n_i) / (a_r / n_r)))
    var_log <- c(var_log, v)
  }
  if (length(log_rr) < 2) {
    return(na_out)
  }
  w <- 1 / var_log
  pooled <- sum(w * log_rr) / sum(w)
  stat <- sum(w * (log_rr - pooled)^2)
  df <- length(log_rr) - 1
  list(
    statistic = stat,
    parameter = df,
    p = stats::pchisq(stat, df, lower.tail = FALSE),
    method = na_out$method
  )
}

#' Coerce stratified 2x2 contingency tables into a validated list of matrices
#' @keywords internal
#' @noRd
.mh_strata_list <- function(strata) {
  if (is.null(strata)) {
    return(list())
  }
  if (length(dim(strata)) == 3 && all(dim(strata)[1:2] == c(2, 2))) {
    out <- lapply(seq_len(dim(strata)[3]), function(i) strata[,, i])
    names(out) <- dimnames(strata)[[3]]
    strata <- out
  }
  if (!is.list(strata)) {
    return(list())
  }
  out <- lapply(strata, function(tab) {
    tab <- as.matrix(tab)
    if (!all(dim(tab) == c(2, 2))) {
      return(NULL)
    }
    storage.mode(tab) <- "double"
    if (any(!is.finite(tab)) || any(tab < 0)) {
      return(NULL)
    }
    tab
  })
  out <- Filter(Negate(is.null), out)
  out
}

#' Coerce stratified 2x2 contingency tables into a 3-dimensional array
#' @keywords internal
#' @noRd
.mh_strata_array <- function(strata) {
  tabs <- .mh_strata_list(strata)
  if (length(tabs) == 0) {
    return(NULL)
  }
  arr <- array(
    NA_real_,
    dim = c(2, 2, length(tabs)),
    dimnames = list(
      rownames(tabs[[1]]) %||% c("index", "ref"),
      colnames(tabs[[1]]) %||% c("event", "nonevent"),
      names(tabs) %||% as.character(seq_along(tabs))
    )
  )
  for (i in seq_along(tabs)) {
    arr[,, i] <- tabs[[i]]
  }
  arr
}
