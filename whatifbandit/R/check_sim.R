#' Perform Validation Checks for [simulate_mab()]
#' @description This function checks to ensure that all required arguments
#' have been properly passed to [simulate_mab()] before continuing with the simulation. When
#' errors are thrown, user-friendly messages are provided to indicate which argument
#' was misspecified. Additionally, when `verbose = TRUE`, additional warning
#' messages may be shown if unnecessary arguments are passed.
#' @name check_mab_sim
#' @inheritParams simulate_mab
#' @returns Nothing; Throws an error if all checks are not met.
#' @keywords internal
check_mab_sim <- function(
  n,
  t,
  p,
  blocks = NULL,
  clusters = NULL,
  control_augment,
  random_assign_prop,
  assignment_dates,
  delayed_feedback,
  time_model = NULL,
  period_sizes = NULL,
  prior_periods = NULL,
  discount_rate,
  dt,
  ndraws = 5000,
  r,
  keep_data,
  keep_models,
  verbose
) {
  check_logical(dt, keep_data, keep_models, verbose)
  check_posint(n, t, ndraws, r, prior_periods, period_sizes)
  check_prop(control_augment, random_assign_prop, discount_rate)

  if (t > n) {
    rlang::abort(
      c("`t` cannot be larger than `n`"),
      "x" = sprintf("You Passed: t: %d, n: %d", t, n)
    )
  }

  if (!is.null(period_sizes)) {
    if (t != length(period_sizes)) {
      rlang::abort(c(
        "When provided `period_sizes` must be length `t`",
        "x" = sprintf("`t`: %d", t),
        "x" = sprintf("`length(period_sizes)` = %d", length(period_sizes))
      ))
    }
    if (n != sum(period_sizes)) {
      rlang::abort(c(
        "When provided `period_sizes` must sum to `n`",
        "x" = sprintf("`n`: %d", n),
        "x" = sprintf("`sum(period_sizes)` = %d", sum(period_sizes))
      ))
    }
  }

  if (!is.null(assignment_dates) && !lubridate::is.Date(assignment_dates)) {
    rlang::abort("`assignment_dates` must be a `Date` vector")
  }

  if (!is.null(time_model) && !is.function(time_model)) {
    rlang::abort("`time_model` must be a function")
  }

  if (delayed_feedback) {
    if (is.null(time_model)) {
      rlang::abort(c(
        "`time_model` must be provided when `delayed_feedback = TRUE`.",
        "x" = "`time_model` is NULL"
      ))
    }
    if (is.null(assignment_dates)) {
      rlang::abort(c(
        "`assignment_dates` must be provided when `delayed_feedback = TRUE`.",
        "x" = "`assignment_dates` is NULL"
      ))
    }
  } else if (!is.null(time_model) && !is.null(assignment_dates)) {
    rlang::warn(c(
      "`time_model` and `assignment_dates` are provided but `delayed_feedback = FALSE`.",
      "i" = "Counterfactual success dates will be simulated but not used for assignment."
    ))
  }

  if (!is.matrix(p) || !is.numeric(p)) {
    rlang::abort("`p` must be a numeric matrix")
  }
  if (is.null(rownames(p))) {
    rlang::abort(c(
      "`p` must have rownames corresponding to treatment conditions.",
      "x" = "`rownames(p)` is NULL"
    ))
  }

  if (any(p > 1 | p < 0)) {
    rlang::abort(c(
      "all `p` must be probabilities between 0 and 1",
      "x" = paste0("You passed: ", paste0(p, collapse = ", "))
    ))
  }

  if (!is.null(blocks) && !is.null(clusters)) {
    do.call(check_sum1, c(list(blocks), clusters))
    do.call(check_names, c(list(blocks), clusters, list(clusters)))
    if (!setequal(names(clusters), names(blocks))) {
      rlang::abort(c(
        "`names(clusters)` must match `names(blocks)` for nested structure.",
        "x" = sprintf(
          "block labels: %s",
          paste(names(blocks), collapse = ", ")
        ),
        "x" = sprintf(
          "cluster labels: %s",
          paste(names(clusters), collapse = ", ")
        )
      ))
    }
    check_p_colnames(p, unlist(lapply(clusters, names)))
  } else if (!is.null(clusters)) {
    check_sum1(clusters = clusters)
    check_names(clusters)
    check_p_colnames(p, names(clusters))
  } else if (!is.null(blocks)) {
    check_sum1(blocks = blocks)
    check_names(blocks)
    check_p_colnames(p, names(blocks))
  } else if (ncol(p) != 1) {
    rlang::abort(c(
      "`p` must have exactly 1 column when no blocks or clusters are provided.",
      "x" = sprintf("`ncol(p)` = %d", ncol(p))
    ))
  }
}

#' @describeIn check_mab_sim Checks if `colnames(p)` matches provided labels
#' @inheritParams simulate_mab
#' @param expected Expected set of group labels.
#' @returns Nothing; Throws an error if `colnames(p)` doesn't match provided labels.

check_p_colnames <- function(p, expected) {
  if (!setequal(colnames(p), expected)) {
    rlang::abort(c(
      "`colnames(p)` must match group labels.",
      "x" = sprintf("Expected: %s", paste(expected, collapse = ", ")),
      "x" = sprintf("Got: %s", paste(colnames(p), collapse = ", "))
    ))
  }
}
