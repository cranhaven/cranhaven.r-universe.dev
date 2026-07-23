#' Tidy an ipf_rake object
#'
#' Returns a one-row-per-variable-per-level tibble with target proportions, weighted proportions, and discrepancy.
#'
#' @param x An `ipf_rake` object.
#' @param ... Additional arguments (ignored).
#'
#' @return A tibble with columns: `variable`, `level`, `target`, `weighted_pct`, `discrepancy`.
#'
#' @examples
#' data <- data.frame(
#'   gender = sample(c('M', 'F'), 100, replace = TRUE, prob = c(0.6, 0.4))
#' )
#' targets <- list(gender = c(M = 0.5, F = 0.5))
#' result <- rake(data, targets)
#' tidy(result)
#'
#' @importFrom generics tidy
#' @exportS3Method
tidy.ipf_rake <- function(x, ...) {
  assessment <- weight_assess(
    data = x$data,
    targets = x$targets,
    weights = x$weights,
    base_weights = x$base_weights,
    na_method = x$na_method
  )

  rows <- lapply(names(assessment), function(v) {
    tbl <- assessment[[v]]
    # Exclude the "Total" row
    tbl <- tbl[tbl$level != 'Total', , drop = FALSE]
    tibble::tibble(
      variable = v,
      level = tbl$level,
      target = tbl$target,
      weighted_pct = tbl$weighted_pct,
      discrepancy = tbl$residual_disc
    )
  })

  do.call(rbind, rows)
}

#' @importFrom generics tidy
#' @export
generics::tidy

#' Glance at an ipf_rake object
#'
#' Returns a single-row tibble with summary statistics.
#'
#' @param x An `ipf_rake` object.
#' @param ... Additional arguments (ignored).
#'
#' @return A single-row tibble with columns: `converged`, `iterations`, `max_prop_err`, `deff`, `n_eff`, `n_obs`, `n_vars`.
#'
#' @examples
#' data <- data.frame(
#'   gender = sample(c('M', 'F'), 100, replace = TRUE, prob = c(0.6, 0.4))
#' )
#' targets <- list(gender = c(M = 0.5, F = 0.5))
#' result <- rake(data, targets)
#' glance(result)
#'
#' @importFrom generics glance
#' @exportS3Method
glance.ipf_rake <- function(x, ...) {
  tibble::tibble(
    converged = x$converged,
    iterations = x$iterations,
    max_prop_err = x$max_prop_err,
    deff = x$deff,
    n_eff = x$n_eff,
    n_obs = length(x$weights),
    n_vars = length(x$vars_used)
  )
}

#' @importFrom generics glance
#' @export
generics::glance

#' Augment data with raked weights
#'
#' Returns the original data frame with a `.weight` column appended.
#'
#' @param x An `ipf_rake` object.
#' @param ... Additional arguments (ignored).
#'
#' @return A tibble with all original columns plus `.weight`.
#'
#' @examples
#' data <- data.frame(
#'   gender = sample(c('M', 'F'), 100, replace = TRUE, prob = c(0.6, 0.4))
#' )
#' targets <- list(gender = c(M = 0.5, F = 0.5))
#' result <- rake(data, targets)
#' augment(result)
#'
#' @importFrom generics augment
#' @exportS3Method
augment.ipf_rake <- function(x, ...) {
  out <- tibble::as_tibble(x$data)
  out$.weight <- x$weights
  out
}

#' @importFrom generics augment
#' @export
generics::augment
