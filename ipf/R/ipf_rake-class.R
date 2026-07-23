#' Construct an ipf_rake object
#'
#' @param weights Final raked weight vector.
#' @param data Original data frame.
#' @param converged Logical, whether convergence was achieved.
#' @param iterations Number of iterations.
#' @param max_prop_err Final maximum proportional error.
#' @param targets Normalized targets used.
#' @param vars_used Character vector of variables raked on.
#' @param base_weights Original base weights.
#' @param type Variable selection type.
#' @param choosemethod Discrepancy aggregation method.
#' @param na_method Missing-data handling method.
#' @param cap Weight cap value.
#' @param deff Design effect.
#' @param n_eff Effective sample size.
#' @param diagnostics Diagnostics tibble.
#'
#' @return An `ipf_rake` S3 object.
#'
#' @noRd
new_ipf_rake <- function(
  weights,
  data,
  converged,
  iterations,
  max_prop_err,
  targets,
  vars_used,
  base_weights,
  type,
  choosemethod,
  na_method,
  cap,
  deff,
  n_eff,
  diagnostics
) {
  structure(
    list(
      weights = weights,
      data = data,
      converged = converged,
      iterations = iterations,
      max_prop_err = max_prop_err,
      targets = targets,
      vars_used = vars_used,
      base_weights = base_weights,
      type = type,
      choosemethod = choosemethod,
      na_method = na_method,
      cap = cap,
      deff = deff,
      n_eff = n_eff,
      diagnostics = diagnostics
    ),
    class = 'ipf_rake'
  )
}

#' Print an ipf_rake object
#'
#' @param x An `ipf_rake` object.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns `x`.
#' @export
#'
#' @examples
#' data <- data.frame(
#'   gender = sample(c('M', 'F'), 100, replace = TRUE, prob = c(0.6, 0.4))
#' )
#' targets <- list(gender = c(M = 0.5, F = 0.5))
#' result <- rake(data, targets)
#' print(result)
print.ipf_rake <- function(x, ...) {
  n <- length(x$weights)
  ws <- weight_summary_rust(x$weights)

  conv_status <- if (x$converged) {
    cli::col_green('Yes')
  } else {
    cli::col_red('No')
  }

  cli::cli_h3('Raking result (ipf)')
  cli::cli_text(
    'Converged: {conv_status} ({x$iterations} iterations, max prop err = {signif(x$max_prop_err, 3)})'
  )
  cli::cli_text('Variables raked: {.val {x$vars_used}}')
  cli::cli_text('Missing handling: {.val {x$na_method}}')
  cli::cli_text(
    'Design effect: {round(x$deff, 3)} | Effective n: {round(x$n_eff)} / {n}'
  )
  cli::cli_text(
    'Weight range: [{round(ws$min, 3)}, {round(ws$max, 3)}] | Mean: {round(ws$mean, 3)} | SD: {round(ws$sd, 3)}'
  )

  invisible(x)
}

#' Summarize an ipf_rake object
#'
#' Produces a detailed summary including per-variable diagnostic tables showing target vs. achieved distributions.
#'
#' @param object An `ipf_rake` object.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns a list with convergence info, weight summary, design effect, and per-variable assessment tibbles.
#' @export
#'
#' @examples
#' data <- data.frame(
#'   gender = sample(c('M', 'F'), 100, replace = TRUE, prob = c(0.6, 0.4))
#' )
#' targets <- list(gender = c(M = 0.5, F = 0.5))
#' result <- rake(data, targets)
#' summary(result)
summary.ipf_rake <- function(object, ...) {
  ws <- weight_summary_rust(object$weights)
  assessment <- weight_assess(
    data = object$data,
    targets = object$targets,
    weights = object$weights,
    base_weights = object$base_weights,
    na_method = object$na_method
  )

  used_base <- !all(object$base_weights == 1)

  cli::cli_h3('Raking Summary (ipf)')
  cli::cli_rule()

  # Convergence
  if (object$converged) {
    cli::cli_alert_success(
      'Converged in {object$iterations} iterations (max prop err = {signif(object$max_prop_err, 3)})'
    )
  } else {
    cli::cli_alert_danger(
      'Did NOT converge after {object$iterations} iterations (max prop err = {signif(object$max_prop_err, 3)})'
    )
  }

  # Base weights
  if (used_base) {
    cli::cli_alert_info('Base weights were provided')
  } else {
    cli::cli_alert_info('No base weights (uniform)')
  }

  # Variable selection
  cli::cli_alert_info(
    'Selection: type = {.val {object$type}}, method = {.val {object$choosemethod}}'
  )
  cli::cli_alert_info('Missing handling: {.val {object$na_method}}')
  cli::cli_alert_info('Variables raked: {.val {object$vars_used}}')

  # Weight summary
  cli::cli_rule('Weight Summary')
  cli::cli_text(
    '  Min: {round(ws$min, 4)}  Q1: {round(ws$q1, 4)}  Median: {round(ws$median, 4)}  Mean: {round(ws$mean, 4)}  Q3: {round(ws$q3, 4)}  Max: {round(ws$max, 4)}'
  )
  cli::cli_text('  SD: {round(ws$sd, 4)}  CV: {round(ws$cv, 4)}')

  # Design effect
  cli::cli_rule('Design Effect')
  cli::cli_text(
    '  Deff: {round(object$deff, 4)}  |  Effective n: {round(object$n_eff)} / {length(object$weights)}'
  )

  # Per-variable tables
  cli::cli_rule('Per-Variable Assessment')
  for (v in names(assessment)) {
    cli::cli_h3(v)
    print(assessment[[v]])
    cat('\n')
  }

  invisible(list(
    convergence = list(
      converged = object$converged,
      iterations = object$iterations,
      max_prop_err = object$max_prop_err
    ),
    base_weights_used = used_base,
    na_method = object$na_method,
    vars_used = object$vars_used,
    weight_summary = ws,
    design_effect = list(deff = object$deff, n_eff = object$n_eff),
    assessment = assessment
  ))
}
