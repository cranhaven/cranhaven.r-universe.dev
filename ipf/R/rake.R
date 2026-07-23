#' Iterative proportional fitting (raking)
#'
#' Adjusts survey weights so that weighted marginal distributions match known population targets.
#' Supports automatic variable selection, iterative re-raking, and weight bounding.
#'
#' @param data A data frame or tibble containing the survey data.
#' @param targets A named list of named numeric vectors specifying target proportions for each raking variable.
#'   Names of the list must match column names in `data`.
#'   Each vector's names must match the levels of the corresponding variable.
#'   Values should sum to 1 (proportions); if not, they are normalized with a warning.
#' @param base_weights Optional numeric vector of base (design) weights.
#'   If `NULL` (default), uniform weights of 1 are used.
#'   Centered to mean 1 before raking.
#' @param cap Maximum weight value (ratio cap).
#'   Weights exceeding this value are trimmed and all weights are renormalized.
#'   Default `5`.
#'   Ignored if `bounds` is specified.
#' @param bounds Optional numeric vector of length 2, `c(lo, hi)`, specifying minimum and maximum weight bounds.
#'   Overrides `cap`.
#' @param type Variable selection method:
#'   - `"nolim"` (default): use all variables in `targets`.
#'   - `"pctlim"`: use only variables with discrepancy >= `pctlim`.
#'   - `"nlim"`: use the `nlim` most discrepant variables.
#' @param pctlim Discrepancy threshold for `type = "pctlim"`.
#'   Default `0.05` (5 percentage points).
#' @param nlim Number of variables for `type = "nlim"`.
#'   Default `5`.
#' @param choosemethod Method for aggregating per-category discrepancies into a single variable score.
#'   One of `"total"`, `"max"`, `"average"`, `"totalsquared"`, `"maxsquared"`, `"averagesquared"`.
#' @param na_method How to handle `NA` values in raking variables.
#'   `"exclude"` (default): targets are proportions among non-NA cases only; NA cases are
#'   invisible to that margin. Matches anesrake.
#'   `"bucket"`: NAs become a frozen extra category; their total weight is preserved and the
#'   named targets are rescaled to the remaining non-NA mass.
#' @param iterate Logical.
#'   If `TRUE` and `type = "pctlim"`, re-check discrepancies after raking and add newly discrepant variables, repeating up to 10 times.
#'   Default `TRUE`.
#' @param max_iter Maximum number of raking iterations.
#'   Default `1000`.
#' @param tol Convergence tolerance (max proportional error).
#'   Default `1e-6`.
#' @param verbose Logical.
#'   If `TRUE`, print iteration progress.
#'   Default `FALSE`.
#' @param diagnostics_every Record per-margin diagnostics every `k` iterations.
#'   `0` means only baseline.
#'   Default `0`.
#'
#' @return An `ipf_rake` object (S3 class) containing:
#'   - `weights`: final raked weight vector
#'   - `data`: the input data frame
#'   - `converged`: logical
#'   - `iterations`: number of iterations
#'   - `max_prop_err`: final max proportional error
#'   - `targets`: normalized targets used
#'   - `vars_used`: character vector of variables raked on
#'   - `base_weights`: original base weights
#'   - `type`, `choosemethod`, `na_method`, `cap`: settings used
#'   - `deff`, `n_eff`: design effect and effective sample size
#'   - `diagnostics`: tibble of per-iteration diagnostics
#' @export
#'
#' @examples
#' data <- data.frame(
#'   gender = sample(c('M', 'F'), 100, replace = TRUE, prob = c(0.6, 0.4)),
#'   age = sample(c('young', 'old'), 100, replace = TRUE, prob = c(0.7, 0.3))
#' )
#' targets <- list(
#'   gender = c(M = 0.5, F = 0.5),
#'   age = c(young = 0.6, old = 0.4)
#' )
#' result <- rake(data, targets)
#' print(result)
rake <- function(
  data,
  targets,
  base_weights = NULL,
  cap = 5,
  bounds = NULL,
  type = c('nolim', 'pctlim', 'nlim'),
  pctlim = 0.05,
  nlim = 5L,
  choosemethod = c(
    'total',
    'max',
    'average',
    'totalsquared',
    'maxsquared',
    'averagesquared'
  ),
  na_method = c('exclude', 'bucket'),
  iterate = TRUE,
  max_iter = 1000L,
  tol = 1e-6,
  verbose = FALSE,
  diagnostics_every = 0L
) {
  # Validation
  if (!is.data.frame(data)) {
    cli::cli_abort('{.arg data} must be a data frame.')
  }
  type <- match.arg(type)
  choosemethod <- match.arg(choosemethod)
  na_method <- match.arg(na_method)

  n <- nrow(data)

  # Base weights
  if (is.null(base_weights)) {
    base_weights <- rep(1.0, n)
  } else {
    if (is.character(base_weights) && length(base_weights) == 1) {
      if (!(base_weights %in% names(data))) {
        cli::cli_abort('Column {.var {base_weights}} not found in data.')
      }
      base_weights <- data[[base_weights]]
    }
    if (!is.numeric(base_weights) || length(base_weights) != n) {
      cli::cli_abort(
        '{.arg base_weights} must be a numeric vector of length {n}.'
      )
    }
    if (anyNA(base_weights) || any(base_weights <= 0)) {
      cli::cli_abort('{.arg base_weights} must be positive and non-NA.')
    }
  }

  # Center base weights to mean 1
  base_weights <- base_weights / mean(base_weights)

  # Targets
  targets <- encode_targets(targets, data)

  # Bounds
  if (!is.null(bounds)) {
    bounds <- as.numeric(bounds)
    if (length(bounds) != 2) {
      cli::cli_abort('{.arg bounds} must be numeric of length 2.')
    }
  } else if (!is.null(cap)) {
    bounds <- c(-Inf, cap)
  }

  # Only apply finite bounds
  bounds_for_rust <- if (!is.null(bounds) && any(is.finite(bounds))) {
    bounds
  } else {
    NULL
  }

  # Variable selection
  active_targets <- targets
  if (type == 'pctlim') {
    disc_scores <- find_discrepant_vars(
      data,
      targets,
      base_weights,
      choosemethod,
      na_method = na_method
    )
    selected <- disc_scores[disc_scores >= pctlim]
    if (length(selected) == 0) {
      cli::cli_warn(
        'No variables exceed {.arg pctlim} threshold of {.val {pctlim}}. Using all variables.'
      )
      active_targets <- targets
    } else {
      active_targets <- targets[names(selected)]
    }
  } else if (type == 'nlim') {
    disc_scores <- find_discrepant_vars(
      data,
      targets,
      base_weights,
      choosemethod,
      na_method = na_method
    )
    top_n <- utils::head(sort(disc_scores, decreasing = TRUE), nlim)
    active_targets <- targets[names(top_n)]
  }

  # Encode margins for Rust
  grand_total <- sum(base_weights)
  rust_margins <- encode_margins_for_rust(
    data,
    active_targets,
    base_weights,
    na_method = na_method
  )

  # Call Rust core
  res <- rake_ipf_rust(
    weights = base_weights,
    margins = rust_margins,
    max_iter = as.integer(max_iter),
    tol = tol,
    bounds = bounds_for_rust,
    grand_total = grand_total,
    diagnostics_every = as.integer(diagnostics_every),
    verbose = isTRUE(verbose)
  )

  final_weights <- as.numeric(res$weights)
  vars_used <- names(active_targets)

  # Iterative re-raking (pctlim only)
  if (isTRUE(iterate) && type == 'pctlim') {
    for (iter_round in seq_len(10)) {
      new_disc <- find_discrepant_vars(
        data,
        targets,
        final_weights,
        choosemethod,
        na_method = na_method
      )
      new_vars <- setdiff(names(new_disc[new_disc >= pctlim]), vars_used)
      if (length(new_vars) == 0) {
        break
      }

      if (verbose) {
        cli::cli_inform(
          'Iteration round {iter_round}: adding {length(new_vars)} variable(s).'
        )
      }

      for (nv in new_vars) {
        active_targets[[nv]] <- targets[[nv]]
      }
      vars_used <- names(active_targets)

      rust_margins <- encode_margins_for_rust(
        data,
        active_targets,
        final_weights,
        na_method = na_method
      )
      grand_total <- sum(final_weights)

      res <- rake_ipf_rust(
        weights = final_weights,
        margins = rust_margins,
        max_iter = as.integer(max_iter),
        tol = tol,
        bounds = bounds_for_rust,
        grand_total = grand_total,
        diagnostics_every = as.integer(diagnostics_every),
        verbose = isTRUE(verbose)
      )
      final_weights <- as.numeric(res$weights)
    }
  }

  # Design effect
  de <- design_effect_rust(final_weights)

  # Diagnostics tibble
  diag <- tibble::tibble(
    iteration = as.integer(res$diagnostics$iteration),
    margin_index = as.integer(res$diagnostics$margin_index),
    level_index = as.integer(res$diagnostics$level_index),
    target = as.numeric(res$diagnostics$target),
    current = as.numeric(res$diagnostics$current),
    prop_err = as.numeric(res$diagnostics$prop_err)
  )

  # Build output
  new_ipf_rake(
    weights = final_weights,
    data = data,
    converged = isTRUE(res$converged),
    iterations = as.integer(res$iterations),
    max_prop_err = as.numeric(res$max_prop_err),
    targets = active_targets,
    vars_used = vars_used,
    base_weights = as.numeric(res$prevec),
    type = type,
    choosemethod = choosemethod,
    na_method = na_method,
    cap = cap,
    deff = as.numeric(de$deff),
    n_eff = as.numeric(de$n_eff),
    diagnostics = diag
  )
}

#' Encode margins list for the Rust raking engine
#'
#' @param data Data frame.
#' @param targets Named list of named numeric targets (proportions, sum to 1).
#' @param weights Current weight vector.
#'
#' @return List of margin lists, each with `$levels` and `$targets`.
#'
#' @noRd
encode_margins_for_rust <- function(
  data,
  targets,
  weights,
  na_method = c('exclude', 'bucket')
) {
  na_method <- match.arg(na_method)

  lapply(names(targets), function(v) {
    tgt <- targets[[v]]
    enc <- encode_variable(
      data[[v]],
      names(tgt),
      var_name = v,
      na_method = na_method
    )

    if (na_method == 'exclude') {
      target_vec <- stats::setNames(numeric(length(enc$level_names)), enc$level_names)
      target_vec[names(tgt)] <- as.numeric(tgt)
      list(
        levels = enc$codes,
        targets = as.numeric(target_vec),
        proportional = TRUE
      )
    } else {
      tgt_totals <- build_margin_targets(
        target = tgt,
        level_names = enc$level_names,
        codes = enc$codes,
        weights = weights,
        na_method = na_method,
        output = 'total'
      )
      list(
        levels = enc$codes,
        targets = as.numeric(tgt_totals),
        proportional = FALSE
      )
    }
  })
}
