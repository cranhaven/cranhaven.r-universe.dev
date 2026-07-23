#' Compute design effect and effective sample size
#'
#' The design effect (deff) measures the variance inflation factor due to unequal weighting.
#' The effective sample size is `n / deff`.
#'
#' @param weights Numeric weight vector.
#'
#' @return A list with `deff` (design effect) and `n_eff` (effective sample size).
#' @export
#'
#' @examples
#' w <- c(1.2, 0.8, 1.5, 0.5, 1.0)
#' design_effect(w)
design_effect <- function(weights) {
  if (!is.numeric(weights)) {
    cli::cli_abort('{.arg weights} must be numeric.')
  }
  design_effect_rust(weights)
}

#' Assess weight quality with diagnostic tables
#'
#' Produces a per-variable diagnostic table comparing target distributions to unweighted and weighted distributions.
#'
#' @param data Data frame.
#' @param targets Named list of named numeric target vectors (proportions).
#' @param weights Final raked weight vector.
#' @param base_weights Original base weights before raking.
#'   If `NULL`, uses uniform weights.
#' @param na_method How to handle `NA` values.
#'   `"exclude"` skips NA cases from that margin.
#'   `"bucket"` treats missing values as an implicit extra category.
#'
#' @return Named list of tibbles, one per variable.
#' @export
#'
#' @examples
#' data <- data.frame(
#'   gender = sample(c('M', 'F'), 100, replace = TRUE, prob = c(0.6, 0.4))
#' )
#' targets <- list(gender = c(M = 0.5, F = 0.5))
#' result <- rake(data, targets)
#' weight_assess(data, targets, result$weights)
weight_assess <- function(
  data,
  targets,
  weights,
  base_weights = NULL,
  na_method = c('exclude', 'bucket')
) {
  na_method <- match.arg(na_method)

  if (is.null(base_weights)) {
    base_weights <- rep(1, nrow(data))
  }

  results <- list()

  for (v in names(targets)) {
    tgt <- targets[[v]]
    enc <- encode_variable(
      data[[v]],
      names(tgt),
      var_name = v,
      na_method = na_method
    )
    pre_target <- build_margin_targets(
      target = tgt,
      level_names = enc$level_names,
      codes = enc$codes,
      weights = base_weights,
      na_method = na_method,
      output = 'proportion'
    )
    post_target <- build_margin_targets(
      target = tgt,
      level_names = enc$level_names,
      codes = enc$codes,
      weights = weights,
      na_method = na_method,
      output = 'proportion'
    )

    # Unweighted (base weights)
    pre <- compute_discrepancy_rust(base_weights, enc$codes, pre_target)
    # Weighted (raked weights)
    post <- compute_discrepancy_rust(weights, enc$codes, post_target)

    level_names <- enc$level_names
    n_levels <- length(level_names)

    # Weighted N
    pre_n <- numeric(n_levels)
    post_n <- numeric(n_levels)
    for (li in seq_along(level_names)) {
      mask <- enc$codes == li
      pre_n[li] <- sum(base_weights[mask])
      post_n[li] <- sum(weights[mask])
    }

    tbl <- tibble::tibble(
      level = level_names,
      target = as.numeric(post_target),
      unweighted_n = pre_n,
      unweighted_pct = as.numeric(pre$weighted_pct),
      weighted_n = post_n,
      weighted_pct = as.numeric(post$weighted_pct),
      change_pct = as.numeric(post$weighted_pct) - as.numeric(pre$weighted_pct),
      residual_disc = as.numeric(post$discrepancy),
      original_disc = as.numeric(pre$discrepancy)
    )

    # Total row
    total_row <- tibble::tibble(
      level = 'Total',
      target = sum(tbl$target),
      unweighted_n = sum(tbl$unweighted_n),
      unweighted_pct = sum(tbl$unweighted_pct),
      weighted_n = sum(tbl$weighted_n),
      weighted_pct = sum(tbl$weighted_pct),
      change_pct = sum(abs(tbl$change_pct)),
      residual_disc = sum(abs(tbl$residual_disc)),
      original_disc = sum(abs(tbl$original_disc))
    )

    results[[v]] <- rbind(tbl, total_row)
  }

  results
}
