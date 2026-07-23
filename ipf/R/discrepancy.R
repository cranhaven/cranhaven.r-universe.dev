#' Compute discrepancy between weighted distribution and targets
#'
#' @param data Data frame containing the variables.
#' @param targets Named list of named numeric target vectors (proportions).
#' @param weights Numeric weight vector.
#' @param var_name Single variable name to compute discrepancy for.
#'   If `NULL`, computes for all variables in `targets`.
#' @param na_method How to handle `NA` values.
#'   `"exclude"` skips NA cases from that margin.
#'   `"bucket"` treats missing values as an implicit extra category.
#'
#' @return Named list with `weighted_pct` and `discrepancy` vectors per variable.
#'
#' @noRd
compute_discrepancy <- function(
  data,
  targets,
  weights,
  var_name = NULL,
  na_method = c('exclude', 'bucket')
) {
  na_method <- match.arg(na_method)
  vars <- if (is.null(var_name)) names(targets) else var_name

  results <- lapply(stats::setNames(vars, vars), function(v) {
    enc <- encode_variable(
      data[[v]],
      names(targets[[v]]),
      var_name = v,
      na_method = na_method
    )
    tgt_ordered <- build_margin_targets(
      target = targets[[v]],
      level_names = enc$level_names,
      codes = enc$codes,
      weights = weights,
      na_method = na_method,
      output = 'proportion'
    )

    compute_discrepancy_rust(
      weights = weights,
      levels = enc$codes,
      targets = tgt_ordered
    )
  })

  results
}

#' Find discrepant variables and their aggregate discrepancy scores
#'
#' Calculates discrepancy between the current weighted distribution and target distributions for each variable, then aggregates using the chosen method.
#'
#' @param data Data frame.
#' @param targets Named list of named numeric target vectors (proportions).
#' @param weights Numeric weight vector.
#' @param choosemethod Method for aggregating per-category discrepancies.
#'   One of `"total"`, `"max"`, `"average"`, `"totalsquared"`, `"maxsquared"`, `"averagesquared"`.
#' @param na_method How to handle `NA` values.
#'   `"exclude"` skips NA cases from that margin.
#'   `"bucket"` treats missing values as an implicit extra category.
#'
#' @return Named numeric vector of aggregate discrepancy per variable.
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
#' find_discrepant_vars(data, targets, weights = rep(1, 100))
find_discrepant_vars <- function(
  data,
  targets,
  weights,
  choosemethod = 'total',
  na_method = c('exclude', 'bucket')
) {
  valid_methods <- c(
    'total',
    'max',
    'average',
    'totalsquared',
    'maxsquared',
    'averagesquared'
  )
  choosemethod <- match.arg(choosemethod, valid_methods)
  na_method <- match.arg(na_method)

  disc_list <- compute_discrepancy(
    data,
    targets,
    weights,
    na_method = na_method
  )

  vapply(
    disc_list,
    function(d) {
      disc <- d$discrepancy
      switch(
        choosemethod,
        total = sum(abs(disc)),
        max = max(abs(disc)),
        average = mean(abs(disc)),
        totalsquared = sum(disc^2),
        maxsquared = max(disc^2),
        averagesquared = mean(disc^2)
      )
    },
    numeric(1)
  )
}
