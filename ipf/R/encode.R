#' Encode a variable to integer codes for the Rust raking engine
#'
#' Converts factor, character, logical, or integer variables to 1-based integer codes.
#' NAs either become 0 (skipped by the Rust core) or an explicit missing bucket.
#'
#' @param x A vector from the data frame.
#' @param target_names Character vector of expected level names from the target.
#' @param var_name Name of the variable (for error messages).
#' @param na_method How to handle `NA` values.
#'   `"exclude"` skips them from that margin.
#'   `"bucket"` treats missing values as an implicit extra category.
#'
#' @return A list with:
#'   - `codes`: integer vector (0 = NA, 1..L = categories)
#'   - `level_names`: character vector of level names in order
#'
#' @noRd
encode_variable <- function(
  x,
  target_names,
  var_name = 'variable',
  na_method = c('exclude', 'bucket')
) {
  na_method <- match.arg(na_method)

  if (is.logical(x)) {
    x <- factor(x, levels = c(FALSE, TRUE), labels = c('FALSE', 'TRUE'))
  } else if (is.character(x)) {
    x <- factor(x)
  } else if (is.numeric(x) && !is.factor(x)) {
    x <- factor(x)
  }

  if (!is.factor(x)) {
    cli::cli_abort(
      '{.arg {var_name}} must be a factor, character, logical, or integer vector.'
    )
  }

  lvls <- levels(x)

  # Validate target names match data levels
  extra_in_target <- setdiff(target_names, lvls)
  extra_in_data <- setdiff(lvls, target_names)

  if (length(extra_in_data) > 0) {
    cli::cli_abort(c(
      'Variable {.var {var_name}} has levels not in targets: {.val {extra_in_data}}.',
      'i' = 'All data levels must have corresponding target values.'
    ))
  }

  if (length(extra_in_target) > 0) {
    cli::cli_warn(
      'Variable {.var {var_name}}: target levels {.val {extra_in_target}} have no observations in data (zero-cell).'
    )
  }

  # Reorder factor to match target_names order
  all_levels <- union(target_names, lvls)
  if (na_method == 'bucket' && anyNA(x)) {
    all_levels <- c(all_levels, '(Missing)')
    x <- as.character(x)
    x[is.na(x)] <- '(Missing)'
    x <- factor(x, levels = all_levels)
  } else {
    x <- factor(x, levels = all_levels)
  }

  codes <- as.integer(x)
  if (na_method == 'exclude') {
    codes[is.na(codes)] <- 0L
  }

  list(
    codes = codes,
    level_names = all_levels
  )
}

build_margin_targets <- function(
  target,
  level_names,
  codes,
  weights,
  na_method = c('exclude', 'bucket'),
  output = c('proportion', 'total')
) {
  na_method <- match.arg(na_method)
  output <- match.arg(output)

  target_vec <- stats::setNames(numeric(length(level_names)), level_names)
  target_vec[names(target)] <- as.numeric(target)

  if (na_method == 'bucket' && '(Missing)' %in% level_names) {
    missing_index <- match('(Missing)', level_names)
    missing_weight <- sum(weights[codes == missing_index])
    observed_total <- sum(weights) - missing_weight

    if (output == 'proportion') {
      grand_total <- sum(weights)
      if (grand_total > 0) {
        target_vec[names(target)] <- as.numeric(target) *
          (observed_total / grand_total)
        target_vec[missing_index] <- missing_weight / grand_total
      }
    } else {
      target_vec[names(target)] <- as.numeric(target) * observed_total
      target_vec[missing_index] <- missing_weight
    }
  } else if (output == 'total') {
    target_vec <- target_vec * sum(weights)
  }

  unname(target_vec)
}

#' Validate and normalize target distributions
#'
#' Ensures targets are named numeric vectors that sum to 1 (proportions).
#' Normalizes them if they appear to be counts or percentages.
#'
#' @param targets Named list of named numeric vectors.
#' @param data Data frame to validate against.
#'
#' @return Normalized targets list (each summing to 1).
#'
#' @noRd
encode_targets <- function(targets, data) {
  if (!is.list(targets) || is.null(names(targets))) {
    cli::cli_abort(
      '{.arg targets} must be a named list of named numeric vectors.'
    )
  }

  var_names <- names(targets)

  for (v in var_names) {
    tgt <- targets[[v]]

    if (!is.numeric(tgt)) {
      cli::cli_abort('Target for {.var {v}} must be numeric.')
    }
    if (is.null(names(tgt))) {
      cli::cli_abort('Target for {.var {v}} must be a named vector.')
    }
    if (any(tgt < 0, na.rm = TRUE)) {
      cli::cli_abort('Target for {.var {v}} contains negative values.')
    }
    if (!(v %in% names(data))) {
      cli::cli_abort('Variable {.var {v}} not found in data.')
    }

    s <- sum(tgt, na.rm = TRUE)
    if (s <= 0) {
      cli::cli_abort('Targets for {.var {v}} sum to zero or negative.')
    }

    # Normalize to proportions
    if (abs(s - 1) > 1e-6) {
      cli::cli_warn(
        'Targets for {.var {v}} sum to {.val {round(s, 4)}}, not 1. Normalizing to proportions.'
      )
      targets[[v]] <- tgt / s
    }
  }

  targets
}
