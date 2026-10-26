# ==============================================================================
# Balance Diagnostics for Matching Results
# ==============================================================================
# Functions to assess the quality of matches by comparing distributions
# of matching variables between left and right units in matched pairs

# Weighted mean and (unbiased) weighted variance/sd. With all weights equal to
# 1 this reduces exactly to mean() and the n-1 sample variance, so unweighted
# callers are unaffected.
.weighted_moments <- function(x, w) {
  if (length(x) == 0) {
    return(list(mean = NA_real_, var = NA_real_, sd = NA_real_))
  }
  sw <- sum(w)
  if (sw == 0) {
    return(list(mean = NA_real_, var = NA_real_, sd = NA_real_))
  }
  m <- sum(w * x) / sw
  denom <- sw^2 - sum(w^2)
  v <- if (denom > 0) sw / denom * sum(w * (x - m)^2) else NA_real_
  list(mean = m, var = v, sd = sqrt(v))
}

#' Calculate Standardized Difference
#'
#' Computes the standardized mean difference between two groups.
#' This is a key metric for assessing balance in matched samples.
#'
#' @param x1 Numeric vector for group 1
#' @param x2 Numeric vector for group 2
#' @param pooled Logical, if TRUE use pooled standard deviation (default),
#'   if FALSE use group 1 standard deviation
#'
#' @return Numeric value representing the standardized difference
#'
#' @details
#' Standardized difference = (mean1 - mean2) / pooled_sd
#' where pooled_sd = sqrt((sd1^2 + sd2^2) / 2)
#'
#' Common thresholds: less than 0.1 is excellent balance, 0.1-0.25 is good
#' balance, 0.25-0.5 is acceptable balance, and greater than 0.5 is poor balance.
#'
#' @keywords internal
standardized_difference <- function(x1, x2, pooled = TRUE, w1 = NULL, w2 = NULL) {
  if (is.null(w1)) w1 <- rep(1, length(x1))
  if (is.null(w2)) w2 <- rep(1, length(x2))

  # Remove NA values (in tandem with their weights)
  ok1 <- !is.na(x1) & !is.na(w1)
  ok2 <- !is.na(x2) & !is.na(w2)
  x1 <- x1[ok1]; w1 <- w1[ok1]
  x2 <- x2[ok2]; w2 <- w2[ok2]

  # Handle edge cases
  if (length(x1) == 0 || length(x2) == 0) {
    return(NA_real_)
  }

  m1 <- .weighted_moments(x1, w1)
  m2 <- .weighted_moments(x2, w2)
  mean_diff <- m1$mean - m2$mean

  if (pooled) {
    pooled_sd <- sqrt((m1$var + m2$var) / 2)
  } else {
    pooled_sd <- m1$sd
  }

  # Avoid division by zero
  if (is.na(pooled_sd) || pooled_sd == 0) {
    return(0)
  }

  return(mean_diff / pooled_sd)
}


#' Calculate Variable-Level Balance Statistics
#'
#' @param left_vals Numeric vector of values from left group
#' @param right_vals Numeric vector of values from right group
#' @param var_name Character, name of the variable
#' @param w_left Optional numeric vector of weights for \code{left_vals}
#'   (e.g. stratum weights from full matching, CEM, or subclassification).
#'   Defaults to equal weights.
#' @param w_right Optional numeric vector of weights for \code{right_vals}.
#'
#' @return List with balance statistics for this variable
#'
#' @keywords internal
calculate_var_balance <- function(left_vals, right_vals, var_name,
                                  w_left = NULL, w_right = NULL) {
  if (is.null(w_left)) w_left <- rep(1, length(left_vals))
  if (is.null(w_right)) w_right <- rep(1, length(right_vals))

  # Remove NAs (in tandem with their weights)
  okL <- !is.na(left_vals) & !is.na(w_left)
  okR <- !is.na(right_vals) & !is.na(w_right)
  left_clean <- left_vals[okL];  wl <- w_left[okL]
  right_clean <- right_vals[okR]; wr <- w_right[okR]

  # Weighted moments (reduce to plain mean/sd when weights are all 1)
  mL <- .weighted_moments(left_clean, wl)
  mR <- .weighted_moments(right_clean, wr)
  mean_left <- mL$mean
  mean_right <- mR$mean
  mean_diff <- mean_left - mean_right

  sd_left <- mL$sd
  sd_right <- mR$sd

  # Standardized difference
  std_diff <- standardized_difference(left_clean, right_clean, pooled = TRUE,
                                      w1 = wl, w2 = wr)

  # Variance ratio (left / right), the conventional matching-balance diagnostic
  var_ratio <- if (!is.na(sd_right) && sd_right > 0) (sd_left / sd_right)^2 else NA_real_

  # Kolmogorov-Smirnov test for distribution similarity (unweighted)
  ks_result <- tryCatch({
    stats::ks.test(left_clean, right_clean)
  }, error = function(e) NULL)

  ks_statistic <- if (!is.null(ks_result)) ks_result$statistic else NA_real_
  ks_pvalue <- if (!is.null(ks_result)) ks_result$p.value else NA_real_

  # Return statistics
  list(
    variable = var_name,
    mean_left = mean_left,
    mean_right = mean_right,
    mean_diff = mean_diff,
    sd_left = sd_left,
    sd_right = sd_right,
    std_diff = std_diff,
    var_ratio = var_ratio,
    ks_statistic = ks_statistic,
    ks_pvalue = ks_pvalue,
    n_left = length(left_clean),
    n_right = length(right_clean)
  )
}


#' Summarise a per-variable balance table
#'
#' The three headline numbers every balance object reports, read off the
#' per-variable table each of them builds.
#'
#' @param var_stats A per-variable balance table, as
#'   \code{calculate_var_balance()} rows bound together.
#' @param n_vars How many variables the balance was asked about. This is the
#'   number asked for rather than \code{nrow(var_stats)}, which is smaller when
#'   a variable produced no statistics.
#'
#' @return List with \code{mean_abs_std_diff}, \code{max_abs_std_diff},
#'   \code{pct_large_imbalance} and \code{n_vars}.
#'
#' @keywords internal
.overall_balance <- function(var_stats, n_vars) {
  abs_std_diffs <- abs(var_stats$std_diff)
  list(
    mean_abs_std_diff = mean(abs_std_diffs, na.rm = TRUE),
    max_abs_std_diff = if (all(is.na(abs_std_diffs))) NA_real_ else
      max(abs_std_diffs, na.rm = TRUE),
    pct_large_imbalance = mean(abs_std_diffs > 0.25, na.rm = TRUE) * 100,
    n_vars = n_vars
  )
}


#' Balance Diagnostics for Matched Pairs
#'
#' Computes comprehensive balance statistics comparing the distribution of
#' matching variables between left and right units in the matched sample.
#'
#' @param result A matching result object from \code{match_couples()},
#'   \code{full_match()}, \code{cem_match()}, or
#'   \code{subclass_match()}
#' @param left Data frame of left units
#' @param right Data frame of right units
#' @param vars Character vector of variable names to check balance for.
#'   Defaults to the variables used in matching (if available in result).
#' @param left_id Character, name of ID column in left data (default: "id")
#' @param right_id Character, name of ID column in right data (default: "id")
#' @param ... Additional arguments passed to methods
#'
#' @return An S3 object of class \code{balance_diagnostics} containing:
#' \describe{
#'   \item{var_stats}{Tibble with per-variable balance statistics}
#'   \item{overall}{List with overall balance metrics}
#'   \item{pairs}{Tibble of matched pairs with variables}
#'   \item{n_matched}{Number of matched pairs}
#'   \item{n_unmatched_left}{Number of unmatched left units}
#'   \item{n_unmatched_right}{Number of unmatched right units}
#'   \item{method}{Matching method used}
#'   \item{has_blocks}{Whether blocking was used}
#'   \item{block_stats}{Per-block statistics (if blocking used)}
#' }
#'
#' @details
#' This function computes several balance metrics:
#'
#' Standardized Difference: The difference in means divided by the pooled
#' standard deviation. Values less than 0.1 indicate excellent balance,
#' 0.1-0.25 good balance.
#'
#' Variance Ratio: The ratio of variances (left/right). Values close to 1
#' are ideal; the conventional acceptable range is 0.5 to 2. For weighted
#' estimators (full matching, CEM, subclassification) the means and variances
#' are computed using the stratum weights.
#'
#' KS Statistic: Kolmogorov-Smirnov test statistic comparing distributions.
#' Lower values indicate more similar distributions.
#'
#' Overall Metrics include mean absolute standardized difference across
#' all variables, proportion of variables with large imbalance
#' (|std diff| > 0.25), and maximum standardized difference.
#'
#' @examples
#' # Create sample data
#' set.seed(123)
#' left <- data.frame(
#'   id = 1:10,
#'   age = rnorm(10, 45, 10),
#'   income = rnorm(10, 50000, 15000)
#' )
#' right <- data.frame(
#'   id = 11:30,
#'   age = rnorm(20, 47, 10),
#'   income = rnorm(20, 52000, 15000)
#' )
#'
#' # Match
#' result <- match_couples(left, right, vars = c("age", "income"))
#'
#' # Get balance diagnostics
#' balance <- balance_diagnostics(result, left, right, vars = c("age", "income"))
#' print(balance)
#'
#' # Get balance table
#' balance_table(balance)
#'
#' @export
balance_diagnostics <- function(result, ...) {
  UseMethod("balance_diagnostics")
}


#' @export
balance_diagnostics.default <- function(result, ...) {
  stop("result must be a matching_result object from match_couples()")
}


#' @rdname balance_diagnostics
#' @export
balance_diagnostics.matching_result <- function(result,
                                                left,
                                                right,
                                                vars = NULL,
                                                left_id = "id",
                                                right_id = "id",
                                                ...) {

  if (!left_id %in% names(left)) {
    stop("left_id '", left_id, "' not found in left data")
  }

  if (!right_id %in% names(right)) {
    stop("right_id '", right_id, "' not found in right data")
  }

  # If vars not specified, try to get from result
  if (is.null(vars)) {
    if (!is.null(result$info$vars)) {
      vars <- result$info$vars
    } else {
      stop("vars must be specified (could not infer from result)")
    }
  }

  # Validate vars exist
  missing_left <- setdiff(vars, names(left))
  missing_right <- setdiff(vars, names(right))

  if (length(missing_left) > 0) {
    stop("Variables not found in left: ", paste(missing_left, collapse = ", "))
  }

  if (length(missing_right) > 0) {
    stop("Variables not found in right: ", paste(missing_right, collapse = ", "))
  }

  # Extract matched pairs
  pairs <- result$pairs

  # Filter to only matched pairs (right_id > 0)
  matched_pairs <- pairs[pairs[[2]] > 0, ]
  n_matched <- nrow(matched_pairs)

  # Count unmatched. Count distinct matched right units (not pair rows), since
  # with ratio > 1 or replacement a right unit can appear in several pairs.
  n_unmatched_left <- sum(pairs[[2]] == 0)
  n_total_left <- nrow(left)
  n_total_right <- nrow(right)
  n_matched_right <- length(unique(matched_pairs[[2]]))
  n_unmatched_right <- n_total_right - n_matched_right

  # Join variables for matched pairs
  left_matched <- merge(
    matched_pairs[, 1, drop = FALSE],
    left[, c(left_id, vars), drop = FALSE],
    by.x = names(matched_pairs)[1],
    by.y = left_id,
    all.x = TRUE
  )

  right_matched <- merge(
    matched_pairs[, 2, drop = FALSE],
    right[, c(right_id, vars), drop = FALSE],
    by.x = names(matched_pairs)[2],
    by.y = right_id,
    all.x = TRUE
  )

  # Calculate balance for each variable
  var_stats_list <- lapply(vars, function(v) {
    left_vals <- left_matched[[v]]
    right_vals <- right_matched[[v]]
    calculate_var_balance(left_vals, right_vals, v)
  })

  # Convert to tibble (use bind_rows for robust column name preservation)
  var_stats <- dplyr::bind_rows(lapply(var_stats_list, tibble::as_tibble))

  overall <- .overall_balance(var_stats, length(vars))

  # Check for blocking
  has_blocks <- "block_id" %in% names(pairs)
  block_stats <- NULL

  if (has_blocks) {
    # Calculate per-block statistics
    blocks <- unique(matched_pairs$block_id)
    blocks <- blocks[!is.na(blocks)]

    block_stats_list <- lapply(blocks, function(b) {
      block_pairs <- matched_pairs[matched_pairs$block_id == b, ]

      # Get left and right values for this block
      left_block <- merge(
        block_pairs[, 1, drop = FALSE],
        left[, c(left_id, vars), drop = FALSE],
        by.x = names(block_pairs)[1],
        by.y = left_id
      )

      right_block <- merge(
        block_pairs[, 2, drop = FALSE],
        right[, c(right_id, vars), drop = FALSE],
        by.x = names(block_pairs)[2],
        by.y = right_id
      )

      # Calculate mean std diff for this block
      block_var_stats <- lapply(vars, function(v) {
        calculate_var_balance(left_block[[v]], right_block[[v]], v)
      })

      mean_std_diff <- mean(abs(sapply(block_var_stats, function(x) x$std_diff)), na.rm = TRUE)

      # Determine quality (handle NA/NaN from empty blocks)
      quality <- if (is.na(mean_std_diff) || is.nan(mean_std_diff)) {
        "Unknown"
      } else if (mean_std_diff < 0.1) {
        "Excellent"
      } else if (mean_std_diff < 0.25) {
        "Good"
      } else if (mean_std_diff < 0.5) {
        "Fair"
      } else {
        "Poor"
      }

      # Find worst variable
      std_diffs <- sapply(block_var_stats, function(x) abs(x$std_diff))
      worst_var_idx <- which.max(std_diffs)
      worst_var <- if (length(worst_var_idx) > 0) vars[worst_var_idx] else NA_character_

      data.frame(
        block_id = b,
        n_pairs = nrow(block_pairs),
        mean_std_diff = mean_std_diff,
        quality = quality,
        worst_var = worst_var,
        stringsAsFactors = FALSE
      )
    })

    block_stats <- dplyr::bind_rows(block_stats_list)
  }

  # Build result object
  result_obj <- list(
    var_stats = var_stats,
    overall = overall,
    pairs = matched_pairs,
    n_matched = n_matched,
    n_unmatched_left = n_unmatched_left,
    n_unmatched_right = n_unmatched_right,
    n_total_left = n_total_left,
    n_total_right = n_total_right,
    method = result$info$method %||% "unknown",
    has_blocks = has_blocks,
    block_stats = block_stats
  )

  class(result_obj) <- "balance_diagnostics"
  return(result_obj)
}


#' @rdname balance_diagnostics
#' @export
balance_diagnostics.full_matching_result <- function(result,
                                                     left, right,
                                                     vars = NULL,
                                                     left_id = "id",
                                                     right_id = "id",
                                                     ...) {
  if (is.null(vars)) vars <- result$info$vars
  if (is.null(vars)) stop("vars must be specified", call. = FALSE)

  groups <- result$groups
  left_in <- groups[groups$side == "left", ]
  right_in <- groups[groups$side == "right", ]

  left_matched <- left[as.character(left[[left_id]]) %in% left_in$id, ]
  right_matched <- right[as.character(right[[right_id]]) %in% right_in$id, ]

  # Weighted balance: apply the full-matching group weights on both sides.
  left_weights <- stats::setNames(left_in$weight, left_in$id)
  right_weights <- stats::setNames(right_in$weight, right_in$id)

  var_stats_list <- lapply(vars, function(v) {
    lv <- left_matched[[v]]
    rv <- right_matched[[v]]
    lw <- unname(left_weights[as.character(left_matched[[left_id]])])
    rw <- unname(right_weights[as.character(right_matched[[right_id]])])
    calculate_var_balance(lv, rv, v, w_left = lw, w_right = rw)
  })

  var_stats <- dplyr::bind_rows(lapply(var_stats_list, tibble::as_tibble))
  overall <- .overall_balance(var_stats, length(vars))

  result_obj <- list(
    var_stats = var_stats,
    overall = overall,
    pairs = NULL,
    n_matched = nrow(left_matched) + nrow(right_matched),
    n_unmatched_left = result$info$n_unmatched_left,
    n_unmatched_right = result$info$n_unmatched_right,
    n_total_left = result$info$n_left,
    n_total_right = result$info$n_right,
    method = "full",
    has_blocks = FALSE,
    block_stats = NULL
  )

  class(result_obj) <- "balance_diagnostics"
  result_obj
}


#' @rdname balance_diagnostics
#' @export
balance_diagnostics.cem_result <- function(result,
                                           left, right,
                                           vars = NULL,
                                           left_id = "id",
                                           right_id = "id",
                                           ...) {
  if (is.null(vars)) vars <- result$info$vars
  if (is.null(vars)) stop("vars must be specified", call. = FALSE)

  matched <- result$matched
  left_side <- matched[matched$side == "left" & matched$weight > 0, ]
  right_side <- matched[matched$side == "right" & matched$weight > 0, ]

  left_matched <- left[as.character(left[[left_id]]) %in% left_side$id, ]
  right_matched <- right[as.character(right[[right_id]]) %in% right_side$id, ]

  # CEM stratum weights, applied on both sides.
  left_weights <- stats::setNames(left_side$weight, left_side$id)
  right_weights <- stats::setNames(right_side$weight, right_side$id)

  var_stats_list <- lapply(vars, function(v) {
    lw <- unname(left_weights[as.character(left_matched[[left_id]])])
    rw <- unname(right_weights[as.character(right_matched[[right_id]])])
    calculate_var_balance(left_matched[[v]], right_matched[[v]], v,
                          w_left = lw, w_right = rw)
  })

  var_stats <- dplyr::bind_rows(lapply(var_stats_list, tibble::as_tibble))
  overall <- .overall_balance(var_stats, length(vars))

  result_obj <- list(
    var_stats = var_stats,
    overall = overall,
    pairs = NULL,
    n_matched = nrow(left_matched) + nrow(right_matched),
    n_unmatched_left = result$info$n_pruned_left,
    n_unmatched_right = result$info$n_pruned_right,
    n_total_left = result$info$n_matched_left + result$info$n_pruned_left,
    n_total_right = result$info$n_matched_right + result$info$n_pruned_right,
    method = "cem",
    has_blocks = FALSE,
    block_stats = NULL
  )

  class(result_obj) <- "balance_diagnostics"
  result_obj
}


#' @rdname balance_diagnostics
#' @param data Data frame used for subclassification (when \code{result} is a
#'   subclass_result)
#' @export
balance_diagnostics.subclass_result <- function(result,
                                                data = NULL,
                                                vars = NULL,
                                                ...) {
  if (is.null(vars)) vars <- result$info$vars
  if (is.null(vars) && !is.null(data)) {
    # Infer from data (exclude treatment, id, etc.)
    exclude <- c("id", "treatment", "weights", "subclass", "ps", "weight")
    vars <- setdiff(names(data), exclude)
  }
  if (is.null(vars)) stop("vars must be specified", call. = FALSE)

  matched <- result$matched
  weighted <- matched[matched$weight > 0, ]

  left_side <- weighted[weighted$side == "left", ]
  right_side <- weighted[weighted$side == "right", ]

  if (!is.null(data)) {
    left_data <- data[as.character(data$id) %in% left_side$id, ]
    right_data <- data[as.character(data$id) %in% right_side$id, ]
  } else {
    stop("data must be provided for subclass_result balance diagnostics",
         call. = FALSE)
  }

  # Subclassification weights, applied on both sides.
  left_weights <- stats::setNames(left_side$weight, left_side$id)
  right_weights <- stats::setNames(right_side$weight, right_side$id)

  var_stats_list <- lapply(vars, function(v) {
    if (!v %in% names(data)) return(NULL)
    lw <- unname(left_weights[as.character(left_data$id)])
    rw <- unname(right_weights[as.character(right_data$id)])
    calculate_var_balance(left_data[[v]], right_data[[v]], v,
                          w_left = lw, w_right = rw)
  })
  var_stats_list <- var_stats_list[!sapply(var_stats_list, is.null)]

  var_stats <- dplyr::bind_rows(lapply(var_stats_list, tibble::as_tibble))
  overall <- .overall_balance(var_stats, length(vars))

  result_obj <- list(
    var_stats = var_stats,
    overall = overall,
    pairs = NULL,
    n_matched = nrow(weighted),
    n_unmatched_left = result$info$n_left - nrow(left_side),
    n_unmatched_right = result$info$n_right - nrow(right_side),
    n_total_left = result$info$n_left,
    n_total_right = result$info$n_right,
    method = "subclassification",
    has_blocks = FALSE,
    block_stats = NULL
  )

  class(result_obj) <- "balance_diagnostics"
  result_obj
}


#' Create Balance Table
#'
#' Formats balance diagnostics into a clean table for display or export.
#'
#' @param balance A balance_diagnostics object from \code{balance_diagnostics()}
#' @param digits Number of decimal places for rounding (default: 3)
#'
#' @return A tibble with formatted balance statistics
#'
#' @export
balance_table <- function(balance, digits = 3) {
  if (!inherits(balance, "balance_diagnostics")) {
    stop("balance must be a balance_diagnostics object")
  }

  # Format variable statistics
  tbl <- balance$var_stats[, c("variable", "mean_left", "mean_right", "mean_diff",
                                "std_diff", "var_ratio", "ks_statistic")]

  # Round numeric columns
  tbl$mean_left <- round(tbl$mean_left, digits)
  tbl$mean_right <- round(tbl$mean_right, digits)
  tbl$mean_diff <- round(tbl$mean_diff, digits)
  tbl$std_diff <- round(tbl$std_diff, digits)
  tbl$var_ratio <- round(tbl$var_ratio, digits)
  tbl$ks_statistic <- round(tbl$ks_statistic, digits)

  # Rename columns
  names(tbl) <- c("Variable", "Mean Left", "Mean Right", "Mean Diff",
                  "Std Diff", "Var Ratio", "KS Stat")

  return(tibble::as_tibble(tbl))
}


#' Print Method for Balance Diagnostics
#'
#' @param x A balance_diagnostics object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input object `x`.
#' @export
print.balance_diagnostics <- function(x, ...) {
  cat("\n")
  cat("Balance Diagnostics for Matched Pairs\n")
  cat("======================================\n\n")

  # Matching summary
  cat("Matching Summary:\n")
  cat(sprintf("  Method: %s\n", x$method))
  cat(sprintf("  Matched pairs: %d\n", x$n_matched))
  cat(sprintf("  Unmatched left: %d (of %d)\n", x$n_unmatched_left, x$n_total_left))
  cat(sprintf("  Unmatched right: %d (of %d)\n", x$n_unmatched_right, x$n_total_right))
  cat("\n")

  # Variable-level statistics
  cat("Variable-level Balance:\n")
  print(balance_table(x, digits = 3), n = Inf)
  cat("\n")

  # Overall assessment
  cat("Overall Balance:\n")
  cat(sprintf("  Mean |Std Diff|: %.3f ", x$overall$mean_abs_std_diff))

  if (x$overall$mean_abs_std_diff < 0.1) {
    cat("(Excellent)\n")
  } else if (x$overall$mean_abs_std_diff < 0.25) {
    cat("(Good)\n")
  } else if (x$overall$mean_abs_std_diff < 0.5) {
    cat("(Acceptable)\n")
  } else {
    cat("(Poor)\n")
  }

  cat(sprintf("  Max |Std Diff|: %.3f\n", x$overall$max_abs_std_diff))
  cat(sprintf("  Vars with |Std Diff| > 0.25: %.1f%%\n", x$overall$pct_large_imbalance))
  cat("\n")

  # Block summaries if present
  if (x$has_blocks && !is.null(x$block_stats)) {
    cat("Block-level Balance:\n")
    print(x$block_stats, n = Inf)
    cat("\n")
  }

  cat("Balance Interpretation:\n")
  cat("  |Std Diff| < 0.10: Excellent balance\n")
  cat("  |Std Diff| 0.10-0.25: Good balance\n")
  cat("  |Std Diff| 0.25-0.50: Acceptable balance\n")
  cat("  |Std Diff| > 0.50: Poor balance\n")
  cat("\n")


  invisible(x)
}

#' Summary method for balance diagnostics
#'
#' @param object A balance_diagnostics object
#' @param ... Additional arguments (ignored)
#'
#' @return A list containing summary statistics (invisibly)
#' @export
#' @method summary balance_diagnostics
summary.balance_diagnostics <- function(object, ...) {
  std_diffs <- abs(object$var_stats$std_diff)
  var_ratios <- object$var_stats$var_ratio

  # Classify balance quality
  quality <- if (object$overall$mean_abs_std_diff < 0.1) {
    "Excellent"
  } else if (object$overall$mean_abs_std_diff < 0.25) {
    "Good"
  } else if (object$overall$mean_abs_std_diff < 0.5) {
    "Acceptable"
  } else {
    "Poor"
  }

  out <- list(
    n_matched = object$n_matched,
    n_unmatched_left = object$n_unmatched_left,
    n_unmatched_right = object$n_unmatched_right,
    n_variables = nrow(object$var_stats),
    mean_abs_std_diff = object$overall$mean_abs_std_diff,
    max_abs_std_diff = object$overall$max_abs_std_diff,
    median_abs_std_diff = stats::median(std_diffs, na.rm = TRUE),
    pct_excellent = mean(std_diffs < 0.1, na.rm = TRUE) * 100,
    pct_good = mean(std_diffs >= 0.1 & std_diffs < 0.25, na.rm = TRUE) * 100,
    pct_acceptable = mean(std_diffs >= 0.25 & std_diffs < 0.5, na.rm = TRUE) * 100,
    pct_poor = mean(std_diffs >= 0.5, na.rm = TRUE) * 100,
    mean_var_ratio = mean(var_ratios, na.rm = TRUE),
    quality = quality,
    has_blocks = object$has_blocks,
    method = object$method
  )

  class(out) <- "summary.balance_diagnostics"
  out
}

#' @export
print.summary.balance_diagnostics <- function(x, ...) {
  cat("Balance Diagnostics Summary\n")
  cat("===========================\n\n")

  cat("Method:", x$method, "\n")
  cat("Matched pairs:", x$n_matched, "\n")
  cat("Variables assessed:", x$n_variables, "\n\n")

  cat("Standardized Differences:\n")
  cat("  Mean |Std Diff|:", sprintf("%.3f", x$mean_abs_std_diff), "\n")
  cat("  Median |Std Diff|:", sprintf("%.3f", x$median_abs_std_diff), "\n")
  cat("  Max |Std Diff|:", sprintf("%.3f", x$max_abs_std_diff), "\n\n")

  cat("Balance Quality Distribution:\n")
  cat("  Excellent (<0.10):", sprintf("%.1f%%", x$pct_excellent), "\n")
  cat("  Good (0.10-0.25):", sprintf("%.1f%%", x$pct_good), "\n")
  cat("  Acceptable (0.25-0.50):", sprintf("%.1f%%", x$pct_acceptable), "\n")
  cat("  Poor (>0.50):", sprintf("%.1f%%", x$pct_poor), "\n\n")

  cat("Overall Quality:", x$quality, "\n")

  invisible(x)
}

#' Plot method for balance diagnostics
#'
#' Produces a Love plot (dot plot) of standardized differences.
#'
#' @param x A balance_diagnostics object
#' @param type Type of plot: "love" (default), "histogram", or "variance"
#' @param threshold Threshold line for standardized differences (default: 0.1)
#' @param ... Additional arguments passed to plotting functions
#'
#' @return The balance_diagnostics object (invisibly)
#' @export
#' @method plot balance_diagnostics
plot.balance_diagnostics <- function(x,
                                     type = c("love", "histogram", "variance"),
                                     threshold = 0.1,
                                     ...) {
  type <- match.arg(type)

  switch(type,
    love = {
      # Love plot (dot plot of standardized differences)
      std_diffs <- x$var_stats$std_diff
      vars <- x$var_stats$variable
      n <- length(std_diffs)

      # Order by absolute value
      ord <- order(abs(std_diffs), decreasing = TRUE)
      std_diffs <- std_diffs[ord]
      vars <- vars[ord]

      # Set up plot area
      old_par <- graphics::par(mar = c(4, 8, 3, 2))
      on.exit(graphics::par(old_par))

      # Plot
      graphics::plot(std_diffs, seq_len(n),
                     xlim = range(c(-max(abs(std_diffs)) * 1.1, max(abs(std_diffs)) * 1.1, -threshold, threshold)),
                     yaxt = "n",
                     xlab = "Standardized Difference",
                     ylab = "",
                     main = "Balance: Standardized Differences",
                     pch = 19,
                     col = ifelse(abs(std_diffs) > threshold, "firebrick", "steelblue"),
                     ...)
      graphics::axis(2, at = seq_len(n), labels = vars, las = 1, cex.axis = 0.8)
      graphics::abline(v = 0, lty = 1, col = "gray50")
      graphics::abline(v = c(-threshold, threshold), lty = 2, col = "firebrick")
    },
    histogram = {
      # Histogram of standardized differences
      std_diffs <- abs(x$var_stats$std_diff)
      graphics::hist(std_diffs,
                     main = "Distribution of |Standardized Differences|",
                     xlab = "|Standardized Difference|",
                     col = "steelblue",
                     border = "white",
                     ...)
      graphics::abline(v = threshold, lty = 2, col = "firebrick", lwd = 2)
    },
    variance = {
      # Variance ratio plot
      var_ratios <- x$var_stats$var_ratio
      vars <- x$var_stats$variable
      n <- length(var_ratios)

      # Order by distance from 1
      ord <- order(abs(log(var_ratios)), decreasing = TRUE)
      var_ratios <- var_ratios[ord]
      vars <- vars[ord]

      old_par <- graphics::par(mar = c(4, 8, 3, 2))
      on.exit(graphics::par(old_par))

      graphics::plot(var_ratios, seq_len(n),
                     xlim = c(0.5, max(2, max(var_ratios, na.rm = TRUE) * 1.1)),
                     yaxt = "n",
                     xlab = "Variance Ratio (left/right)",
                     ylab = "",
                     main = "Balance: Variance Ratios",
                     pch = 19,
                     col = ifelse(var_ratios < 0.5 | var_ratios > 2, "firebrick", "steelblue"),
                     log = "x",
                     ...)
      graphics::axis(2, at = seq_len(n), labels = vars, las = 1, cex.axis = 0.8)
      graphics::abline(v = 1, lty = 1, col = "gray50")
      graphics::abline(v = c(0.5, 2), lty = 2, col = "firebrick")
    }
  )

  invisible(x)
}
