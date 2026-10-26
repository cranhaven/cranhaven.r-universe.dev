# ==============================================================================
# Cardinality Matching: the pruning heuristic
# ==============================================================================
# A full match, then repeated deletion of the pairs that carry the worst
# variable's imbalance. Reached from cardinality_match(engine = "heuristic").
# ==============================================================================

# Row positions of a pairs column's ids in the side it came from, in `pairs`
# row order. Both readers below need the values pair by pair, and merge()
# sorts by its key, so the left values would arrive ordered by left_id and the
# right values by right_id -- two orders, neither of them the pairs'.
.pair_side_index <- function(ids, side_ids) {
  match(as.character(ids), side_ids)
}

#' Compute standardized differences for current pairs
#' @keywords internal
.compute_pair_balance <- function(pairs, left, right, vars,
                                  left_ids, right_ids) {
  li <- .pair_side_index(pairs$left_id, left_ids)
  ri <- .pair_side_index(pairs$right_id, right_ids)

  std_diffs <- vapply(vars, function(v) {
    standardized_difference(left[[v]][li], right[[v]][ri])
  }, numeric(1))

  list(std_diffs = std_diffs)
}

#' Compute per-pair differences on a single variable
#' @keywords internal
.pair_var_diffs <- function(pairs, left, right, var, left_ids, right_ids) {
  # Check if diff column already exists
  diff_col <- paste0(".", var, "_diff")
  if (diff_col %in% names(pairs)) {
    return(pairs[[diff_col]])
  }

  li <- .pair_side_index(pairs$left_id, left_ids)
  ri <- .pair_side_index(pairs$right_id, right_ids)

  left[[var]][li] - right[[var]][ri]
}

#' Balance pruning heuristic
#'
#' Runs a full optimal match, then deletes pairs until every matching variable
#' sits inside the standardized-difference threshold or the iteration budget
#' runs out.
#'
#' @param left,right Data frames of units, one row each.
#' @param vars Character vector of matching variable names.
#' @param max_std_diff Standardized-difference threshold the loop prunes toward.
#' @param distance,weights,scale,auto_scale,sigma Distance construction, as in
#'   [match_couples()].
#' @param max_distance,calipers Pair constraints, as in [match_couples()].
#' @param left_id,right_id Id columns, as in [match_couples()].
#' @param method LAP solver method for the initial match.
#' @param max_iter Maximum deletion rounds.
#' @param batch_fraction Share of the remaining pairs deleted each round.
#'
#' @return A `matching_result` carrying `info$pruning_iterations` and
#'   `info$pairs_removed`.
#' @keywords internal
.cardinality_prune <- function(left, right, vars, max_std_diff,
                               distance = "euclidean", weights = NULL,
                               scale = FALSE, auto_scale = FALSE, sigma = NULL,
                               max_distance = Inf, calipers = NULL,
                               left_id = NULL, right_id = NULL,
                               method = "auto", max_iter = 100L,
                               batch_fraction = 0.1) {

  # Validate
  if (!is.numeric(max_std_diff) || length(max_std_diff) != 1 ||
      max_std_diff <= 0) {
    stop("max_std_diff must be a positive number", call. = FALSE)
  }

  # Step 1: Full initial matching
  result <- match_couples(
    left, right, vars = vars,
    left_id = left_id, right_id = right_id,
    distance = distance, weights = weights,
    scale = scale, auto_scale = auto_scale,
    sigma = sigma,
    max_distance = max_distance, calipers = calipers,
    method = method,
    return_unmatched = TRUE,
    return_diagnostics = TRUE
  )

  if (result$info$n_matched == 0) {
    result$info$pruning_iterations <- 0L
    result$info$pairs_removed <- 0L
    return(result)
  }

  # Step 2: Iterative pruning
  pairs <- result$pairs
  original_n <- nrow(pairs)
  iter <- 0L

  # The ids the pairs are keyed on, read the way every other entry point reads
  # them, so that a data frame with no id column resolves to the same values
  # match_couples() put in the pairs.
  left_ids <- extract_ids(left, "left", left_id)
  right_ids <- extract_ids(right, "right", right_id)

  for (it in seq_len(max_iter)) {
    if (nrow(pairs) <= 1) break

    # Compute balance for current pairs
    bal <- .compute_pair_balance(pairs, left, right, vars,
                                left_ids, right_ids)

    # Check if balance is achieved
    if (all(abs(bal$std_diffs) <= max_std_diff)) break

    iter <- it

    # Find worst variable
    worst_idx <- which.max(abs(bal$std_diffs))
    worst_var <- vars[worst_idx]

    # For each pair, compute contribution to imbalance on worst variable
    pair_diffs <- .pair_var_diffs(pairs, left, right, worst_var,
                                 left_ids, right_ids)
    abs_diffs <- abs(pair_diffs)

    # Remove worst batch
    n_remove <- max(1L, ceiling(nrow(pairs) * batch_fraction))
    n_remove <- min(n_remove, nrow(pairs) - 1L)  # Keep at least 1 pair
    remove_idx <- order(abs_diffs, decreasing = TRUE)[seq_len(n_remove)]
    pairs <- pairs[-remove_idx, , drop = FALSE]
  }

  # Step 3: Build result
  result$pairs <- pairs
  result$unmatched <- list(
    left = setdiff(left_ids, as.character(pairs$left_id)),
    right = setdiff(right_ids, as.character(pairs$right_id))
  )
  result$info$n_matched <- nrow(pairs)
  result$info$total_distance <- sum(pairs$distance, na.rm = TRUE)
  result$info$pruning_iterations <- iter
  result$info$pairs_removed <- original_n - nrow(pairs)
  result$info$max_std_diff_target <- max_std_diff
  # Pruning discards focal units, so what the design identifies moves with it.
  result$info[c("estimand", "focal", "focal_discarded")] <-
    design_estimand(nrow(left), dplyr::n_distinct(pairs$left_id))

  result
}

#' Report for a pruned match
#'
#' The fields a `cardinality_report` carries that the prune loop computes, and
#' `NA` for the ones it does not. The loop maximizes nothing and derives no
#' bound, so `best_possible` and `gap` have no value to hold; `NA` is what they
#' hold, rather than a number that would read as a bound.
#'
#' @param pairs The matched pairs the loop settled on.
#' @param max_std_diff The threshold it pruned toward.
#'
#' @return An object of class `c("cardinality_heuristic", "cardinality_report")`.
#' @keywords internal
.cardinality_heuristic_report <- function(pairs, max_std_diff) {
  structure(list(n_matched = nrow(pairs),
                 n_left_matched = length(unique(pairs$left_id)),
                 best_possible = NA_integer_,
                 gap = NA_integer_,
                 gap_fraction = NA_real_,
                 certified = FALSE,
                 objective = NA_real_,
                 bound = NA_real_,
                 stopped_on = "heuristic",
                 n_nodes = NA_integer_,
                 engine = "heuristic",
                 status = "heuristic",
                 max_std_diff = max_std_diff,
                 total_distance = sum(pairs$distance, na.rm = TRUE),
                 pairs = pairs),
            class = c("cardinality_heuristic", "cardinality_report"))
}

#' @param x A `cardinality_heuristic` report.
#' @param ... Ignored.
#' @return Invisibly returns `x`.
#' @export
#' @method print cardinality_heuristic
#' @rdname dot-cardinality_heuristic_report
print.cardinality_heuristic <- function(x, ...) {
  line <- function(label, value) cat(sprintf("%-21s%s\n", label, value))
  line("Matched units:", format(x$n_matched))
  line("Best possible:", "not computed")
  cat("Heuristic: no optimality guarantee\n")
  invisible(x)
}
