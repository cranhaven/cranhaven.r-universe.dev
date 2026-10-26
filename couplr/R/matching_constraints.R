# ==============================================================================
# Matching Constraints - Apply calipers and max_distance
# ==============================================================================

#' Large value for forbidden pairs
#'
#' A numeric constant used to mark forbidden pairs in cost matrices.
#' @format Numeric value (half of .Machine$double.xmax).
#' @keywords internal
BIG_COST <- .Machine$double.xmax / 2

# The admissibility test every path shares: a cost stands for an allowed pair
# when it is finite and below the forbidden marker. `NA`, `Inf` and a cost at or
# above `BIG_COST` all say the same thing, which is that there is no edge there.
.is_valid_cost <- function(cost) {
  is.finite(cost) & cost < BIG_COST
}

#' Apply maximum distance constraint
#'
#' @return Modified cost matrix with forbidden pairs marked.
#' @keywords internal
apply_max_distance <- function(cost_matrix, max_distance = Inf) {
  if (is.null(max_distance)) {
    return(cost_matrix)
  }

  if (!is.numeric(max_distance) || length(max_distance) != 1) {
    stop("max_distance must be a single numeric value", call. = FALSE)
  }

  if (is.infinite(max_distance)) {
    return(cost_matrix)
  }

  if (max_distance <= 0) {
    stop("max_distance must be positive", call. = FALSE)
  }

  if (is_lazy_cost_spec(cost_matrix)) {
    # O(1): just record the threshold. Cheaper than the dense O(n*m) masking
    # below, since no matrix ever gets materialized in the first place.
    cost_matrix$max_distance <- max_distance
    return(cost_matrix)
  }

  # Mark pairs exceeding max_distance as forbidden (Inf, not BIG_COST: the
  # LAP solvers detect non-finite cells as forbidden and short-circuit on
  # infeasibility; a huge finite "BIG_COST" looks like a regular edge to JV
  # and can stall the solver on sparse problems).
  cost_matrix[cost_matrix > max_distance] <- Inf

  cost_matrix
}

#' Apply caliper constraints
#'
#' Calipers impose per-variable maximum absolute differences.
#'
#' @return Modified cost matrix with forbidden pairs marked.
#' @keywords internal
apply_calipers <- function(cost_matrix, left, right, calipers, vars) {
  if (is.null(calipers)) {
    return(cost_matrix)
  }

  if (is_lazy_cost_spec(cost_matrix)) {
    # O(n + m) per caliper rather than the dense path's O(n*m) scan: record the
    # threshold with the variable's raw values, which the cost source compares
    # pair by pair. The raw values, not the spec's feature columns: those are
    # scaled and weighted, and a caliper is stated on the variable as supplied.
    new_calipers <- cost_matrix$calipers
    for (var_name in names(calipers)) {
      if (!var_name %in% vars) next  # not a matching variable, same as dense path
      new_calipers[[length(new_calipers) + 1]] <- list(
        var = var_name, threshold = calipers[[var_name]],
        left = as.numeric(left[[var_name]]),
        right = as.numeric(right[[var_name]])
      )
    }
    cost_matrix$calipers <- new_calipers
    return(cost_matrix)
  }

  # For each variable with a caliper
  for (var_name in names(calipers)) {
    if (!(var_name %in% vars)) {
      next  # Skip if not in matching variables
    }

    caliper_value <- calipers[[var_name]]

    # The whole n_left x n_right table of absolute differences on this
    # variable, in one pass, and the cells beyond the caliper are forbidden.
    over <- abs(outer(left[[var_name]], right[[var_name]], `-`)) > caliper_value
    cost_matrix[over] <- Inf
  }

  cost_matrix
}

#' Mark forbidden pairs
#'
#' Generic function to mark specific pairs as forbidden.
#'
#' @return Modified cost matrix with forbidden pairs marked.
#' @keywords internal
mark_forbidden_pairs <- function(cost_matrix, forbidden_indices) {
  if (is.null(forbidden_indices) || nrow(forbidden_indices) == 0) {
    return(cost_matrix)
  }

  if (is_lazy_cost_spec(cost_matrix)) {
    stop("Explicit forbidden pairs are not supported with memory_mode = \"lazy\" yet; ",
         "use memory_mode = \"dense\".", call. = FALSE)
  }

  # forbidden_indices should be a 2-column matrix of (row, col) indices
  for (k in seq_len(nrow(forbidden_indices))) {
    i <- forbidden_indices[k, 1]
    j <- forbidden_indices[k, 2]
    cost_matrix[i, j] <- Inf
  }

  cost_matrix
}

#' Apply all constraints to cost matrix
#'
#' Main entry point for applying constraints.
#'
#' @return Modified cost matrix with all constraints applied.
#' @keywords internal
apply_all_constraints <- function(cost_matrix, left, right, vars,
                                  max_distance = Inf, calipers = NULL,
                                  forbidden = NULL) {
  # Apply max_distance
  cost_matrix <- apply_max_distance(cost_matrix, max_distance)

  # Apply calipers
  cost_matrix <- apply_calipers(cost_matrix, left, right, calipers, vars)

  # Apply custom forbidden pairs if provided
  cost_matrix <- mark_forbidden_pairs(cost_matrix, forbidden)

  cost_matrix
}

#' Check if any valid pairs exist
#'
#' @return Logical indicating whether any valid pairs exist.
#' @keywords internal
has_valid_pairs <- function(cost_matrix) {
  if (is_lazy_cost_spec(cost_matrix)) {
    # Deferred to solve time: an O(n*m) scan here to answer this precisely
    # would cost exactly what the lazy path exists to avoid. If the
    # constraints actually leave no valid pairs, the solve attempt itself
    # throws an InfeasibleException, which callers (.solve_with_partial_feasibility)
    # already catch and translate into the same "no valid pairs" outcome.
    return(TRUE)
  }
  any(.is_valid_cost(cost_matrix))
}

#' Count valid pairs in cost matrix
#'
#' @return Integer count of valid (non-forbidden) pairs.
#' @keywords internal
count_valid_pairs <- function(cost_matrix) {
  sum(.is_valid_cost(cost_matrix))
}
