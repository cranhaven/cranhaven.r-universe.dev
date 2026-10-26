#' Find k-best optimal assignments
#'
#' Returns the top k optimal (or near-optimal) assignments using 'Murty' algorithm.
#' Useful for exploring alternative optimal solutions or finding robust assignments.
#'
#' @param x Cost matrix, data frame, or tibble. If a data frame/tibble,
#'   must include columns specified by `source`, `target`, and `cost`.
#' @param k Number of best solutions to return (default: 3)
#' @param source Column name for source/row indices (if `x` is a data frame)
#' @param target Column name for target/column indices (if `x` is a data frame)
#' @param cost Column name for costs (if `x` is a data frame)
#' @param maximize Logical; if TRUE, finds k-best maximizing assignments (default: FALSE)
#' @param method Algorithm for each sub-problem (default: "murty"). Future versions
#'   may support additional methods.
#' @param single_method Algorithm used for solving each node in the search tree
#'   (default: "jv")
#' @param forbidden Value to mark forbidden assignments (default: NA)
#'
#' @return A tibble with columns:
#'   - `rank`: ranking of solutions (1 = best, 2 = second best, etc.)
#'   - `solution_id`: unique identifier for each solution
#'   - `source`: source indices
#'   - `target`: target indices
#'   - `cost`: cost of each edge in the assignment
#'   - `total_cost`: total cost of the complete solution
#'
#' @examples
#' # Matrix input - find 5 best solutions
#' cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3)
#' lap_solve_kbest(cost, k = 5)
#'
#' # Data frame input
#' library(dplyr)
#' df <- tibble(
#'   source = rep(1:3, each = 3),
#'   target = rep(1:3, times = 3),
#'   cost = c(4, 2, 5, 3, 3, 6, 7, 5, 4)
#' )
#' lap_solve_kbest(df, k = 3, source, target, cost)
#'
#' # With maximization
#' lap_solve_kbest(cost, k = 3, maximize = TRUE)
#'
#' @export
lap_solve_kbest <- function(x, k = 3, source = NULL, target = NULL, cost = NULL,
                        maximize = FALSE, method = "murty", 
                        single_method = "jv", forbidden = NA) {
  
  # Handle data frame input
  if (is.data.frame(x)) {
    source_col <- rlang::enquo(source)
    target_col <- rlang::enquo(target)
    cost_col <- rlang::enquo(cost)
    
    if (rlang::quo_is_null(source_col) || rlang::quo_is_null(target_col) || 
        rlang::quo_is_null(cost_col)) {
      stop("For data frame input, must specify `source`, `target`, and `cost` columns")
    }
    
    return(lap_solve_kbest_df(x, k = k, source_col, target_col, cost_col,
                          maximize = maximize, method = method,
                          single_method = single_method, forbidden = forbidden))
  }
  
  # Handle matrix input
  cost_matrix <- mask_forbidden(as.matrix(x), forbidden)
  
  # Call the underlying k-best function
  result <- kbest_assignment(cost_matrix, k = k, maximize = maximize,
                            method = method, single_method = single_method)
  
  # Convert to modern tidy format
  if (nrow(result) == 0) {
    out <- tibble::tibble(
      rank = integer(0),
      solution_id = integer(0),
      source = integer(0),
      target = integer(0),
      cost = numeric(0),
      total_cost = numeric(0)
    )
  } else {
    out <- tibble::tibble(
      rank = result$rank,
      solution_id = result$match_id,
      source = result$row,
      target = result$col,
      cost = result$cost_edge,
      total_cost = result$cost_total
    )
  }
  
  class(out) <- c("lap_solve_kbest_result", class(out))
  out
}

#' @keywords internal
lap_solve_kbest_df <- function(df, k, source_col, target_col, cost_col,
                           maximize = FALSE, method = "murty",
                           single_method = "jv", forbidden = NA) {
  
  # Extract columns with error handling
  source_vals <- tryCatch(
    rlang::eval_tidy(source_col, df),
    error = function(e) stop("For data frame input, must specify `source`, `target`, and `cost` columns", call. = FALSE)
  )
  target_vals <- tryCatch(
    rlang::eval_tidy(target_col, df),
    error = function(e) stop("For data frame input, must specify `source`, `target`, and `cost` columns", call. = FALSE)
  )
  cost_vals <- tryCatch(
    rlang::eval_tidy(cost_col, df),
    error = function(e) stop("For data frame input, must specify `source`, `target`, and `cost` columns", call. = FALSE)
  )
  
  built <- long_to_cost_matrix(source_vals, target_vals, cost_vals, forbidden)
  cost_matrix <- built$cost_matrix
  unique_sources <- built$sources
  unique_targets <- built$targets
  
  # Solve
  result <- kbest_assignment(cost_matrix, k = k, maximize = maximize,
                            method = method, single_method = single_method)
  
  # Convert back to original indices
  if (nrow(result) == 0) {
    out <- tibble::tibble(
      rank = integer(0),
      solution_id = integer(0),
      source = unique_sources[integer(0)],
      target = unique_targets[integer(0)],
      cost = numeric(0),
      total_cost = numeric(0)
    )
  } else {
    out <- tibble::tibble(
      rank = result$rank,
      solution_id = result$match_id,
      source = unique_sources[result$row],
      target = unique_targets[result$col],
      cost = result$cost_edge,
      total_cost = result$cost_total
    )
  }
  
  class(out) <- c("lap_solve_kbest_result", class(out))
  out
}

#' Print method for k-best assignment results
#' @param x A `lap_solve_kbest_result`.
#' @param ... Additional arguments passed to `print()`. Ignored.
#' @return Invisibly returns the input object `x`.
#' @export
#' @method print lap_solve_kbest_result
print.lap_solve_kbest_result <- function(x, ...) {
  cat("K-Best Assignment Results\n")
  cat("=========================\n\n")
  
  if (nrow(x) == 0) {
    cat("No solutions found\n")
    return(invisible(x))
  }
  
  n_solutions <- length(unique(x$solution_id))
  cat("Number of solutions:", n_solutions, "\n")
  
  # Get summary of total costs
  solution_costs <- x |>
    dplyr::group_by(solution_id, rank) |>
    dplyr::summarise(total_cost = dplyr::first(total_cost), .groups = "drop") |>
    dplyr::arrange(rank)
  
  cat("\nSolution costs:\n")
  for (i in seq_len(min(5, nrow(solution_costs)))) {
    cat(sprintf("  Rank %d: %.4f\n", 
                solution_costs$rank[i], 
                solution_costs$total_cost[i]))
  }
  
  if (nrow(solution_costs) > 5) {
    cat(sprintf("  ... and %d more solutions\n", nrow(solution_costs) - 5))
  }
  
  cat("\nAssignments:\n")
  print(tibble::as_tibble(x), ...)
  
  invisible(x)
}

#' Get summary of k-best results
#'
#' Extract summary information from k-best assignment results.
#'
#' @param object An object of class `lap_solve_kbest_result`.
#' @param ... Additional arguments (unused).
#'
#' @return A tibble with one row per solution containing:
#'   - `rank`: solution rank  
#'   - `solution_id`: solution identifier  
#'   - `total_cost`: total cost of the solution  
#'   - `n_assignments`: number of assignments in the solution  
#'
#' @export
#' @method summary lap_solve_kbest_result
summary.lap_solve_kbest_result <- function(object, ...) {
  x <- object

  if (nrow(x) == 0) {
    return(tibble::tibble(
      rank = integer(0),
      solution_id = integer(0),
      total_cost = numeric(0),
      n_assignments = integer(0)
    ))
  }

  x |>
    dplyr::group_by(rank, solution_id, total_cost) |>
    dplyr::summarise(n_assignments = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(rank)
}

# ==============================================================================
# Low-Level K-Best Function (Internal Helper)
# ==============================================================================

#' K-best linear assignment solutions (internal helper)
#'
#' Internal helper function for computing k-best solutions.
#' Users should use `lap_solve_kbest()` instead.
#'
#' @param cost Cost matrix
#' @param k Number of solutions
#' @param maximize Maximize flag
#' @param method Algorithm (only "murty" supported)
#' @param single_method Base solver
#'
#' @return data.frame with solution details
#' @keywords internal
#' @noRd
kbest_assignment <- function(cost, k = 3, maximize = FALSE,
                             method = c("murty"),
                             single_method = c("jv")) {
  method <- match.arg(method)
  single_method <- match.arg(single_method)
  cost <- as.matrix(cost)
  if (!is.numeric(cost)) {
    stop("`cost` must be a numeric matrix, got ", typeof(cost))
  }
  if (any(is.nan(cost))) stop("NaN not allowed in `cost`")

  x <- lap_kbest_murty(cost, as.integer(k), maximize, single_method)

  matches <- x$matches
  totals  <- as.numeric(x$totals)
  nsol <- nrow(matches)
  n <- ncol(matches)

  if (nsol == 0L) {
    return(data.frame(
      rank = integer(),
      match_id = integer(),
      row = integer(),
      col = integer(),
      cost_edge = double(),
      cost_total = double()
    ))
  }

  rows <- rep(seq_len(n), times = nsol)
  cols <- as.integer(t(matches))
  rank <- rep(seq_len(nsol), each = n)
  match_id <- rep(seq_len(nsol), each = n)
  edge_costs <- cost[cbind(rows, cols)]
  tot <- rep(totals, each = n)

  data.frame(
    rank = rank,
    match_id = match_id,
    row = rows,
    col = cols,
    cost_edge = edge_costs,
    cost_total = tot
  )
}
