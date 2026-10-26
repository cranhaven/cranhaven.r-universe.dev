# ==============================================================================
# Parallel Matching Helpers
# ==============================================================================

#' Check if parallel processing is available
#'
#' @return Logical indicating if future package is available
#' @keywords internal
can_parallelize <- function() {
  requireNamespace("future", quietly = TRUE) &&
    requireNamespace("future.apply", quietly = TRUE)
}

#' Setup parallel processing with future
#'
#' @param parallel Logical or plan specification
#' @param n_workers Number of workers (NULL for auto-detect)
#' @return List with original plan and whether we set up parallelization
#' @keywords internal
setup_parallel <- function(parallel = FALSE, n_workers = NULL) {
  # Return early if parallel is FALSE
  if (isFALSE(parallel)) {
    return(list(
      setup = FALSE,
      original_plan = NULL
    ))
  }

  # Check if future packages are available
  if (!can_parallelize()) {
    if (isTRUE(parallel)) {
      warning(
        "Parallel processing requested but 'future' and/or 'future.apply' ",
        "packages are not installed. Install with:\n",
        "  install.packages(c('future', 'future.apply'))\n",
        "Falling back to sequential processing.",
        call. = FALSE
      )
    }
    return(list(
      setup = FALSE,
      original_plan = NULL
    ))
  }

  # Store original plan
  original_plan <- future::plan()

  # Setup parallel plan
  if (isTRUE(parallel)) {
    # Auto-select plan based on platform
    if (is.null(n_workers)) {
      n_workers <- parallelly::availableCores(omit = 1)
    }

    # Use multisession (works on all platforms)
    future::plan(future::multisession, workers = n_workers)

    message(
      "Parallel processing enabled with ", n_workers, " workers\n",
      "Tip: Set plan explicitly with future::plan() for more control"
    )
  } else if (is.character(parallel)) {
    # Allow user to specify plan by name
    tryCatch({
      future::plan(parallel)
    }, error = function(e) {
      warning(
        "Could not set parallel plan '", parallel, "': ", e$message, "\n",
        "Falling back to sequential processing.",
        call. = FALSE
      )
      return(list(setup = FALSE, original_plan = NULL))
    })
  }

  list(
    setup = TRUE,
    original_plan = original_plan
  )
}

#' Restore original parallel plan
#'
#' @param parallel_state State from setup_parallel()
#' @return No return value, called for side effects (restores parallel plan).
#' @keywords internal
restore_parallel <- function(parallel_state) {
  if (parallel_state$setup && !is.null(parallel_state$original_plan)) {
    future::plan(parallel_state$original_plan)
  }
}

#' Parallel lapply using future
#'
#' @param X Vector to iterate over
#' @param FUN Function to apply
#' @param ... Additional arguments to FUN
#' @param parallel Whether parallel processing is enabled
#' @return List of results
#' @keywords internal
parallel_lapply <- function(X, FUN, ..., parallel = FALSE) {
  if (parallel && can_parallelize()) {
    # Use future.apply for parallel execution
    future.apply::future_lapply(X, FUN, ..., future.seed = TRUE)
  } else {
    # Fall back to regular lapply
    lapply(X, FUN, ...)
  }
}

#' Shared parallel block matching implementation
#'
#' Core logic for both optimal (LAP) and greedy parallel block matching.
#' Called by match_blocks_parallel() and greedy_blocks_parallel().
#'
#' @param solver_fn Solver function (assignment or greedy_matching)
#' @param solver_params Named list of extra args passed to solver_fn
#' @param check_costs If TRUE, passed through to .couples_single
#' @param strict_no_pairs If TRUE, passed through to .couples_single
#' @return List with combined results from all blocks
#' @keywords internal
.blocks_parallel <- function(blocks, left, right, left_ids, right_ids,
                             block_col, vars, distance, weights, scale,
                             max_distance, calipers,
                             solver_fn, solver_params = list(),
                             check_costs = FALSE,
                             strict_no_pairs = FALSE,
                             parallel = FALSE,
                             replace = FALSE,
                             ratio = 1L,
                             sigma = NULL,
                             memory_mode = "auto") {

  # Force-capture internal functions for parallel workers (future serializes
  # closures but may not resolve package-internal names on workers)
  couples_single_fn <- .couples_single
  block_summary_row_fn <- .block_summary_row

  # Function to match a single block
  match_one_block <- function(block) {
    left_block <- left[left[[block_col]] == block, ]
    right_block <- right[right[[block_col]] == block, ]

    # Handle empty blocks
    if (nrow(left_block) == 0 || nrow(right_block) == 0) {
      block_left_ids <- if (nrow(left_block) > 0) {
        left_ids[left[[block_col]] == block]
      } else {
        character(0)
      }

      block_right_ids <- if (nrow(right_block) > 0) {
        right_ids[right[[block_col]] == block]
      } else {
        character(0)
      }

      return(list(
        pairs = tibble::tibble(
          left_id = character(0),
          right_id = character(0),
          distance = numeric(0),
          block_id = character(0)
        ),
        unmatched_left = block_left_ids,
        unmatched_right = block_right_ids,
        solver = character(0),
        summary = block_summary_row_fn(
          block_id = block,
          n_left = nrow(left_block),
          n_right = nrow(right_block),
          pairs = tibble::tibble(distance = numeric(0)),
          n_unmatched_left = length(block_left_ids),
          n_unmatched_right = length(block_right_ids)
        )
      ))
    }

    # Get IDs for this block
    block_left_ids <- left_ids[left[[block_col]] == block]
    block_right_ids <- right_ids[right[[block_col]] == block]

    # Match within block using shared implementation
    block_result <- couples_single_fn(
      left_block, right_block, block_left_ids, block_right_ids,
      vars, distance, weights, scale,
      max_distance, calipers,
      solver_fn = solver_fn, solver_params = solver_params,
      check_costs = check_costs, strict_no_pairs = strict_no_pairs,
      replace = replace, ratio = ratio,
      sigma = sigma, memory_mode = memory_mode
    )

    # Add block_id column to pairs
    if (nrow(block_result$pairs) > 0) {
      block_result$pairs$block_id <- as.character(block)
    }

    list(
      pairs = block_result$pairs,
      unmatched_left = block_result$unmatched$left,
      unmatched_right = block_result$unmatched$right,
      solver = block_result$info$solver,
      potentials = block_result$potentials,
      summary = block_summary_row_fn(
        block_id = block,
        n_left = nrow(left_block),
        n_right = nrow(right_block),
        pairs = block_result$pairs,
        n_unmatched_left = length(block_result$unmatched$left),
        n_unmatched_right = length(block_result$unmatched$right)
      )
    )
  }

  # Process blocks (in parallel if enabled)
  block_results <- parallel_lapply(
    blocks,
    match_one_block,
    parallel = parallel
  )

  # Combine results
  all_pairs <- dplyr::bind_rows(lapply(block_results, function(x) x$pairs))
  all_unmatched_left <- unlist(lapply(block_results, function(x) x$unmatched_left))
  all_unmatched_right <- unlist(lapply(block_results, function(x) x$unmatched_right))
  all_summaries <- dplyr::bind_rows(lapply(block_results, function(x) x$summary))
  all_solvers <- unlist(lapply(block_results, function(x) x$solver))

  # Handle empty results
  if (nrow(all_pairs) == 0) {
    all_pairs <- tibble::tibble(
      left_id = character(0),
      right_id = character(0),
      distance = numeric(0),
      block_id = character(0)
    )
  }

  list(
    pairs = all_pairs,
    unmatched = list(
      left = as.character(all_unmatched_left),
      right = as.character(all_unmatched_right)
    ),
    block_summary = all_summaries,
    solvers = as.character(all_solvers),
    potentials = lapply(block_results, function(x) x$potentials)
  )
}

#' Match blocks in parallel
#'
#' @param blocks Vector of block IDs
#' @param left Left dataset with block_col
#' @param right Right dataset with block_col
#' @param left_ids IDs from left
#' @param right_ids IDs from right
#' @param block_col Name of blocking column
#' @param vars Variables for matching
#' @param distance Distance metric
#' @param weights Variable weights
#' @param scale Scaling method
#' @param max_distance Maximum distance
#' @param calipers Caliper constraints
#' @param method LAP method
#' @param parallel Whether to use parallel processing
#' @return List with combined results from all blocks
#' @keywords internal
match_blocks_parallel <- function(blocks, left, right, left_ids, right_ids,
                                  block_col, vars, distance, weights, scale,
                                  max_distance, calipers, method,
                                  parallel = FALSE) {
  .blocks_parallel(
    blocks, left, right, left_ids, right_ids,
    block_col, vars, distance, weights, scale,
    max_distance, calipers,
    solver_fn = assignment,
    solver_params = list(method = method),
    check_costs = FALSE,
    strict_no_pairs = TRUE,
    parallel = parallel
  )
}

#' Greedy match blocks in parallel
#'
#' @param blocks Vector of block IDs
#' @param left Left dataset with block_col
#' @param right Right dataset with block_col
#' @param left_ids IDs from left
#' @param right_ids IDs from right
#' @param block_col Name of blocking column
#' @param vars Variables for matching
#' @param distance Distance metric
#' @param weights Variable weights
#' @param scale Scaling method
#' @param max_distance Maximum distance
#' @param calipers Caliper constraints
#' @param strategy Greedy strategy
#' @param parallel Whether to use parallel processing
#' @return List with combined results from all blocks
#' @keywords internal
greedy_blocks_parallel <- function(blocks, left, right, left_ids, right_ids,
                                   block_col, vars, distance, weights, scale,
                                   max_distance, calipers, strategy,
                                   parallel = FALSE) {
  .blocks_parallel(
    blocks, left, right, left_ids, right_ids,
    block_col, vars, distance, weights, scale,
    max_distance, calipers,
    solver_fn = greedy_matching,
    solver_params = list(strategy = strategy),
    check_costs = FALSE,
    strict_no_pairs = FALSE,
    parallel = parallel
  )
}