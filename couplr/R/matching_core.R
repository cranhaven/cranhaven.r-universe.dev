# ==============================================================================
# Matching Core - match_couples()
# ==============================================================================

# ==============================================================================
# Shared Internal Implementations
# ==============================================================================

# Replace forbidden entries of `sub` with a finite sentinel, so that a
# minimum-cost solve maximises the number of admissible pairs before it
# minimises cost. The sentinel magnitude comes from .cardinality_sentinel() in
# R/lap_cardinality.R, which is the same quantity assignment(cardinality =) uses
# to price its dummy columns. Returns NULL when that ordering can no longer be
# represented exactly in a double.
.pad_forbidden <- function(sub, admissible) {
  real <- sub[admissible]
  sentinel <- .cardinality_sentinel(real, min(dim(sub)))
  if (is.null(sentinel)) {
    return(NULL)
  }
  sub[!admissible] <- sentinel
  sub
}

# Solve a LAP that may have rows or columns where every edge is forbidden
# (Inf / NA / >= BIG_COST). Such rows/cols can't be matched, and most C++
# solvers would raise "Infeasible: row N has no allowed edges". We drop them
# before calling the solver and re-map the result back to original indices so
# the caller can report them as unmatched.
#
# The pruned submatrix can still admit no perfect matching, because Hall's
# condition fails: several rows compete for the same few admissible columns.
# The objective there is lexicographic, the largest number of admissible pairs
# first and the smallest total cost among matchings of that size second, and
# it is reached by padding the forbidden entries with a sentinel and solving
# the padded problem with the same optimal solver. Pairs that came back on a
# sentinel edge are dropped before the result is returned.
#
# Returns a list with:
#   result       — raw solver output for the submatrix (or NULL if degenerate)
#   matched_rows — 1-based indices into the *original* cost_matrix rows
#   matched_cols — 1-based indices into the *original* cost_matrix cols
#   potentials   — the assignment duals of the submatrix, spread over the
#                  original rows and columns with NA on the ones pruned, or
#                  NULL when the answer is not the optimum of that LP
.solve_with_partial_feasibility <- function(cost_matrix, solver_fn,
                                            solver_params = list()) {
  if (is_lazy_cost_spec(cost_matrix)) {
    return(.solve_lazy_with_partial_feasibility(cost_matrix, solver_fn, solver_params))
  }

  feasible <- .is_valid_cost(cost_matrix)
  row_ok <- rowSums(feasible) > 0L
  col_ok <- colSums(feasible) > 0L

  if (!any(row_ok) || !any(col_ok)) {
    return(list(result = NULL,
                matched_rows = integer(0),
                matched_cols = integer(0)))
  }

  if (all(row_ok) && all(col_ok)) {
    sub <- cost_matrix
    sub_feasible <- feasible
  } else {
    sub <- cost_matrix[row_ok, col_ok, drop = FALSE]
    sub_feasible <- feasible[row_ok, col_ok, drop = FALSE]
  }

  orig_rows <- which(row_ok)
  orig_cols <- which(col_ok)

  res <- tryCatch(
    do.call(solver_fn, c(list(sub, maximize = FALSE), solver_params)),
    error = function(e) NULL
  )

  padded <- NULL
  greedy_fallback <- FALSE
  if (is.null(res)) {
    padded <- .pad_forbidden(sub, sub_feasible)
    if (!is.null(padded)) {
      res <- tryCatch(
        do.call(solver_fn, c(list(padded, maximize = FALSE), solver_params)),
        error = function(e) NULL
      )
      if (is.null(res)) {
        padded <- NULL
      }
    }
  }

  if (is.null(res)) {
    # Neither the direct nor the padded solve returned. Greedy still produces a
    # partial matching, but it is not the optimal one, so say so rather than
    # letting an optimal request come back quietly downgraded.
    res <- tryCatch(
      greedy_matching(sub, strategy = "sorted"),
      error = function(e) NULL
    )
    if (is.null(res)) {
      return(list(result = NULL,
                  matched_rows = integer(0),
                  matched_cols = integer(0)))
    }
    greedy_fallback <- TRUE
    warning("constraints admit no complete matching and the cost range is too ",
            "wide to solve the maximum-cardinality problem exactly; returning ",
            "a greedy partial matching, which is not optimal. Relax ",
            "max_distance/calipers or rescale the covariates to recover an ",
            "optimal result.", call. = FALSE)
  }

  match_vec <- as.integer(res$match)
  matched_sub_rows <- which(match_vec > 0L)
  matched_sub_cols <- match_vec[matched_sub_rows]

  if (!is.null(padded)) {
    # Drop the pairs that were only matched through a sentinel edge.
    keep <- sub_feasible[cbind(matched_sub_rows, matched_sub_cols)]
    matched_sub_rows <- matched_sub_rows[keep]
    matched_sub_cols <- matched_sub_cols[keep]
  }

  matched_rows <- orig_rows[matched_sub_rows]
  matched_cols <- orig_cols[matched_sub_cols]

  # The submatrix is an assignment LP, and its duals are what the matching is
  # certified against. A padded solve answered a different objective and a
  # greedy one no objective at all, so neither has them.
  potentials <- NULL
  if (is.null(padded) && !greedy_fallback &&
      !identical(solver_fn, greedy_matching)) {
    potentials <- .sub_potentials(res, sub, orig_rows, orig_cols,
                                  dim(cost_matrix))
  }

  list(result = res, matched_rows = matched_rows, matched_cols = matched_cols,
       potentials = potentials)
}

# The assignment duals of `sub`, placed at the original rows and columns it was
# cut from. A solver that returned none is answered by assignment_duals(): the
# optimal duals of a linear program are shared by all its optimal solutions, so
# duals from a second solve are duals for the matching the first one returned.
.sub_potentials <- function(res, sub, orig_rows, orig_cols, dims) {
  duals <- .solver_duals(res, transposed = FALSE)
  if (is.null(duals)) {
    duals <- tryCatch({
      d <- assignment_duals(sub)
      list(u = d$u, v = d$v)
    }, error = function(e) NULL)
  }
  if (is.null(duals)) {
    return(NULL)
  }
  rows <- rep(NA_real_, dims[1])
  cols <- rep(NA_real_, dims[2])
  rows[orig_rows] <- duals$u
  cols[orig_cols] <- duals$v
  list(rows = rows, cols = cols)
}

# Lazy-cost-spec counterpart of .solve_with_partial_feasibility(). The whole
# problem is put to the solver first. When the constraints admit no complete
# matching -- the lazy solver throws, or the implicit loop answers with Hall's
# witness -- the answer is the one the dense path reaches by pruning and
# sentinel padding: the largest matching the constraints admit, and the
# cheapest among those. Here it is the one-to-one design solved by the design
# loop over the same specification, whose shortfall is a maximum flow certified
# by a residual cut over every admissible pair and whose cost is priced against
# its potentials, so no pair set is built on the way to it either.
.solve_lazy_with_partial_feasibility <- function(cost_matrix, solver_fn,
                                                 solver_params = list()) {
  mode <- lazy_cost_spec_mode(cost_matrix)

  if (identical(solver_fn, greedy_matching)) {
    stop("method = \"greedy\" does not support memory_mode = \"", mode,
         "\" yet; use an optimal method (\"jv\"/\"auction\") or ",
         "memory_mode = \"dense\".", call. = FALSE)
  }

  res <- tryCatch(
    do.call(solver_fn, c(list(cost_matrix, maximize = FALSE), solver_params)),
    error = function(e) e
  )

  if (inherits(res, "error") || identical(res$status, "infeasible")) {
    partial <- .lazy_partial_matching(cost_matrix)
    # Hall's witness still says why no complete matching exists, beside the
    # largest one that does.
    if (!inherits(res, "error") && !is.null(res$witness)) {
      partial$result$witness <- res$witness
    }
    return(partial)
  }

  match_vec <- as.integer(res$match)
  matched_rows <- which(match_vec > 0L)
  matched_cols <- match_vec[matched_rows]

  duals <- .solver_duals(res, transposed = FALSE)
  potentials <- if (is.null(duals)) NULL else list(rows = duals$u, cols = duals$v)

  list(result = res, matched_rows = matched_rows, matched_cols = matched_cols,
       potentials = potentials)
}

# The maximum-cardinality minimum-cost matching of a specification, by the
# design loop over its one-to-one network. The rows and columns come back in
# the specification's own indices, which is what the design maps read.
.lazy_partial_matching <- function(spec) {
  knobs <- .implicit_defaults()
  raw <- lap_design_implicit(spec$left_mat, spec$right_mat, spec$distance,
                             lazy_cost_spec_inv_cov(spec), spec$max_distance,
                             lazy_cost_spec_calipers(spec),
                             "one_to_one", 0, 0, knobs$keep_per_row, knobs$width,
                             knobs$tol, knobs$max_rounds, TRUE)
  placed <- as.numeric(raw$flow) > 0
  matched_rows <- as.integer(raw$block$row[placed])
  matched_cols <- as.integer(raw$block$col[placed])
  if (!length(matched_rows)) {
    warning("No valid pairs found after applying constraints", call. = FALSE)
  }

  result <- list(
    match = NULL,
    status = raw$status,
    method_used = "implicit",
    search = list(
      seed_width      = as.integer(raw$search$seed_width),
      candidate_edges = as.numeric(raw$search$candidate_edges),
      possible_edges  = as.numeric(raw$search$possible_edges),
      edges_evaluated = as.numeric(raw$search$edges_evaluated),
      n_rounds        = as.integer(raw$search$n_rounds),
      rounds          = tibble::as_tibble(raw$search$rounds)
    )
  )
  list(result = result, matched_rows = matched_rows, matched_cols = matched_cols)
}

# ==============================================================================
# The compiled design
# ==============================================================================
# match_couples() offers three designs and none of them is written out as a
# network here. The design is named to the compilers in src/flow/flow_compile.h,
# which build it, check the structural property its solve relies on, and return
# the maps saying which of the caller's units each node stands for:
#
#   design       network                            solved as
#   --------------------------------------------------------------------------
#   1:1          one unit per row, a column          the assignment problem
#                admits one row                      R/lap_solve.R solves
#   k:1          the same, rows replicated k times   the same, read back
#                                                    through the replica map
#   replacement  k units per row, a column admits    each row's own k cheapest
#                every row                           columns
#
# Only the shape is compiled. Costs stay in the matrix this file built and the
# lowered problem is that matrix read through the maps, which is what keeps a
# lazy cost source reachable on the 1:1 path, where the maps are the identity
# and the matrix is passed to the solver untouched.
.couples_design <- function(n_rows, n_cols, replace = FALSE, ratio = 1L) {
  design <- if (isTRUE(replace)) {
    "with_replacement"
  } else if (ratio > 1L) {
    "fixed_ratio"
  } else {
    "one_to_one"
  }

  plan <- lap_flow_compile_couples(design, n_rows, n_cols, ratio)
  plan$row_unit <- as.integer(plan$row_unit)
  plan$col_unit <- as.integer(plan$col_unit)
  plan
}

# The matrix the compiled design is solved from: the caller's costs read through
# the design's maps. A design that did not reshape its input is solved from the
# matrix itself. A lazy cost spec is reshaped by reading its feature rows through
# the same maps, which replicates k:1 rows at the cost of their covariates rather
# than of their pairs. Its inverse covariance is taken before that, from the
# units themselves: the pooled covariance of replicated rows is a different
# matrix, and the distance is the one the caller's units define.
.couples_costs <- function(cost_matrix, plan) {
  if (!isTRUE(plan$reshaped)) {
    return(cost_matrix)
  }
  if (is_lazy_cost_spec(cost_matrix)) {
    spec <- cost_matrix
    spec$inv_cov <- lazy_cost_spec_inv_cov(cost_matrix)
    return(lazy_cost_spec_rows(spec, plan$row_unit, plan$col_unit))
  }
  cost_matrix[plan$row_unit, plan$col_unit, drop = FALSE]
}

# The pairs a solve produced, in the caller's ids, carrying the distance of each
# pair and one .{var}_diff column per matching variable. `rows` and `cols` index
# the two sides pair by pair, so every column here is read at the same position
# and the pairing is positional throughout rather than joined back together.
.pairs_tibble <- function(left, right, left_ids, right_ids,
                          rows, cols, distances, vars) {
  pairs <- tibble::tibble(
    left_id = left_ids[rows],
    right_id = right_ids[cols],
    distance = distances
  )

  for (v in vars) {
    pairs[[paste0(".", v, "_diff")]] <- left[[v]][rows] - right[[v]][cols]
  }

  pairs
}

# Read a solved assignment back as pairs in the caller's units. The solver
# answered in node offsets of the compiled design, and the maps turn each one
# into the left or right unit it stands for: the identity on the 1:1 design,
# replica e back to row e / k on the k:1 one.
#
# A pair the solver returned on a forbidden edge is dropped here, and its two
# units come back unmatched. The solver returns one when a complete matching
# demands it and pruning has left the forbidden entry in the matrix; reporting it
# would put a pair in the result at a price the same cost says is no pair at all.
.couples_pairs <- function(solved, plan, cost_matrix, left, right,
                           left_ids, right_ids, vars) {
  matched_rows <- plan$row_unit[solved$matched_rows]
  matched_cols <- plan$col_unit[solved$matched_cols]

  if (length(matched_rows) == 0L) {
    return(list(
      pairs = tibble::tibble(
        left_id = character(0),
        right_id = character(0),
        distance = numeric(0)
      ),
      matched_rows = integer(0),
      matched_cols = integer(0)
    ))
  }

  distances <- if (is_lazy_cost_spec(cost_matrix)) {
    lazy_pair_distances(cost_matrix, matched_rows, matched_cols)
  } else {
    cost_matrix[cbind(matched_rows, matched_cols)]
  }

  valid <- .is_valid_cost(distances)
  matched_rows <- matched_rows[valid]
  matched_cols <- matched_cols[valid]
  distances <- distances[valid]

  pairs <- .pairs_tibble(left, right, left_ids, right_ids,
                         matched_rows, matched_cols, distances, vars)

  list(pairs = pairs, matched_rows = matched_rows, matched_cols = matched_cols)
}

#' Shared single matching implementation
#'
#' Core logic for both optimal (LAP) and greedy matching without blocking.
#' Called by match_couples_single() (optimal and greedy paths).
#'
#' @param solver_fn Solver function (assignment or greedy_matching)
#' @param solver_params Named list of extra args passed to solver_fn
#' @param check_costs If TRUE, run check_cost_distribution before solving
#' @param strict_no_pairs If TRUE, call err_no_valid_pairs (stops); else warn
#' @return List with pairs tibble, unmatched list, and info list.
#' @keywords internal
.couples_single <- function(left, right, left_ids, right_ids,
                            vars, distance, weights, scale,
                            max_distance, calipers,
                            solver_fn, solver_params = list(),
                            check_costs = FALSE,
                            strict_no_pairs = FALSE,
                            replace = FALSE,
                            ratio = 1L,
                            sigma = NULL,
                            memory_mode = "auto") {

  # Build cost matrix
  cost_matrix <- build_cost_matrix(left, right, vars, distance, weights, scale,
                                   sigma = sigma, memory_mode = memory_mode)

  # Apply constraints
  cost_matrix <- apply_all_constraints(cost_matrix, left, right, vars,
                                       max_distance, calipers)

  # Check cost distribution if requested
  if (check_costs) {
    check_cost_distribution(cost_matrix, warn = TRUE)
  }

  # Check for valid pairs
  if (!has_valid_pairs(cost_matrix)) {
    if (strict_no_pairs) {
      err_no_valid_pairs("No valid pairs after applying constraints")
    } else {
      warning("No valid pairs found after applying constraints", call. = FALSE)
    }

    return(list(
      pairs = tibble::tibble(
        left_id = character(0),
        right_id = character(0),
        distance = numeric(0)
      ),
      unmatched = list(
        left = left_ids,
        right = right_ids
      ),
      info = list(
        n_matched = 0,
        total_distance = 0
      )
    ))
  }

  # --- The design, compiled ---
  plan <- .couples_design(nrow(cost_matrix), ncol(cost_matrix),
                          replace = replace, ratio = ratio)

  # --- Replacement matching, one row at a time ---
  if (identical(plan$route, "separable")) {
    return(.couples_replace(
      cost_matrix, left, right, left_ids, right_ids, vars, ratio, plan
    ))
  }

  # --- 1:1 and k:1 matching, as the assignment the design lowers to ---
  # Drop rows/cols with no allowed edges so the LAP solver sees a feasible
  # submatrix; the dropped indices return as unmatched. Without this filter
  # the C++ solvers raise "Infeasible: row N has no allowed edges" instead
  # of producing the partial matching the caller expects with max_distance
  # / calipers constraints.
  solved <- .solve_with_partial_feasibility(.couples_costs(cost_matrix, plan),
                                            solver_fn, solver_params)
  solver_result <- solved$result

  read <- .couples_pairs(
    solved, plan, cost_matrix, left, right, left_ids, right_ids, vars
  )
  pairs <- read$pairs

  unmatched_left <- setdiff(seq_len(nrow(left)), read$matched_rows)
  unmatched_right <- setdiff(seq_len(nrow(right)), read$matched_cols)

  info <- list(
    solver = if (is.null(solver_result)) NA_character_ else solver_result$method_used,
    n_matched = nrow(pairs),
    total_distance = sum(pairs$distance, na.rm = TRUE)
  )
  if (identical(plan$design, "fixed_ratio")) {
    info$ratio <- ratio
  }

  out <- list(
    pairs = pairs,
    unmatched = list(
      left = left_ids[unmatched_left],
      right = right_ids[unmatched_right]
    ),
    info = info
  )
  out$potentials <- .couples_potentials(solved$potentials, plan,
                                        left_ids, right_ids)

  # The proof and the search record sit at the top level, beside `status`, for
  # the reason `status` does: return_diagnostics = FALSE truncates `info` to
  # three fields, and a certificate that survives only a diagnostic call is not
  # one a caller can rely on.
  .carry_solve_evidence(out, solver_result)
}

# The solve's duals in the caller's units, named by id. A design that
# replicates a left unit k times solves k rows for it, one per partner, and
# each replica carries its own dual. The unit's dual in the k:1 flow LP is
# their largest: every replica's dual satisfies c_ij - u_r - v_j >= 0 on every
# admissible pair, so their largest still does, and the pairs the unit is
# matched to are arcs at their capacity of one, where the LP allows a reduced
# cost at or below zero. At an exact optimum the replicas' duals coincide.
.couples_potentials <- function(potentials, plan, left_ids, right_ids) {
  if (is.null(potentials)) {
    return(NULL)
  }
  by_unit <- function(values, unit, n) {
    out <- rep(NA_real_, n)
    known <- !is.na(values)
    if (any(known)) {
      best <- tapply(values[known], unit[known], max)
      out[as.integer(names(best))] <- as.numeric(best)
    }
    out
  }
  list(
    left = stats::setNames(by_unit(potentials$rows, plan$row_unit,
                                   length(left_ids)), as.character(left_ids)),
    right = stats::setNames(by_unit(potentials$cols, plan$col_unit,
                                    length(right_ids)), as.character(right_ids))
  )
}

# Per-block potentials merged into one pair over every unit. Blocks are
# separate solves over disjoint units and a pair across two blocks is not
# admissible, so the blocks' potentials together are a dual solution of the
# whole problem. A unit whose block has none reads NA.
.merge_potentials <- function(parts, left_ids, right_ids) {
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) {
    return(NULL)
  }
  spread <- function(side, ids) {
    out <- stats::setNames(rep(NA_real_, length(ids)), as.character(ids))
    values <- unlist(lapply(parts, `[[`, side))
    out[names(values)] <- values
    out
  }
  list(left = spread("left", left_ids), right = spread("right", right_ids))
}

# Move what a solve proved about itself onto the matching it produced.
.carry_solve_evidence <- function(out, solver_result) {
  if (is.null(solver_result)) {
    return(out)
  }
  if (!is.null(solver_result$certificate)) {
    out$certificate <- solver_result$certificate
  }
  if (!is.null(solver_result$search)) {
    out$search <- solver_result$search
  }
  if (!is.null(solver_result$witness)) {
    out$witness <- solver_result$witness
  }
  out
}

#' Replacement matching: each left picks its best right independently
#'
#' The compiled design gives every column capacity for every row, so the rows
#' never compete and the optimum of the whole network is each row's own cheapest
#' columns. `plan$per_row` is how many of them a row takes: the requested ratio,
#' or the column count when there are fewer columns than that. A lazy cost spec
#' is answered one row at a time in C++, through the same row search the
#' implicit loop seeds with, so no row of costs is ever held in R.
#'
#' The LP separates by row, so its duals are explicit. A row's dual is the
#' cost of the most expensive partner it took, which is its k-th cheapest
#' admissible cost, and every column's is zero, since no column has a capacity
#' to price. The pairs taken then reduce to at most zero and the pairs passed
#' over to at least zero, which are the optimality conditions of the flow LP
#' with each pair arc carrying at most one unit.
#'
#' @return List with pairs tibble, unmatched list, info list, and potentials.
#' @keywords internal
.couples_replace <- function(cost_matrix, left, right,
                             left_ids, right_ids, vars, ratio = 1L, plan) {
  k <- plan$per_row

  if (is_lazy_cost_spec(cost_matrix)) {
    found <- lap_replace_lazy(cost_matrix$left_mat, cost_matrix$right_mat,
                              cost_matrix$distance,
                              lazy_cost_spec_inv_cov(cost_matrix),
                              cost_matrix$max_distance,
                              lazy_cost_spec_calipers(cost_matrix), k)
    rows <- as.integer(found$rows)
    cols <- as.integer(found$cols)
    dists <- as.numeric(found$distance)
  } else {
    per_row <- lapply(seq_len(nrow(cost_matrix)), function(i) {
      row_costs <- cost_matrix[i, ]
      ordered_cols <- order(row_costs)[seq_len(k)]
      ordered_dists <- row_costs[ordered_cols]
      valid <- .is_valid_cost(ordered_dists)
      list(cols = ordered_cols[valid], dists = ordered_dists[valid])
    })
    counts <- vapply(per_row, function(x) length(x$cols), integer(1))
    rows <- rep.int(seq_along(per_row), counts)
    cols <- as.integer(unlist(lapply(per_row, `[[`, "cols")))
    dists <- as.numeric(unlist(lapply(per_row, `[[`, "dists")))
  }

  pairs <- if (length(rows)) {
    .pairs_tibble(left, right, left_ids, right_ids, rows, cols, dists, vars)
  } else {
    tibble::tibble(left_id = character(0), right_id = character(0),
                   distance = numeric(0))
  }

  row_dual <- rep(NA_real_, length(left_ids))
  if (length(rows)) {
    taken <- tapply(dists, rows, max)
    row_dual[as.integer(names(taken))] <- as.numeric(taken)
  }

  list(
    pairs = pairs,
    unmatched = list(
      left = setdiff(left_ids, unique(pairs$left_id)),
      right = setdiff(right_ids, unique(pairs$right_id))
    ),
    info = list(
      n_matched = nrow(pairs),
      total_distance = sum(pairs$distance, na.rm = TRUE),
      replace = TRUE,
      ratio = ratio
    ),
    potentials = list(
      left = stats::setNames(row_dual, as.character(left_ids)),
      right = stats::setNames(numeric(length(right_ids)),
                              as.character(right_ids))
    )
  )
}

#' Shared matching from precomputed distance object
#'
#' Core logic for both optimal (LAP) and greedy matching from distance objects.
#' Called by match_couples_from_distance() (optimal and greedy paths).
#'
#' @param solver_fn Solver function (assignment or greedy_matching)
#' @param solver_params Named list of extra args passed to solver_fn
#' @param check_costs If TRUE, run check_cost_distribution before solving
#' @param strict_no_pairs If TRUE, call err_no_valid_pairs (stops); else warn
#' @param method_label String for info$method (e.g., "from_distance_object")
#' @param extra_info Named list of extra fields to add to info
#' @return A matching_result object with pairs, info, and optional diagnostics.
#' @keywords internal
.couples_from_distance <- function(dist_obj,
                                   max_distance = Inf,
                                   calipers = NULL,
                                   ignore_blocks = FALSE,
                                   require_full_matching = FALSE,
                                   replace = FALSE,
                                   ratio = 1L,
                                   return_unmatched = TRUE,
                                   return_diagnostics = FALSE,
                                   solver_fn, solver_params = list(),
                                   check_costs = FALSE,
                                   strict_no_pairs = FALSE,
                                   method_label = "from_distance_object",
                                   extra_info = list(),
                                   diagnostics_fields = c("method", "n_matched",
                                                          "total_distance")) {

  # Extract from distance object
  cost_matrix <- dist_obj$cost_matrix
  left <- dist_obj$original_left
  right <- dist_obj$original_right
  left_ids <- dist_obj$left_ids
  right_ids <- dist_obj$right_ids

  # Apply additional constraints if specified
  if (!is.infinite(max_distance) || !is.null(calipers)) {
    cost_matrix <- apply_all_constraints(
      cost_matrix,
      left, right,
      dist_obj$metadata$vars,
      max_distance,
      calipers
    )
  }

  # compute_distances() prints that blocking will be applied when it keeps a
  # block variable, so the promise has to reach the solve. On a materialised
  # matrix a block is a forbidden set: every cross-block pair is removed,
  # which leaves the feasible set the blocked data-frame path solves one
  # stratum at a time. A unit whose block is missing joins no stratum.
  if (!isTRUE(ignore_blocks) && !is.null(dist_obj$block_id)) {
    bcol <- dist_obj$block_id
    if (!bcol %in% names(left) || !bcol %in% names(right)) {
      stop("block variable ", bcol, " is recorded on the distance object ",
           "but is not in the data it was built from", call. = FALSE)
    }
    if (is_lazy_cost_spec(cost_matrix)) {
      stop("blocking a distance object built with memory_mode = ",
           lazy_cost_spec_mode(cost_matrix), " is not supported yet; rebuild ",
           "it with memory_mode = dense, or pass ignore_blocks = TRUE",
           call. = FALSE)
    }
    lb <- as.character(left[[bcol]])
    rb <- as.character(right[[bcol]])
    same <- outer(lb, rb, function(a, b) !is.na(a) & !is.na(b) & a == b)
    cost_matrix[!same] <- Inf
  }

  # Check cost distribution if requested
  if (check_costs) {
    check_cost_distribution(cost_matrix, warn = TRUE)
  }

  # Check for valid pairs
  if (!has_valid_pairs(cost_matrix)) {
    if (strict_no_pairs) {
      err_no_valid_pairs("No valid pairs after applying constraints")
    } else {
      warning("No valid pairs found after applying constraints", call. = FALSE)
    }

    info <- c(
      list(
        method = method_label,
        n_matched = 0,
        total_distance = 0,
        n_left = length(left_ids),
        n_right = length(right_ids)
      ),
      extra_info,
      design_estimand(length(left_ids), 0L)
    )

    return(structure(
      list(
        pairs = tibble::tibble(
          left_id = character(0),
          right_id = character(0),
          distance = numeric(0)
        ),
        unmatched = list(
          left = left_ids,
          right = right_ids
        ),
        info = info
      ),
      class = c("matching_result", "couplr_result")
    ))
  }

  # A precomputed distance object reaches the same designs through another
  # door, so it compiles to the same network and is solved through the same
  # maps. Distances are reported alone here: no variable goes to .couples_pairs
  # and no difference column is written.
  plan <- .couples_design(nrow(cost_matrix), ncol(cost_matrix),
                          replace = replace, ratio = ratio)

  if (identical(plan$route, "separable")) {
    return(.couples_replace(
      cost_matrix, left, right, left_ids, right_ids, character(0), ratio, plan
    ))
  }

  # Solve with row/col filtering (see .solve_with_partial_feasibility)
  solved <- .solve_with_partial_feasibility(.couples_costs(cost_matrix, plan),
                                            solver_fn, solver_params)
  solver_result <- solved$result

  read <- .couples_pairs(
    solved, plan, cost_matrix, left, right, left_ids, right_ids,
    vars = character(0)
  )
  pairs <- read$pairs

  unmatched_left <- setdiff(seq_along(left_ids), read$matched_rows)
  unmatched_right <- setdiff(seq_along(right_ids), read$matched_cols)

  info <- c(
    list(
      method = method_label,
      solver = if (is.null(solver_result)) NA_character_ else solver_result$method_used,
      n_matched = nrow(pairs),
      total_distance = sum(pairs$distance),
      distance_metric = dist_obj$metadata$distance,
      scaled = !identical(dist_obj$metadata$scale, FALSE),
      n_left = length(left_ids),
      n_right = length(right_ids)
    ),
    extra_info
  )

  result <- list(
    pairs = pairs,
    unmatched = list(
      left = left_ids[unmatched_left],
      right = right_ids[unmatched_right]
    ),
    info = info
  )
  result$potentials <- .couples_potentials(solved$potentials, plan,
                                           left_ids, right_ids)

  # Check for full matching if required
  if (require_full_matching) {
    check_full_matching(result)
  }

  # Before the truncation below removes info$solver and return_unmatched removes
  # the unmatched ids, both of which the status is read from.
  result$status <- .matching_status(
    solver      = result$info$solver,
    greedy      = identical(method_label, "greedy"),
    n_pairs     = nrow(result$pairs),
    n_requested = length(left_ids)
  )

  result$info <- c(
    result$info,
    design_estimand(length(left_ids), dplyr::n_distinct(result$pairs$left_id))
  )

  if (!return_unmatched) {
    result$unmatched <- NULL
  }

  if (!return_diagnostics) {
    result$info <- result$info[
      c(diagnostics_fields, "estimand", "focal", "focal_discarded")
    ]
  }

  structure(result, class = c("matching_result", "couplr_result"))
}

# One block's row of the summary, and the whole blocked matching's info. Both
# branches of .couples_blocked() build theirs here, because they used to build
# their own: the two disagreed on which fields were present, on their order, and
# on whether a block with nothing to match got a row at all.
#
# Every block gets a row, matched or not, so nrow(block_summary) is the block
# count on both branches.
.block_summary_row <- function(block_id, n_left, n_right, pairs,
                               n_unmatched_left, n_unmatched_right) {
  tibble::tibble(
    block_id = as.character(block_id),
    n_left = as.integer(n_left),
    n_right = as.integer(n_right),
    n_matched = nrow(pairs),
    total_distance = sum(pairs$distance, na.rm = TRUE),
    mean_distance = if (nrow(pairs) > 0L) {
      mean(pairs$distance, na.rm = TRUE)
    } else {
      NA_real_
    },
    n_unmatched_left = as.integer(n_unmatched_left),
    n_unmatched_right = as.integer(n_unmatched_right)
  )
}

.empty_block_summary <- function() {
  tibble::tibble(
    block_id = character(0),
    n_left = integer(0),
    n_right = integer(0),
    n_matched = integer(0),
    total_distance = numeric(0),
    mean_distance = numeric(0),
    n_unmatched_left = integer(0),
    n_unmatched_right = integer(0)
  )
}

# `solvers` is one method per block that ran a solve, which is what carries a
# block's greedy fallback out to the status. Reporting the requested method
# here instead reports every block as having run it, including the one that
# did not.
.blocked_info <- function(pairs, n_blocks, block_summary, solvers) {
  list(
    solver = if (length(solvers) > 0L) unique(solvers) else NA_character_,
    blocked = TRUE,
    n_blocks = n_blocks,
    n_matched = nrow(pairs),
    total_distance = sum(pairs$distance, na.rm = TRUE),
    block_summary = block_summary
  )
}

#' Shared blocked matching implementation
#'
#' Core logic for both optimal (LAP) and greedy blocked matching.
#' Called by match_couples_blocked() (optimal and greedy paths).
#'
#' @param solver_fn Solver function (assignment or greedy_matching)
#' @param solver_params Named list of extra args passed to solver_fn
#' @param check_costs If TRUE, passed through to .couples_single
#' @param strict_no_pairs If TRUE, passed through to .couples_single
#' @return List with pairs tibble, unmatched list, and info list.
#' @keywords internal
.couples_blocked <- function(left, right, left_ids, right_ids,
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

  blocks <- unique(c(left[[block_col]], right[[block_col]]))

  # Use parallel processing if requested and available
  if (parallel && length(blocks) > 1) {
    result <- .blocks_parallel(
      blocks, left, right, left_ids, right_ids,
      block_col, vars, distance, weights, scale,
      max_distance, calipers,
      solver_fn = solver_fn, solver_params = solver_params,
      check_costs = check_costs, strict_no_pairs = strict_no_pairs,
      parallel = TRUE,
      replace = replace, ratio = ratio,
      sigma = sigma, memory_mode = memory_mode
    )

    # Reorder columns to put block_id first
    if (nrow(result$pairs) > 0) {
      result$pairs <- dplyr::select(result$pairs, "block_id", dplyr::everything())
    }

    return(list(
      pairs = result$pairs,
      unmatched = result$unmatched,
      info = .blocked_info(result$pairs, length(blocks),
                           result$block_summary, result$solvers),
      potentials = .merge_potentials(result$potentials, left_ids, right_ids)
    ))
  }

  # Sequential processing
  all_pairs <- list()
  all_unmatched_left <- character(0)
  all_unmatched_right <- character(0)
  block_summaries <- list()
  block_potentials <- list()
  solvers <- character(0)

  for (block in blocks) {
    left_block <- left[left[[block_col]] == block, ]
    right_block <- right[right[[block_col]] == block, ]

    if (nrow(left_block) == 0 || nrow(right_block) == 0) {
      # A block with nothing on one side runs no solve, so it contributes no
      # solver; its units are unmatched and it still gets a summary row.
      block_left_ids <- character(0)
      block_right_ids <- character(0)
      if (nrow(left_block) > 0) {
        block_left_ids <- left_ids[left[[block_col]] == block]
        all_unmatched_left <- c(all_unmatched_left, block_left_ids)
      }
      if (nrow(right_block) > 0) {
        block_right_ids <- right_ids[right[[block_col]] == block]
        all_unmatched_right <- c(all_unmatched_right, block_right_ids)
      }
      block_summaries[[length(block_summaries) + 1]] <- .block_summary_row(
        block_id = block,
        n_left = nrow(left_block),
        n_right = nrow(right_block),
        pairs = tibble::tibble(distance = numeric(0)),
        n_unmatched_left = length(block_left_ids),
        n_unmatched_right = length(block_right_ids)
      )
      next
    }

    # Get IDs for this block
    block_left_ids <- left_ids[left[[block_col]] == block]
    block_right_ids <- right_ids[right[[block_col]] == block]

    # Match within block
    block_result <- .couples_single(
      left_block, right_block, block_left_ids, block_right_ids,
      vars, distance, weights, scale,
      max_distance, calipers,
      solver_fn = solver_fn, solver_params = solver_params,
      check_costs = check_costs, strict_no_pairs = strict_no_pairs,
      replace = replace, ratio = ratio,
      sigma = sigma, memory_mode = memory_mode
    )

    # Add block_id column
    if (nrow(block_result$pairs) > 0) {
      block_result$pairs$block_id <- block
      all_pairs[[length(all_pairs) + 1]] <- block_result$pairs
    }

    # Accumulate unmatched
    all_unmatched_left <- c(all_unmatched_left, block_result$unmatched$left)
    all_unmatched_right <- c(all_unmatched_right, block_result$unmatched$right)

    solvers <- c(solvers, block_result$info$solver)
    block_potentials[[length(block_potentials) + 1]] <- block_result$potentials

    # Block summary
    block_summaries[[length(block_summaries) + 1]] <- .block_summary_row(
      block_id = block,
      n_left = nrow(left_block),
      n_right = nrow(right_block),
      pairs = block_result$pairs,
      n_unmatched_left = length(block_result$unmatched$left),
      n_unmatched_right = length(block_result$unmatched$right)
    )
  }

  # Combine results
  if (length(all_pairs) > 0) {
    pairs <- dplyr::bind_rows(all_pairs)
    # Reorder columns to put block_id first
    pairs <- dplyr::select(pairs, "block_id", dplyr::everything())
  } else {
    pairs <- tibble::tibble(
      block_id = character(0),
      left_id = character(0),
      right_id = character(0),
      distance = numeric(0)
    )
  }

  block_summary_df <- if (length(block_summaries) > 0) {
    dplyr::bind_rows(block_summaries)
  } else {
    .empty_block_summary()
  }

  list(
    pairs = pairs,
    unmatched = list(
      left = all_unmatched_left,
      right = all_unmatched_right
    ),
    info = .blocked_info(pairs, length(blocks), block_summary_df, solvers),
    potentials = .merge_potentials(block_potentials, left_ids, right_ids)
  )
}

# ==============================================================================
# Exported Matching Functions
# ==============================================================================

#' Match two datasets into couples
#'
#' Performs one-to-one matching between two datasets. Supports blocking,
#' distance constraints, and various distance metrics.
#'
#' With `method` set to a LAP solver (the default `"auto"`, or `"jv"`,
#' `"hungarian"`, ...) it finds the matching that minimizes total distance among
#' all feasible matchings. With `method = "greedy"` it uses a fast greedy
#' strategy (selected by `strategy`) that does not guarantee the optimal total
#' distance but scales to very large datasets.
#'
#' @param left Data frame of "left" units (e.g., treated, cases)
#' @param right Data frame of "right" units (e.g., control, controls)
#' @param left_id,right_id Name of the column holding the unit identifier, or
#'   NULL (default) to use a column called `id`, then meaningful row names,
#'   then synthesized ids `left_1 ... left_n` / `right_1 ... right_m` with a
#'   warning. The values of this column are what `pairs$left_id` and
#'   `pairs$right_id` carry, and what [join_matched()], [match_data()],
#'   [balance_diagnostics()], [sensitivity_analysis()] and [as_matchit()] join
#'   on, so the same column name is passed to those verbs. Ids read from the
#'   data must be unique.
#' @param vars Variable names to use for distance computation
#' @param distance Distance metric: "euclidean", "manhattan", "mahalanobis",
#'   or a custom function
#' @param weights Optional named vector of variable weights
#' @param scale Scaling method: FALSE (none), "standardize", "range", or "robust"
#' @param auto_scale If TRUE, automatically check variable health and select
#'   scaling method (default: FALSE)
#' @param max_distance Maximum allowed distance (pairs exceeding this are forbidden)
#' @param calipers Named list of per-variable maximum absolute differences
#' @param block_id Column name containing block IDs (for stratified matching)
#' @param ignore_blocks If TRUE, ignore block_id even if present
#' @param require_full_matching If TRUE, error if any units remain unmatched
#' @param method Matching method. A LAP solver for optimal matching ("auto",
#'   "hungarian", "jv", "gabow_tarjan", ...), or "greedy" for fast approximate
#'   matching (see `strategy`).
#' @param strategy Greedy strategy, used only when `method = "greedy"`. All
#'   three strategies solve the same full cost matrix already built by
#'   `match_couples()`; none of them reduce the O(n*m) memory that matrix
#'   takes.
#'   - "row_best": for each row, take its best available column (default).
#'     The only strategy that needs no extra storage beyond the cost matrix.
#'   - "sorted": collect every valid pair, sort by distance, greedily assign
#'   - "pq": collect every valid pair into a heap and pop the smallest first.
#'     Avoids the upfront sort but holds the same number of candidate pairs
#'     as "sorted", so it is not more memory-efficient
#' @param return_unmatched Include unmatched units in output
#' @param return_diagnostics Include detailed diagnostics in output
#' @param parallel Enable parallel processing for blocked matching.
#'   Requires 'future' and 'future.apply' packages. Can be:
#'   - `FALSE`: Sequential processing (default)
#'   - `TRUE`: Auto-configure parallel backend
#'   - Character: Specify future plan (e.g., "multisession", "multicore")
#' @param replace If TRUE, allow matching with replacement (same right unit
#'   can be matched to multiple left units). Default: FALSE.
#' @param ratio Integer, number of right units to match per left unit.
#'   Default: 1 (one-to-one matching). For k:1 matching, set ratio = k.
#' @param check_costs If TRUE, check distance distribution for potential problems
#'   and provide helpful warnings before matching (default: TRUE)
#' @param sigma Optional covariance matrix for Mahalanobis distance. If NULL
#'   (default), the pooled sample covariance is used. Only relevant when
#'   \code{distance = "mahalanobis"}.
#' @param memory_mode One of "auto" (default), "dense", "lazy" or "implicit".
#'   "auto" warns (or, when `method` is `"jv"`/`"auction"`, switches) when the dense cost matrix would consume a large
#'   fraction of free system RAM. "lazy" computes each pairwise distance from
#'   the underlying feature data as the solver needs it, instead of
#'   allocating the full n_left x n_right matrix; supported for `method =
#'   "jv"`/`"auction"`, including
#'   `replace = TRUE`, where each left unit's cheapest partners are found one
#'   row at a time, and `ratio > 1`, where the left units' covariates are
#'   replicated rather than their rows of distances, and not yet for
#'   `method = "greedy"` (blocking via `block_id` is the other option that
#'   reduces memory, by solving smaller sub-problems). A custom distance
#'   function is called on a block of left units against every right unit,
#'   the same contract the dense path calls it under, with the block sized so
#'   the matrix it returns stays near a million cells; each matched pair is
#'   then evaluated again alone, and a distance that depends on the other units
#'   in its call is an error. When the constraints admit no complete matching, the
#'   largest matching they admit, cheapest among those, is found by the
#'   edge-generation loop over the same specification, as the dense path finds
#'   it by padding. Where the metric carries a ball
#'   bound, the column set is held in a ball tree and a subtree whose bound
#'   cannot beat the current threshold is discarded without being read:
#'   `"mahalanobis"` always, the metrics linear in the covariates up to six of
#'   them. `"manhattan"` and `"chebyshev"`, a covariance with no Cholesky
#'   factor, and a higher-dimensional linear metric read the columns instead. "implicit" states the problem
#'   over every pair and solves it over a fraction of them, generating the pairs
#'   the answer turns out to need and proving that the ones it never generated
#'   could not have improved it; same requirements as "lazy". On
#'   the eight-covariate problem the benchmarks use it leads "lazy" from 5,000
#'   units upward, by 1.1x at 5,000 rising to 3.1x at 50,000, and loses below
#'   that where the loop's fixed costs are still visible; what it buys at every
#'   size is the certificate over the complete problem. "auto" never selects
#'   it. "dense" skips the RAM check entirely.
#' @param certify Logical; whether the result carries a checked
#'   `assignment_certificate` as `certificate`. Applies to
#'   `memory_mode = "implicit"`, where it defaults to `TRUE`: the certificate is
#'   what separates the answer from an approximate one. On the other paths the
#'   matching is certified after the fact with [verify_assignment()], against
#'   the cost matrix it was solved from.
#'
#' @return A list with class "matching_result" containing:
#'   - `pairs`: Tibble of matched pairs with distances
#'   - `unmatched`: List of unmatched left and right IDs
#'   - `info`: Matching diagnostics and metadata
#'   - `status`: One of [solver_status_values()], computed from what the solve
#'     achieved. `"optimal"` when every left unit found a partner under an
#'     optimal method, `"partial"` when constraints left some unmatched,
#'     `"heuristic"` when a greedy method ran, either because it was asked for
#'     or because the constrained path fell back to it, and `"infeasible"` when
#'     nothing could be matched.
#'
#'   - `potentials`: The dual potentials of the design's linear program, a list
#'     with elements `left` and `right` holding one value per unit, named by
#'     id. The reduced cost of a pair is its distance minus the two
#'     potentials: at least zero on every admissible pair the matching left
#'     out, zero on every pair of a 1:1 matching, and at most zero on the pairs
#'     of a k:1 or with-replacement matching, whose pair arcs carry at most
#'     one unit. A left unit's potential on the k:1 design is the largest of
#'     its replicas'. With replacement the LP separates by row: a left unit's
#'     potential is the distance to the farthest partner it took and every
#'     right unit's is zero. A unit with no admissible partner, or in a block
#'     with none, reads `NA`. Absent for `method = "greedy"` and for a
#'     constrained problem answered by maximum cardinality, which is not the
#'     LP the potentials belong to. On the 1:1 design they are the duals
#'     [verify_assignment()] checks, and a method that returns none has them
#'     computed by [assignment_duals()].
#'
#'   Under `memory_mode = "implicit"` it also carries `certificate`, the checked
#'   statement of optimality (see [verify_assignment()], which names the
#'   arithmetic it was decided in), and `search`: the pairs
#'   the loop generated out of the pairs the problem states, the pairs a cost
#'   was computed for, and one row per round of what each round did. An
#'   infeasible answer carries `witness` instead, naming the units that could
#'   not be matched and the partners they have between them.
#'
#' @examples
#' # Basic matching
#' left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5), y = c(2, 4, 6, 8, 10))
#' right <- data.frame(id = 6:10, x = c(1.1, 2.2, 3.1, 4.2, 5.1), y = c(2.1, 4.1, 6.2, 8.1, 10.1))
#' result <- match_couples(left, right, vars = c("x", "y"))
#' print(result$pairs)
#'
#' # With constraints
#' result <- match_couples(left, right, vars = c("x", "y"),
#'                         max_distance = 1,
#'                         calipers = list(x = 0.5))
#'
#' # With blocking
#' left$region <- c("A", "A", "B", "B", "B")
#' right$region <- c("A", "A", "B", "B", "B")
#' blocks <- matchmaker(left, right, block_type = "group", block_by = "region")
#' result <- match_couples(blocks$left, blocks$right, vars = c("x", "y"))
#'
#' # Fast greedy matching for large datasets
#' result <- match_couples(left, right, vars = c("x", "y"),
#'                         method = "greedy", strategy = "sorted")
#'
#' @export
match_couples <- function(left, right = NULL,
                          vars = NULL,
                          left_id = NULL,
                          right_id = NULL,
                          distance = "euclidean",
                          weights = NULL,
                          scale = FALSE,
                          auto_scale = FALSE,
                          max_distance = Inf,
                          calipers = NULL,
                          block_id = NULL,
                          ignore_blocks = FALSE,
                          require_full_matching = FALSE,
                          method = "auto",
                          strategy = c("row_best", "sorted", "pq"),
                          return_unmatched = TRUE,
                          return_diagnostics = FALSE,
                          parallel = FALSE,
                          replace = FALSE,
                          ratio = 1L,
                          check_costs = TRUE,
                          sigma = NULL,
                          memory_mode = "auto",
                          certify = NULL) {

  strategy <- match.arg(strategy)
  greedy <- identical(method, "greedy")
  implicit <- identical(memory_mode, "implicit")

  if (!is.null(certify) && !implicit) {
    stop("`certify` applies to memory_mode = \"implicit\", where the ",
         "certificate comes out of the solve. Elsewhere, certify a matching ",
         "with verify_assignment() against the cost matrix it was solved ",
         "from.", call. = FALSE)
  }

  # Validate replace and ratio
  if (!is.logical(replace) || length(replace) != 1) {
    stop("replace must be TRUE or FALSE", call. = FALSE)
  }
  ratio <- as.integer(ratio)
  if (length(ratio) != 1 || is.na(ratio) || ratio < 1L) {
    stop("ratio must be a positive integer", call. = FALSE)
  }

  # Check if left is a distance_object
  if (is_distance_object(left)) {
    # The distances are already materialised, so there is no representation
    # left to choose. Naming one would read as a setting that did something.
    if (!identical(memory_mode, "auto")) {
      stop("memory_mode does not apply to a precomputed distance object; it ",
           "was fixed when compute_distances() built it", call. = FALSE)
    }
    return(match_couples_from_distance(
      left,
      max_distance = max_distance,
      calipers = calipers,
      ignore_blocks = ignore_blocks,
      require_full_matching = require_full_matching,
      replace = replace,
      ratio = ratio,
      certify = certify,
      method = method,
      strategy = strategy,
      return_unmatched = return_unmatched,
      return_diagnostics = return_diagnostics,
      check_costs = check_costs
    ))
  }

  # Standard path: left and right are datasets
  if (is.null(right)) {
    couplr_stop("When left is a dataset, right must be provided\n",
                "  ", couplr_emoji("search"),
                "Need two datasets to make couples!")
  }

  if (is.null(vars)) {
    couplr_stop("When left is a dataset, vars must be specified\n",
                "  ", couplr_emoji("info"),
                "Use vars = c('var1', 'var2', ...) to specify matching ",
                "variables")
  }

  # Apply automatic preprocessing if requested
  if (auto_scale) {
    preproc <- preprocess_matching_vars(
      left, right, vars,
      auto_scale = TRUE,
      scale_method = if (identical(scale, FALSE)) "auto" else scale,
      check_health = TRUE,
      remove_problematic = TRUE,
      verbose = TRUE
    )

    # Update vars and scale based on preprocessing
    vars <- preproc$vars
    if (preproc$scaling_method != "none") {
      scale <- preproc$scaling_method
    }
  }

  # Validate inputs
  validate_matching_inputs(left, right, vars)
  weights <- validate_weights(weights, vars)
  calipers <- validate_calipers(calipers, vars)

  # Extract IDs
  left_ids <- extract_ids(left, "left", left_id, warn_synthetic = TRUE)
  right_ids <- extract_ids(right, "right", right_id, warn_synthetic = TRUE)

  # Store original row indices
  left$..row_idx <- seq_len(nrow(left))
  right$..row_idx <- seq_len(nrow(right))

  # Detect blocking
  block_info <- detect_blocking(left, right, block_id, ignore_blocks)

  if (block_info$use_blocking && implicit) {
    # Each block is its own solve and would carry its own certificate, and a
    # certificate per block is not a certificate for the matching. Blocking is
    # also the other answer to the memory the loop addresses, so the two are
    # alternatives rather than a combination.
    stop("memory_mode = \"implicit\" is not supported with blocking: each ",
         "block is a separate solve, and one certificate per block is not a ",
         "proof about the matching. Drop block_id, or use ",
         "ignore_blocks = TRUE.", call. = FALSE)
  }

  if (block_info$use_blocking) {
    # Setup parallel processing if requested
    parallel_state <- setup_parallel(parallel)
    on.exit(restore_parallel(parallel_state), add = TRUE)

    # Blocked matching
    result <- match_couples_blocked(
      left, right, left_ids, right_ids,
      block_col = block_info$block_col,
      vars = vars, distance = distance, weights = weights, scale = scale,
      max_distance = max_distance, calipers = calipers,
      method = method, strategy = strategy,
      parallel = parallel_state$setup,
      replace = replace, ratio = ratio,
      sigma = sigma, memory_mode = memory_mode
    )
  } else {
    # Single matching
    result <- match_couples_single(
      left, right, left_ids, right_ids,
      vars = vars, distance = distance, weights = weights, scale = scale,
      max_distance = max_distance, calipers = calipers,
      method = method, strategy = strategy,
      check_costs = check_costs,
      replace = replace, ratio = ratio,
      sigma = sigma, memory_mode = memory_mode, certify = certify
    )
  }

  # Clean up temporary column
  left$..row_idx <- NULL
  right$..row_idx <- NULL

  # Check for full matching if required
  if (require_full_matching) {
    check_full_matching(result)
  }

  # Add metadata
  result$info$method <- if (greedy) "greedy" else "lap"
  if (greedy) result$info$strategy <- strategy
  result$info$distance_metric <- distance
  result$info$scaled <- !identical(scale, FALSE)
  result$info$n_left <- nrow(left)
  result$info$n_right <- nrow(right)
  if (replace) result$info$replace <- TRUE
  if (ratio > 1L) result$info$ratio <- ratio
  result$info <- c(
    result$info,
    design_estimand(nrow(left),
                    dplyr::n_distinct(result$pairs$left_id))
  )

  # Computed here because info$solver is about to go: the truncation below drops
  # it. Status sits at the top level for the same reason -- inside info it would
  # not survive a default call.
  #
  # A design asks for `ratio` partners per left unit, so that product is what a
  # complete matching places, on the k:1 and with-replacement designs alike.
  result$status <- .matching_status(
    solver      = result$info$solver,
    greedy      = greedy,
    n_pairs     = nrow(result$pairs),
    n_requested = nrow(left) * ratio
  )

  if (!return_unmatched) {
    result$unmatched <- NULL
  }

  if (!return_diagnostics) {
    if (greedy) {
      keep <- c("method", "strategy", "n_matched", "total_distance")
      if (!is.null(result$info$n_blocks)) keep <- c(keep, "n_blocks")
    } else {
      keep <- c("method", "n_matched", "total_distance")
    }
    # The estimand survives the truncation for the reason `status` sits at the
    # top level: it is what as_matchit() labels the target population with,
    # and a field only a diagnostic call carries is one that door cannot read.
    keep <- c(keep, "estimand", "focal", "focal_discarded")
    result$info <- result$info[keep]
  }

  structure(result, class = c("matching_result", "couplr_result"))
}

#' Match from Precomputed Distance Object
#'
#' Internal function to handle matching when a distance_object is provided
#'
#' @return A matching_result object with pairs, info, and optional diagnostics.
#' @keywords internal
match_couples_from_distance <- function(dist_obj,
                                        max_distance = Inf,
                                        calipers = NULL,
                                        ignore_blocks = FALSE,
                                        require_full_matching = FALSE,
                                        replace = FALSE,
                                        ratio = 1L,
                                        certify = NULL,
                                        method = "auto",
                                        strategy = "row_best",
                                        return_unmatched = TRUE,
                                        return_diagnostics = FALSE,
                                        check_costs = TRUE) {
  greedy <- identical(method, "greedy")
  .couples_from_distance(
    dist_obj,
    max_distance = max_distance,
    calipers = calipers,
    ignore_blocks = ignore_blocks,
    require_full_matching = require_full_matching,
    replace = replace,
    ratio = ratio,
    return_unmatched = return_unmatched,
    return_diagnostics = return_diagnostics,
    solver_fn = if (greedy) greedy_matching else assignment,
    solver_params = if (greedy) {
      list(strategy = strategy)
    } else if (is.null(certify)) {
      list(method = method)
    } else {
      list(method = method, certify = certify)
    },
    check_costs = if (greedy) FALSE else check_costs,
    strict_no_pairs = !greedy,
    method_label = if (greedy) "greedy" else "from_distance_object",
    extra_info = if (greedy) list(strategy = strategy) else NULL,
    diagnostics_fields = if (greedy) {
      c("method", "strategy", "n_matched", "total_distance")
    } else {
      c("method", "n_matched", "total_distance")
    }
  )
}

#' Match without blocking (single problem)
#'
#' @return List with pairs tibble and matching info.
#' @keywords internal
match_couples_single <- function(left, right, left_ids, right_ids,
                                 vars, distance, weights, scale,
                                 max_distance, calipers, method,
                                 strategy = "row_best",
                                 check_costs = TRUE,
                                 replace = FALSE, ratio = 1L,
                                 sigma = NULL,
                                 memory_mode = "auto",
                                 certify = NULL) {
  greedy <- identical(method, "greedy")
  .couples_single(
    left, right, left_ids, right_ids,
    vars, distance, weights, scale,
    max_distance, calipers,
    solver_fn = if (greedy) greedy_matching else assignment,
    solver_params = if (greedy) {
      list(strategy = strategy)
    } else if (is.null(certify)) {
      list(method = method)
    } else {
      list(method = method, certify = certify)
    },
    check_costs = if (greedy) FALSE else check_costs,
    strict_no_pairs = !greedy,
    replace = replace, ratio = ratio,
    sigma = sigma, memory_mode = memory_mode
  )
}

#' Match with blocking (multiple problems)
#'
#' @return List with pairs tibble and matching info.
#' @keywords internal
match_couples_blocked <- function(left, right, left_ids, right_ids,
                                  block_col, vars, distance, weights, scale,
                                  max_distance, calipers, method,
                                  strategy = "row_best",
                                  parallel = FALSE,
                                  replace = FALSE, ratio = 1L,
                                  sigma = NULL,
                                  memory_mode = "auto") {
  greedy <- identical(method, "greedy")
  .couples_blocked(
    left, right, left_ids, right_ids,
    block_col, vars, distance, weights, scale,
    max_distance, calipers,
    solver_fn = if (greedy) greedy_matching else assignment,
    solver_params = if (greedy) list(strategy = strategy) else list(method = method),
    check_costs = FALSE,
    strict_no_pairs = !greedy,
    parallel = parallel,
    replace = replace, ratio = ratio,
    sigma = sigma, memory_mode = memory_mode
  )
}

#' Detect and validate blocking
#'
#' @return List with use_blocking (logical) and block_col (character or NULL).
#' @keywords internal
detect_blocking <- function(left, right, block_id, ignore_blocks) {
  if (ignore_blocks) {
    return(list(use_blocking = FALSE, block_col = NULL))
  }

  # Explicit block_id specified
  if (!is.null(block_id)) {
    if (!(block_id %in% names(left))) {
      stop(sprintf("block_id column '%s' not found in left", block_id), call. = FALSE)
    }
    if (!(block_id %in% names(right))) {
      stop(sprintf("block_id column '%s' not found in right", block_id), call. = FALSE)
    }
    return(list(use_blocking = TRUE, block_col = block_id))
  }

  # Auto-detect block column
  left_block_col <- get_block_id_column(left)
  right_block_col <- get_block_id_column(right)

  if (!is.null(left_block_col) && !is.null(right_block_col)) {
    if (left_block_col == right_block_col) {
      return(list(use_blocking = TRUE, block_col = left_block_col))
    }
  }

  list(use_blocking = FALSE, block_col = NULL)
}

#' Check if full matching was achieved
#'
#' @return No return value; throws error if unmatched units exist.
#' @keywords internal
check_full_matching <- function(result) {
  n_unmatched <- length(result$unmatched$left) + length(result$unmatched$right)

  if (n_unmatched > 0) {
    stop(
      sprintf("Full matching required but %d units remain unmatched:\n", n_unmatched),
      sprintf("  - %d left unmatched\n", length(result$unmatched$left)),
      sprintf("  - %d right unmatched\n", length(result$unmatched$right)),
      "Consider relaxing constraints (max_distance, calipers) or set require_full_matching = FALSE",
      call. = FALSE
    )
  }

  invisible(TRUE)
}
