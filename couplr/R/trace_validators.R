# ==============================================================================
# Trace validators: full per-frame parity checks
# ==============================================================================
# These helpers verify that a trace produced by a registered trace function
# describes a sequence of *valid partial states* of an assignment algorithm,
# and that the final state agrees with the production C++ oracle.
#
# Used by tests/testthat/test-trace-parity.R for every registered method, and
# available as internal helpers for one-off debugging of new trace functions.
#
# Checks performed:
#
#   structural  - matching is integer length n_rows, entries in 0..n_cols,
#                 each non-zero column appears at most once (no double-bookings).
#   no_forbidden - any non-zero matching[i] must satisfy is.finite(cost[i, j]).
#   final_cost   - meta$total_cost (and the cost recomputed from the final
#                  frame's matching) matches the C++ oracle within tolerance.
#                  Sum for standard LAP; max-edge for bottleneck.
#
# Per-frame dual feasibility is intentionally NOT validated here: many
# algorithms (auction, cycle-cancel, push-relabel) traverse dual-infeasible
# intermediate states by design. Algorithm-specific invariants belong in
# algorithm-specific tests.
# ==============================================================================

#' @keywords internal
#' @noRd
validate_partial_matching <- function(matching, n_rows, n_cols, frame_idx = NA_integer_) {
  loc <- if (is.na(frame_idx)) "" else sprintf(" (frame %d)", frame_idx)
  if (!is.integer(matching) && !is.numeric(matching)) {
    stop("matching", loc, " must be integer/numeric, got ", typeof(matching), call. = FALSE)
  }
  if (length(matching) != n_rows) {
    stop("matching", loc, " must have length n_rows = ", n_rows,
         ", got length ", length(matching), call. = FALSE)
  }
  if (any(is.na(matching))) {
    stop("matching", loc, " contains NA at positions ",
         paste(which(is.na(matching)), collapse = ", "), call. = FALSE)
  }
  m_int <- as.integer(matching)
  if (any(m_int < 0L | m_int > n_cols)) {
    bad <- which(m_int < 0L | m_int > n_cols)
    stop("matching", loc, " has out-of-range entries at positions ",
         paste(bad, collapse = ", "),
         " (legal range: 0..", n_cols, ")", call. = FALSE)
  }
  nz <- m_int[m_int > 0L]
  if (anyDuplicated(nz)) {
    dups <- nz[duplicated(nz)]
    stop("matching", loc, " double-books columns: ",
         paste(unique(dups), collapse = ", "),
         " - the same column cannot be assigned to two rows in a valid partial state.",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
validate_no_forbidden <- function(matching, cost, frame_idx = NA_integer_) {
  loc <- if (is.na(frame_idx)) "" else sprintf(" (frame %d)", frame_idx)
  m_int <- as.integer(matching)
  matched <- which(m_int > 0L)
  if (length(matched) == 0L) return(invisible(TRUE))
  vals <- cost[cbind(matched, m_int[matched])]
  bad <- matched[!is.finite(vals)]
  if (length(bad) > 0L) {
    stop("matching", loc, " uses forbidden (NA/Inf) edges at rows ",
         paste(bad, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
matching_objective <- function(matching, cost, method, maximize = FALSE) {
  m_int <- as.integer(matching)
  matched <- which(m_int > 0L)
  if (length(matched) == 0L) return(0)
  vals <- cost[cbind(matched, m_int[matched])]
  if (identical(method, "bottleneck")) {
    # min-bottleneck: objective is the max matched edge (we shrunk the worst).
    # max-bottleneck: objective is the min matched edge (we lifted the worst).
    if (isTRUE(maximize)) min(vals) else max(vals)
  } else {
    sum(vals)
  }
}

#' @keywords internal
#' @noRd
oracle_total_cost <- function(cost, maximize, method) {
  if (identical(method, "bottleneck")) {
    # bottleneck_assignment() returns $bottleneck, not $total_cost - a pre-existing
    # API inconsistency with assignment(). We follow the existing field name here.
    res <- bottleneck_assignment(cost, maximize = maximize)
    as.numeric(res$bottleneck)
  } else {
    res <- assignment(cost, maximize = maximize, method = method)
    as.numeric(res$total_cost)
  }
}

#' Full parity check for a trace produced by a registered trace function
#'
#' Validates:
#'   - every frame's matching is a valid partial state (no duplicates,
#'     in-range, no forbidden edges)
#'   - the final-frame matching's objective matches meta$total_cost
#'   - meta$total_cost matches the C++ oracle within tolerance
#'
#' @keywords internal
#' @noRd
validate_trace_parity <- function(trace, cost, maximize, method, tol = 1e-6) {
  validate_trace(trace)

  n_rows <- trace$meta$n_rows
  n_cols <- trace$meta$n_cols

  for (i in seq_along(trace$frames)) {
    f <- trace$frames[[i]]
    validate_partial_matching(f$matching, n_rows, n_cols, frame_idx = i)
    validate_no_forbidden(f$matching, cost, frame_idx = i)
  }

  final_matching <- trace$frames[[length(trace$frames)]]$matching
  if (any(final_matching == 0L)) {
    bad <- which(final_matching == 0L)
    stop("Final frame has unassigned rows: ",
         paste(bad, collapse = ", "), call. = FALSE)
  }

  final_obj <- matching_objective(final_matching, cost, method, maximize = maximize)
  meta_total <- as.numeric(trace$meta$total_cost)
  if (abs(final_obj - meta_total) > tol * (1 + abs(meta_total))) {
    stop(sprintf(
      "Final-frame matching objective %.10g does not match meta$total_cost %.10g (tol = %g)",
      final_obj, meta_total, tol
    ), call. = FALSE)
  }

  oracle <- oracle_total_cost(cost, maximize, method)
  if (abs(meta_total - oracle) > tol * (1 + abs(oracle))) {
    stop(sprintf(
      "Trace total_cost %.10g disagrees with C++ oracle %.10g for method '%s' (tol = %g)",
      meta_total, oracle, method, tol
    ), call. = FALSE)
  }

  invisible(TRUE)
}
