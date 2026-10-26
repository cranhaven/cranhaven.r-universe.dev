# ==============================================================================
# Shared helpers for trace construction
# ==============================================================================
# Frame and meta constructors used by every R/trace_*.R file. Single source of
# truth for the on-the-wire shape consumed by inst/htmlwidgets/lap_animate.js.
#
# Per-trace state (matching vectors, duals, prices, scanned sets, ...) lives in
# the trace function's lexical scope; make_frame() is only the constructor for
# one trace frame, called from each trace's local emit().
# ==============================================================================

#' Orient a cost matrix so the internal solve has at least as many columns as
#' rows
#'
#' Traces that need n <= m solve a transposed copy and report in the caller's
#' orientation. Returns the matrix to solve on, its dimensions, and whether it
#' was transposed.
#'
#' @keywords internal
#' @noRd
trace_orient_wide <- function(cost, n_orig, m_orig) {
  transposed <- n_orig > m_orig
  cost_int <- if (transposed) t(cost) else cost
  list(transposed = transposed, cost = cost_int,
       n = nrow(cost_int), m = ncol(cost_int))
}

#' Report an internal matching in the caller's orientation
#'
#' `row_to_col` is over the internal rows. When the solve ran transposed, entry
#' i names the original column that the original row `row_to_col[i]` takes, so
#' the vector is inverted back to length `n_orig`.
#'
#' @keywords internal
#' @noRd
trace_external_matching <- function(row_to_col, n_orig, transposed) {
  out <- integer(n_orig)
  if (!transposed) {
    n <- min(length(row_to_col), n_orig)
    if (n > 0L) out[seq_len(n)] <- row_to_col[seq_len(n)]
  } else {
    for (i in seq_along(row_to_col)) {
      j <- row_to_col[i]
      if (j > 0L) out[j] <- i
    }
  }
  out
}

#' Report an internal (row, col) edge in the caller's orientation
#'
#' @keywords internal
#' @noRd
trace_ext_edge <- function(i_int, j_int, transposed) {
  if (transposed) c(j_int, i_int) else c(i_int, j_int)
}

#' Construct a single trace frame
#'
#' Returns a list with the canonical fields consumed by lap_animate's
#' JavaScript renderer. Optional fields default to NULL or empty list so the
#' caller only names what is meaningful for the current algorithm.
#'
#' @keywords internal
#' @noRd
make_frame <- function(step, phase, description, matching,
                       dual_u = NULL, dual_v = NULL,
                       active_edges = list(), path = list(),
                       progress_text = NULL) {
  frame <- list(
    step         = as.integer(step),
    phase        = phase,
    description  = description,
    matching     = matching,
    dual_u       = dual_u,
    dual_v       = dual_v,
    active_edges = active_edges,
    path         = path
  )
  # Only dual-scaling traces (e.g. gabow_tarjan) carry a per-frame progress
  # label; omit the field entirely otherwise so the wire shape is unchanged.
  if (!is.null(progress_text)) frame$progress_text <- progress_text
  frame
}

#' Construct the meta block for a trace
#'
#' Wraps the algorithm-level constants (matrix, objective, descriptive text)
#' into the canonical meta layout. Single source of truth for meta fields.
#'
#' @keywords internal
#' @noRd
make_meta <- function(algorithm, n_rows, n_cols, cost_matrix, maximize,
                      total_cost, description) {
  list(
    algorithm   = algorithm,
    n_rows      = as.integer(n_rows),
    n_cols      = as.integer(n_cols),
    cost_matrix = cost_matrix,
    maximize    = isTRUE(maximize),
    total_cost  = as.numeric(total_cost),
    description = description
  )
}

#' Sum of matched edge costs (handles partial matchings)
#'
#' Used by traces to compute the objective from a matching vector. For
#' bottleneck algorithms compute max/min directly - this helper is sum only.
#'
#' @keywords internal
#' @noRd
matching_total_cost <- function(cost, matching) {
  m_int <- as.integer(matching)
  matched <- which(m_int > 0L)
  if (length(matched) == 0L) return(0)
  sum(cost[cbind(matched, m_int[matched])], na.rm = TRUE)
}

#' Convert NA/Inf cost entries to a finite big-M with masking
#'
#' Most dual-based algorithms can't operate on Inf or NA arithmetic, so each
#' trace replaces forbidden cells with a large finite penalty for its inner
#' loop while remembering which entries were forbidden. This helper centralises
#' the dance.
#'
#' Returns a list with:
#'   cost_work    - the work copy, sign-flipped for maximize, big-M for forbidden
#'   finite_mask  - logical, TRUE where original cost was finite
#'   big_m        - the penalty value substituted for forbidden cells
#'   scale        - the largest absolute finite cost (useful for eps choice)
#'
#' @keywords internal
#' @noRd
prepare_cost_work <- function(cost, maximize = FALSE) {
  cost_work <- if (maximize) -cost else cost
  finite_mask <- is.finite(cost_work)
  if (!any(finite_mask)) {
    stop("`cost` has no finite entries.", call. = FALSE)
  }
  scale <- max(abs(cost_work[finite_mask]))
  n <- nrow(cost); m <- ncol(cost)
  big <- (scale + 1) * (n + m + 1)
  cost_work[!finite_mask] <- big
  list(
    cost_work   = cost_work,
    finite_mask = finite_mask,
    big_m       = big,
    scale       = scale
  )
}

#' Standard input validation shared by every trace_*() function
#'
#' Throws on empty / non-numeric / NaN cost. Returns the matrix coerced to
#' numeric matrix form along with n, m.
#'
#' @keywords internal
#' @noRd
validate_cost_input <- function(cost, fn_name) {
  cost <- as.matrix(cost)
  if (!is.numeric(cost)) {
    stop(sprintf("%s: `cost` must be a numeric matrix.", fn_name), call. = FALSE)
  }
  n <- nrow(cost); m <- ncol(cost)
  if (n == 0L || m == 0L) {
    stop(sprintf("%s: cost matrix must have at least one row and one column.", fn_name),
         call. = FALSE)
  }
  if (any(is.nan(cost))) {
    stop(sprintf("%s: NaN not allowed in `cost`.", fn_name), call. = FALSE)
  }
  list(cost = cost, n = n, m = m)
}

#' Validate and prepare a square cost matrix for a trace function
#'
#' Combines the empty/non-numeric/NaN check, the square (nrow == ncol)
#' requirement, and the forbidden-cell big-M substitution used by the dual-based
#' traces. Returns everything the trace body needs.
#'
#' Returns a list with: cost (numeric matrix), n, m, cost_signed (sign-flipped
#' for maximize), cost_work (big-M for forbidden cells), finite_mask, any_forbidden,
#' big_m, scale.
#'
#' @param solver_hint Method name to suggest in the rectangular-input error
#'   (e.g. "jv"); if NULL, the suggestion is omitted.
#' @keywords internal
#' @noRd
validate_square_cost <- function(cost, fn_name, maximize = FALSE,
                                 solver_hint = NULL) {
  v <- validate_cost_input(cost, fn_name)
  cost <- v$cost; n <- v$n; m <- v$m
  if (n != m) {
    stop(sprintf("%s requires a square cost matrix (nrow == ncol).", fn_name),
         if (!is.null(solver_hint))
           sprintf(" For production solving on rectangular inputs use assignment(cost, method = \"%s\") or lap_solve(cost, method = \"%s\").",
                   solver_hint, solver_hint)
         else "",
         call. = FALSE)
  }
  pw <- prepare_cost_work(cost, maximize)
  list(
    cost          = cost,
    n             = n,
    m             = m,
    cost_signed   = if (maximize) -cost else cost,
    cost_work     = pw$cost_work,
    finite_mask   = pw$finite_mask,
    any_forbidden = !all(pw$finite_mask),
    big_m         = pw$big_m,
    scale         = pw$scale
  )
}
