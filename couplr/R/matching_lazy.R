# ==============================================================================
# Matching Lazy - lazy_cost_spec S3 class for memory_mode = "lazy"/"implicit"
# ==============================================================================
# When build_cost_matrix() resolves to lazy mode, it returns a lazy_cost_spec
# instead of a dense matrix: a lightweight list holding the (already scaled/
# weighted) feature matrices and enough metadata to build a
# lap::LazyCostMatrix in C++ on demand, without ever materializing an
# n_left x n_right matrix in R.
#
# The same object states the complete implicit problem the edge-generation loop
# solves, so `mode` records which of the two the caller asked for and travels
# with the spec to the solve. Threading it as a separate argument instead would
# mean every function between build_cost_matrix() and assignment() carrying a
# field of the object it already holds.

#' Construct a lazy cost specification
#'
#' `mode` is the memory mode that resolved to this specification: `"lazy"`,
#' solved over every pair, or `"implicit"`, solved by generating the pairs the
#' answer turns out to need.
#'
#' @return An object of class "lazy_cost_spec".
#' @keywords internal
new_lazy_cost_spec <- function(left_mat, right_mat, distance, sigma, weights, vars,
                               mode = c("lazy", "implicit")) {
  mode <- match.arg(mode)
  structure(
    list(
      left_mat = left_mat,
      right_mat = right_mat,
      distance = if (is.function(distance)) distance else tolower(as.character(distance)[1]),
      sigma = sigma,
      weights = weights,
      vars = vars,
      n_left = nrow(left_mat),
      n_right = nrow(right_mat),
      max_distance = Inf,
      calipers = list(),
      mode = mode
    ),
    class = "lazy_cost_spec"
  )
}

#' @keywords internal
is_lazy_cost_spec <- function(x) inherits(x, "lazy_cost_spec")

#' The memory mode a lazy cost specification was built for
#'
#' A spec built before `mode` existed, or by hand, is a lazy one: solving every
#' pair is what the class has always meant.
#'
#' @return "lazy" or "implicit".
#' @keywords internal
lazy_cost_spec_mode <- function(spec) {
  mode <- spec$mode
  if (is.null(mode)) "lazy" else mode
}

#' @export
dim.lazy_cost_spec <- function(x) c(x$n_left, x$n_right)

#' Swap left/right in a lazy cost spec
#'
#' A cheap metadata field-swap (left_mat <-> right_mat, n_left <-> n_right),
#' unlike the dense path's `t()` matrix copy. max_distance is unaffected, and
#' each caliper's raw values swap sides with the units they belong to.
#'
#' @keywords internal
transpose_lazy_cost_spec <- function(spec) {
  transposed <- spec
  transposed$left_mat <- spec$right_mat
  transposed$right_mat <- spec$left_mat
  transposed$n_left <- spec$n_right
  transposed$n_right <- spec$n_left
  transposed$calipers <- lapply(spec$calipers, function(cal) {
    cal[c("left", "right")] <- cal[c("right", "left")]
    cal
  })
  # A user's function is called as f(left, right) and need not be symmetric, so
  # the transposed problem asks it the original question and turns the answer.
  if (is.function(spec$distance)) {
    f <- spec$distance
    transposed$distance <- function(l, r) t(as.matrix(f(r, l)))
  }
  transposed
}

#' Compute paired (not cross) distances for specific matched pairs
#'
#' Given matched row/column index pairs (as produced by a solve), reports each
#' pair's distance. The evaluation is the solver's own: the same C++ routine the
#' lazy path priced the pair with is called on that pair, rather than the
#' formula being written a second time here. Two implementations of one metric
#' agree to rounding and not to the last bit, and the difference is visible
#' where it matters most -- a caliper set at a distance the package reported can
#' exclude the pair it was read from.
#'
#' This is cheap regardless of n_left/n_right: the number of matched pairs never
#' exceeds min(n_left, n_right), so it never approaches the O(n*m) cost the lazy
#' path exists to avoid.
#'
#' @return Numeric vector of length length(matched_rows).
#' @keywords internal
lazy_pair_distances <- function(spec, matched_rows, matched_cols) {
  d <- cpp_lazy_pair_distances(
    left_mat = spec$left_mat,
    right_mat = spec$right_mat,
    metric = spec$distance,
    inv_cov = lazy_cost_spec_inv_cov(spec),
    rows = as.integer(matched_rows),
    cols = as.integer(matched_cols)
  )
  if (is.function(spec$distance)) {
    .lazy_distance_consistent(spec, matched_rows, matched_cols, d)
  }
  d
}

# A user's distance function is read one block of left rows at a time against
# every right unit, and every answer the lazy and implicit paths give, their
# certificates included, assumes the distance of a pair does not depend on which
# other units shared its call. That is the function's contract rather than
# something a solve can prove, but a function that breaks it shows on the pairs
# a solve chose: each is evaluated again here alone, a call holding one left and
# one right unit and nothing else, and compared with the distance the solve
# read. Up to `limit` pairs are checked, spread evenly over the matching.
.lazy_distance_consistent <- function(spec, rows, cols, solved, limit = 2000L) {
  k <- length(rows)
  if (!k) {
    return(invisible(TRUE))
  }
  checked <- if (k <= limit) seq_len(k) else unique(round(seq(1, k, length.out = limit)))
  for (t in checked) {
    again <- as.matrix(spec$distance(spec$left_mat[rows[t], , drop = FALSE],
                                     spec$right_mat[cols[t], , drop = FALSE]))[1L, 1L]
    was <- solved[t]
    same <- if (is.finite(was) && is.finite(again)) {
      abs(again - was) <= 1e-9 * max(1, abs(was))
    } else {
      identical(is.finite(was), is.finite(again))
    }
    if (!same) {
      stop("The distance function returned ", format(was, digits = 12),
           " for left unit ", rows[t], " and right unit ", cols[t],
           " in a call on a block of units and ", format(again, digits = 12),
           " in a call on that pair alone. The lazy and implicit paths call it ",
           "on blocks of units, so a pair's distance must not depend on the ",
           "other units in the call. Use memory_mode = \"dense\" for a function ",
           "that needs every unit at once.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Precompute the Mahalanobis inverse covariance matrix for a lazy cost spec
#'
#' Mirrors compute_distance_matrix()'s pooled within-group covariance logic
#' exactly (R/matching_distance.R) -- computed once in R rather than
#' reimplemented in C++, so the two code paths can't drift apart. A spec whose
#' rows were reshaped carries the matrix its units defined as `inv_cov`, and
#' that is returned as it is.
#'
#' @return p x p inverse covariance matrix, or NULL if distance != "mahalanobis".
#' @keywords internal
lazy_cost_spec_inv_cov <- function(spec) {
  if (!identical(spec$distance, "mahalanobis") && !identical(spec$distance, "maha")) {
    return(NULL)
  }
  if (!is.null(spec$inv_cov)) {
    return(spec$inv_cov)
  }
  n_left <- spec$n_left
  n_right <- spec$n_right
  if (!is.null(spec$sigma)) {
    cov_mat <- spec$sigma
  } else if (n_left >= 2L && n_right >= 2L) {
    S_L <- stats::cov(spec$left_mat)
    S_R <- stats::cov(spec$right_mat)
    cov_mat <- ((n_left - 1L) * S_L + (n_right - 1L) * S_R) / (n_left + n_right - 2L)
  } else {
    cov_mat <- stats::cov(rbind(spec$left_mat, spec$right_mat))
  }
  inv_cov <- tryCatch(solve(cov_mat), error = function(e) NULL)
  if (is.null(inv_cov) || rcond(cov_mat) < .Machine$double.eps) {
    stop("Covariance matrix is singular or near-singular; cannot compute Mahalanobis distance. ",
         "Consider removing collinear variables or supplying a regularized sigma.",
         call. = FALSE)
  }
  inv_cov
}

#' Calipers of a lazy cost spec, as the C++ cost source reads them
#'
#' One `list(threshold, left, right)` per caliper, named by its variable:
#' the threshold and the variable's raw values for every left and right unit,
#' in the row order of `left_mat` and `right_mat`.
#'
#' @return Named list of caliper records.
#' @keywords internal
lazy_cost_spec_calipers <- function(spec) {
  stats::setNames(
    lapply(spec$calipers, function(cal) cal[c("threshold", "left", "right")]),
    vapply(spec$calipers, function(cal) cal$var, character(1))
  )
}

#' Rows of a lazy cost spec, by unit index on each side
#'
#' Reads the feature rows and every caliper's raw values through the same
#' index, so the two stay aligned when a design replicates or reorders units.
#'
#' @keywords internal
lazy_cost_spec_rows <- function(spec, rows, cols) {
  out <- spec
  out$left_mat <- spec$left_mat[rows, , drop = FALSE]
  out$right_mat <- spec$right_mat[cols, , drop = FALSE]
  out$n_left <- nrow(out$left_mat)
  out$n_right <- nrow(out$right_mat)
  out$calipers <- lapply(spec$calipers, function(cal) {
    cal$left <- cal$left[rows]
    cal$right <- cal$right[cols]
    cal
  })
  out
}
