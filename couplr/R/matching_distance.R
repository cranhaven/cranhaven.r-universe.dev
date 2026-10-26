# ==============================================================================
# Matching Distance - Distance computation and scaling
# ==============================================================================

# Metrics that reduce over dimensions one at a time: each dimension's
# n_left x n_right table of absolute differences comes from outer(), and
# `combine` folds it into the running total (`+` for Manhattan, pmax for
# Chebyshev, `acc + d^2` for squared Euclidean).
.per_dim_reduce <- function(left_mat, right_mat, combine) {
  acc <- matrix(0, nrow = nrow(left_mat), ncol = nrow(right_mat))
  for (k in seq_len(ncol(left_mat))) {
    acc <- combine(acc, abs(outer(left_mat[, k], right_mat[, k], `-`)))
  }
  acc
}

# Summing (L_ik - R_jk)^2 over dimensions holds every digit the coordinates
# carry. The Gram-matrix identity ||L||^2 + ||R||^2 - 2 L.R would fold the same
# sum into one BLAS call, but subtracting two large nearly equal terms costs
# most of the mantissa whenever the coordinates are large next to the distance
# between them.
.squared_euclidean <- function(left_mat, right_mat) {
  .per_dim_reduce(left_mat, right_mat, function(acc, d) acc + d^2)
}

#' Compute pairwise distance matrix
#'
#' @param left_mat Numeric matrix of left units (rows = units, cols = variables).
#' @param right_mat Numeric matrix of right units (rows = units, cols = variables).
#' @param distance Character string specifying distance metric, or a function.
#' @param sigma Optional covariance matrix for Mahalanobis distance. If `NULL`
#'   (default), the pooled within-group covariance is estimated from `left_mat`
#'   and `right_mat`: \eqn{\Sigma = ((n_L-1)\,S_L + (n_R-1)\,S_R) / (n_L+n_R-2)}.
#'   This matches the convention used by `optmatch::match_on()` for
#'   treated/control Mahalanobis matching. If either group has fewer than two
#'   rows, falls back to the overall-sample covariance of the two groups
#'   stacked together.
#'
#' @return Numeric matrix of pairwise distances (n_left x n_right).
#' @keywords internal
compute_distance_matrix <- function(left_mat, right_mat, distance = "euclidean",
                                    sigma = NULL) {
  n_left <- nrow(left_mat)
  n_right <- nrow(right_mat)
  n_vars <- ncol(left_mat)

  # Validate dimensions
  if (ncol(right_mat) != n_vars) {
    stop("left_mat and right_mat must have same number of columns", call. = FALSE)
  }

  # Handle distance specification
  if (is.function(distance)) {
    # User-provided distance function
    # Assume it takes two matrices and returns a distance matrix
    return(distance(left_mat, right_mat))
  }

  # Built-in distance metrics
  distance <- tolower(as.character(distance)[1])

  dist_matrix <- matrix(0, nrow = n_left, ncol = n_right)

  if (distance %in% c("euclidean", "l2")) {
    dist_matrix <- sqrt(.squared_euclidean(left_mat, right_mat))
  } else if (distance %in% c("manhattan", "l1", "cityblock")) {
    dist_matrix <- .per_dim_reduce(left_mat, right_mat, `+`)
  } else if (distance %in% c("squared_euclidean", "sqeuclidean", "sq")) {
    dist_matrix <- .squared_euclidean(left_mat, right_mat)
  } else if (distance %in% c("chebyshev", "chebychev", "maximum", "max")) {
    dist_matrix <- .per_dim_reduce(left_mat, right_mat, pmax)
  } else if (distance %in% c("mahalanobis", "maha")) {
    # Mahalanobis distance: sqrt((x-y)' * Sigma^-1 * (x-y))
    if (!is.null(sigma)) {
      # User-supplied covariance matrix
      if (!is.matrix(sigma) || nrow(sigma) != n_vars || ncol(sigma) != n_vars) {
        stop(sprintf("sigma must be a %d x %d matrix matching the number of variables",
                     n_vars, n_vars), call. = FALSE)
      }
      cov_mat <- sigma
    } else {
      # Pooled within-group covariance — standard convention for Mahalanobis
      # matching with two groups (e.g., treated/control). Inflating Sigma by
      # between-group differences (i.e. using cov(rbind(L, R))) would
      # under-weight matching on variables where the two groups differ most.
      # Falls back to overall covariance when a group is too small for cov().
      if (n_left >= 2L && n_right >= 2L) {
        S_L <- stats::cov(left_mat)
        S_R <- stats::cov(right_mat)
        cov_mat <- ((n_left - 1L) * S_L + (n_right - 1L) * S_R) /
                   (n_left + n_right - 2L)
      } else {
        cov_mat <- stats::cov(rbind(left_mat, right_mat))
      }
    }

    # Robust singularity check using rcond (not det == 0)
    inv_cov <- tryCatch(
      solve(cov_mat),
      error = function(e) NULL
    )
    if (is.null(inv_cov) || rcond(cov_mat) < .Machine$double.eps) {
      stop("Covariance matrix is singular or near-singular; cannot compute Mahalanobis distance. ",
           "Consider removing collinear variables or supplying a regularized sigma.",
           call. = FALSE)
    }

    # Vectorized computation: for each left row, compute distances to all right rows
    # D_ij = sqrt( (L_i - R_j) %*% inv_cov %*% (L_i - R_j)' )
    for (i in seq_len(n_left)) {
      diff_mat <- sweep(right_mat, 2, left_mat[i, ])  # n_right x n_vars
      dist_matrix[i, ] <- sqrt(pmax(rowSums(diff_mat * (diff_mat %*% inv_cov)), 0))
    }
  } else {
    stop(sprintf("Unknown distance metric: %s", distance), call. = FALSE)
  }

  dist_matrix
}

#' Apply scaling to matching variables
#'
#' @return List with scaled left/right matrices and scaling parameters.
#' @keywords internal
apply_scaling <- function(left_mat, right_mat, method = "standardize") {
  if (method == FALSE || method == "none" || is.null(method)) {
    return(list(left = left_mat, right = right_mat, params = NULL))
  }

  # Compute scaling parameters from combined data
  combined <- rbind(left_mat, right_mat)

  if (method == TRUE || method == "standardize" || method == "scale") {
    # Standardize to mean 0, sd 1
    means <- colMeans(combined)
    sds <- apply(combined, 2, stats::sd)

    # Avoid division by zero
    sds[sds == 0] <- 1

    left_scaled <- scale(left_mat, center = means, scale = sds)
    right_scaled <- scale(right_mat, center = means, scale = sds)

    params <- list(method = "standardize", means = means, sds = sds)
  } else if (method == "range" || method == "minmax") {
    # Scale to [0, 1] based on combined range
    mins <- apply(combined, 2, min)
    maxs <- apply(combined, 2, max)
    ranges <- maxs - mins

    # Avoid division by zero
    ranges[ranges == 0] <- 1

    left_scaled <- sweep(sweep(left_mat, 2, mins, "-"), 2, ranges, "/")
    right_scaled <- sweep(sweep(right_mat, 2, mins, "-"), 2, ranges, "/")

    params <- list(method = "range", mins = mins, maxs = maxs, ranges = ranges)
  } else if (method == "robust") {
    # Robust scaling using median and MAD (median absolute deviation)
    medians <- apply(combined, 2, stats::median)
    mads <- apply(combined, 2, stats::mad)

    # Avoid division by zero
    mads[mads == 0] <- 1

    left_scaled <- sweep(sweep(left_mat, 2, medians, "-"), 2, mads, "/")
    right_scaled <- sweep(sweep(right_mat, 2, medians, "-"), 2, mads, "/")

    params <- list(method = "robust", medians = medians, mads = mads)
  } else {
    stop(sprintf("Unknown scaling method: %s", method), call. = FALSE)
  }

  # Remove attributes added by scale()
  left_scaled <- as.matrix(left_scaled)
  right_scaled <- as.matrix(right_scaled)
  attr(left_scaled, "scaled:center") <- NULL
  attr(left_scaled, "scaled:scale") <- NULL
  attr(right_scaled, "scaled:center") <- NULL
  attr(right_scaled, "scaled:scale") <- NULL

  list(left = left_scaled, right = right_scaled, params = params)
}

#' Apply weights to matching variables
#'
#' @return Numeric matrix with columns weighted.
#' @keywords internal
apply_weights <- function(mat, weights) {
  if (is.null(weights) || all(weights == 1)) {
    return(mat)
  }

  if (length(weights) != ncol(mat)) {
    stop("Length of weights must match number of columns in matrix", call. = FALSE)
  }

  # Apply weights by scaling columns
  # For distance calculation, we multiply each variable by sqrt(weight)
  # so that squared differences are weighted correctly
  sweep(mat, 2, sqrt(weights), "*")
}

#' Build cost matrix for matching
#'
#' This is the main entry point for distance computation.
#'
#' @param memory_mode One of "auto" (default), "dense", "lazy" or "implicit".
#'   "auto" warns (or, when the caller supports it, switches) when the dense
#'   matrix would consume a large fraction of free system RAM. `memory_mode =
#'   "lazy"` returns a `lazy_cost_spec` instead of a matrix when the calling
#'   path and distance metric support it (built-in metrics via `assignment()`
#'   with `method = "jv"`/`"auction"`); otherwise it errors clearly rather
#'   than silently falling back to dense. `memory_mode = "implicit"` returns
#'   the same specification marked for the edge-generation loop, which solves
#'   it without building the pair set at all.
#' @param caller_supports_lazy Whether the calling code path can actually
#'   consume a `lazy_cost_spec` result for a solve over every pair. Defaults
#'   to `TRUE`; a caller with no such solve (e.g. `full_match()`, whose flow
#'   over every pair would hold every pair) passes `FALSE` so
#'   `memory_mode = "auto"` never promotes to lazy for it, and an explicit
#'   `memory_mode = "lazy"` request errors clearly instead of returning a
#'   `lazy_cost_spec` the caller cannot use.
#' @param caller_supports_implicit Whether the calling path's design is one the
#'   edge-generation loop solves. Defaults to whatever the caller says about
#'   lazy, since the loop reads the same specification.
#' @return Numeric matrix of distances with optional scaling/weights applied.
#' @keywords internal
build_cost_matrix <- function(left, right, vars, distance = "euclidean",
                               weights = NULL, scale = FALSE, sigma = NULL,
                               memory_mode = "auto", caller_supports_lazy = TRUE,
                               caller_supports_implicit = caller_supports_lazy) {
  # Extract variable matrices
  left_mat <- extract_matching_vars(left, vars)
  right_mat <- extract_matching_vars(right, vars)

  # A custom distance FUNCTION reaches the lazy and implicit paths through a
  # cost source that calls it on a block of left rows against every right unit
  # and keeps a few of those blocks, which is the contract the dense path
  # already calls it under, with the R call paid per block rather than per pair.
  resolved <- resolve_memory_mode(
    nrow(left_mat), nrow(right_mat), memory_mode,
    solver_supports_lazy = caller_supports_lazy,
    solver_supports_implicit = caller_supports_implicit
  )

  # Validate and normalize weights
  weights <- validate_weights(weights, vars)

  # Apply scaling if requested
  if (!identical(scale, FALSE)) {
    scaled <- apply_scaling(left_mat, right_mat, method = scale)
    left_mat <- scaled$left
    right_mat <- scaled$right
    scaling_params <- scaled$params
  } else {
    scaling_params <- NULL
  }

  # Apply weights
  left_mat <- apply_weights(left_mat, weights)
  right_mat <- apply_weights(right_mat, weights)

  if (resolved %in% c("lazy", "implicit")) {
    return(new_lazy_cost_spec(left_mat, right_mat, distance, sigma, weights, vars,
                              mode = resolved))
  }

  # Compute distance matrix
  dist_matrix <- compute_distance_matrix(left_mat, right_mat, distance,
                                         sigma = sigma)

  # Add metadata as attributes
  attr(dist_matrix, "distance") <- distance
  attr(dist_matrix, "weights") <- weights
  attr(dist_matrix, "scaling") <- scaling_params
  attr(dist_matrix, "vars") <- vars

  dist_matrix
}
