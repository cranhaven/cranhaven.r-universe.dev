#' Compute and Cache Distance Matrix for Reuse
#'
#' Precomputes a distance matrix between left and right datasets, allowing
#' it to be reused across multiple matching operations with different
#' constraints. This is particularly useful when exploring different matching
#' parameters (max_distance, calipers, methods) without recomputing distances.
#'
#' @param left Left dataset (data frame)
#' @param right Right dataset (data frame)
#' @param vars Character vector of variable names to use for distance computation
#' @param distance Distance metric (default: "euclidean")
#' @param weights Optional numeric vector of variable weights
#' @param scale Scaling method: FALSE, "standardize", "range", or "robust"
#' @param auto_scale Apply automatic preprocessing (default: FALSE)
#' @param left_id Name of ID column in left (default: "id")
#' @param right_id Name of ID column in right (default: "id")
#' @param block_id Optional block ID column name for blocked matching
#' @param memory_mode One of "auto" (default), "dense", or "lazy". "auto"
#'   warns (or, for a built-in distance metric, switches) when the dense
#'   matrix would consume a large fraction of free system RAM. "lazy" returns
#'   a `lazy_cost_spec` instead of a matrix (built-in distance metrics only);
#'   pass it straight to \code{match_couples()} the same way as a dense
#'   `cost_matrix`. "dense" skips the RAM check entirely.
#'
#' @return An S3 object of class "distance_object" containing:
#'   - `cost_matrix`: Numeric matrix of distances
#'   - `left_ids`: Character vector of left IDs
#'   - `right_ids`: Character vector of right IDs
#'   - `block_id`: Block ID column name (if specified)
#'   - `metadata`: List with computation details (vars, distance, scale, etc.)
#'   - `original_left`: Original left dataset (for later joining)
#'   - `original_right`: Original right dataset (for later joining)
#'
#' @details
#' This function computes distances once and stores them in a reusable object.
#' The resulting distance_object can be passed to \code{match_couples()}
#' (optionally with \code{method = "greedy"}) instead of providing datasets and
#' variables.
#'
#' Benefits:
#' - **Performance**: Avoid recomputing distances when trying different constraints
#' - **Exploration**: Quickly test max_distance, calipers, or methods
#' - **Consistency**: Ensures same distances used across comparisons
#'
#' With the default `memory_mode = "auto"`/`"dense"`, the stored `cost_matrix`
#' is a plain dense matrix; couplr has no sparse matrix representation, so
#' forbidden pairs (`Inf`) still occupy a cell rather than being dropped from
#' storage. With `memory_mode = "lazy"`, `cost_matrix` is instead a
#' `lazy_cost_spec` that recomputes distances on demand from the underlying
#' feature data.
#'
#' The distance_object stores the original datasets, allowing downstream
#' functions like \code{join_matched()} to work seamlessly.
#'
#' @examples
#' # Compute distances once
#' left <- data.frame(id = 1:5, age = c(25, 30, 35, 40, 45), income = c(45, 52, 48, 61, 55) * 1000)
#' right <- data.frame(id = 6:10, age = c(24, 29, 36, 41, 44), income = c(46, 51, 47, 60, 54) * 1000)
#'
#' dist_obj <- compute_distances(
#'   left, right,
#'   vars = c("age", "income"),
#'   scale = "standardize"
#' )
#'
#' # Reuse for different matching strategies
#' result1 <- match_couples(dist_obj, max_distance = 0.5)
#' result2 <- match_couples(dist_obj, max_distance = 1.0)
#' result3 <- match_couples(dist_obj, method = "greedy", strategy = "sorted")
#'
#' # All use the same precomputed distances
#'
#' @export
compute_distances <- function(left, right,
                              vars,
                              distance = "euclidean",
                              weights = NULL,
                              scale = FALSE,
                              auto_scale = FALSE,
                              left_id = "id",
                              right_id = "id",
                              block_id = NULL,
                              memory_mode = "auto") {

  # Validate inputs
  if (!is.data.frame(left) || !is.data.frame(right)) {
    stop("left and right must be data frames")
  }

  if (!left_id %in% names(left)) {
    stop("left_id column '", left_id, "' not found in left dataset")
  }

  if (!right_id %in% names(right)) {
    stop("right_id column '", right_id, "' not found in right dataset")
  }

  missing_vars <- setdiff(vars, intersect(names(left), names(right)))
  if (length(missing_vars) > 0) {
    stop("Variables not found in both datasets: ", paste(missing_vars, collapse = ", "))
  }

  # Apply auto-scaling if requested. preprocess_matching_vars() only *selects*
  # a scaling method and prunes unhealthy variables; the scaling itself is
  # applied by build_cost_matrix() below, so we forward the chosen method as
  # `scale` rather than pretending the data is already scaled.
  if (auto_scale) {
    preprocess_result <- preprocess_matching_vars(left, right, vars, scale_method = "auto")
    vars <- preprocess_result$vars
    auto_method <- preprocess_result$scaling_method
    scale <- if (identical(auto_method, "none")) FALSE else auto_method
  }

  # Extract IDs
  left_ids <- as.character(left[[left_id]])
  right_ids <- as.character(right[[right_id]])

  # Check for duplicates
  if (any(duplicated(left_ids))) {
    stop("Duplicate IDs found in left dataset")
  }
  if (any(duplicated(right_ids))) {
    stop("Duplicate IDs found in right dataset")
  }

  # Handle blocking
  if (!is.null(block_id)) {
    if (!block_id %in% names(left) || !block_id %in% names(right)) {
      stop("block_id column '", block_id, "' not found in both datasets")
    }

    # For blocked matching, we'll store the block info but compute full matrix
    # The actual blocking will be handled by match_couples
    message("Note: Block information stored. Blocking will be applied during matching.")
  }

  # Build cost matrix
  cost_matrix <- build_cost_matrix(left, right, vars, distance, weights, scale,
                                   memory_mode = memory_mode)

  # Store row and column names for clarity (a lazy_cost_spec is a list, not
  # a matrix -- it has no dimnames to set, and left_ids/right_ids below
  # already carry this information for both representations).
  if (!is_lazy_cost_spec(cost_matrix)) {
    rownames(cost_matrix) <- left_ids
    colnames(cost_matrix) <- right_ids
  }

  # Create distance object
  dist_obj <- structure(
    list(
      cost_matrix = cost_matrix,
      left_ids = left_ids,
      right_ids = right_ids,
      block_id = block_id,
      metadata = list(
        vars = vars,
        distance = distance,
        weights = weights,
        scale = scale,
        auto_scale = auto_scale,
        left_id = left_id,
        right_id = right_id,
        n_left = nrow(left),
        n_right = nrow(right),
        computed_at = Sys.time()
      ),
      original_left = left,
      original_right = right
    ),
    class = c("distance_object", "couplr_distance")
  )

  dist_obj
}


#' Check if Object is a Distance Object
#'
#' @param x Object to check
#' @return Logical: TRUE if x is a distance_object
#'
#' @examples
#' left <- data.frame(id = 1:3, x = c(1, 2, 3))
#' right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1))
#' dist_obj <- compute_distances(left, right, vars = "x")
#' is_distance_object(dist_obj)  # TRUE
#' is_distance_object(list())    # FALSE
#'
#' @export
is_distance_object <- function(x) {
  inherits(x, "distance_object")
}


#' Update Constraints on Distance Object
#'
#' Apply new constraints to a precomputed distance object without
#' recomputing the underlying distances. This is useful for exploring
#' different constraint scenarios quickly.
#'
#' @param dist_obj A distance_object from \code{compute_distances()}
#' @param max_distance Maximum allowed distance (pairs with distance > max_distance become Inf)
#' @param calipers Named list of per-variable calipers
#'
#' @return A new distance_object with updated cost_matrix
#'
#' @details
#' This function creates a new distance_object with modified constraints
#' applied to the cost matrix. The original distance_object is not modified.
#'
#' Constraints:
#' - \code{max_distance}: Sets cost to Inf for pairs exceeding this threshold
#' - \code{calipers}: Per-variable restrictions (e.g., calipers = list(age = 5))
#'
#' The function returns a new object rather than modifying in place,
#' following R's copy-on-modify semantics.
#'
#' @examples
#' left <- data.frame(id = 1:5, age = c(25, 30, 35, 40, 45))
#' right <- data.frame(id = 6:10, age = c(24, 29, 36, 41, 44))
#' dist_obj <- compute_distances(left, right, vars = "age")
#'
#' # Apply constraints
#' constrained <- update_constraints(dist_obj, max_distance = 2)
#' result <- match_couples(constrained)
#'
#' @export
update_constraints <- function(dist_obj, max_distance = Inf, calipers = NULL) {

  if (!is_distance_object(dist_obj)) {
    stop("dist_obj must be a distance_object from compute_distances()")
  }

  # Start with a copy of the original cost matrix. Delegate to
  # apply_max_distance()/apply_calipers() (rather than masking `new_cost`
  # inline) so this works identically whether cost_matrix is a dense matrix
  # or a lazy_cost_spec -- both already dispatch correctly on the object's
  # class.
  new_cost <- apply_max_distance(dist_obj$cost_matrix, max_distance)

  if (!is.null(calipers)) {
    new_cost <- apply_calipers(
      new_cost,
      dist_obj$original_left,
      dist_obj$original_right,
      calipers,
      dist_obj$metadata$vars
    )
  }

  # Create new distance object with updated matrix
  new_dist_obj <- dist_obj
  new_dist_obj$cost_matrix <- new_cost
  new_dist_obj$metadata$constraints_applied <- list(
    max_distance = max_distance,
    calipers = calipers,
    updated_at = Sys.time()
  )

  new_dist_obj
}


#' Print Method for Distance Objects
#'
#' @param x A distance_object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input object `x`.
#' @export
print.distance_object <- function(x, ...) {
  cat("Distance Object\n")
  cat("===============\n\n")

  cat("Dimensions:\n")
  cat("  Left units:  ", x$metadata$n_left, "\n")
  cat("  Right units: ", x$metadata$n_right, "\n")
  cat("  Matrix size: ", nrow(x$cost_matrix), "x", ncol(x$cost_matrix), "\n\n")

  cat("Computation:\n")
  cat("  Variables:   ", paste(x$metadata$vars, collapse = ", "), "\n")
  cat("  Distance:    ", x$metadata$distance, "\n")
  cat("  Scaling:     ", if (identical(x$metadata$scale, FALSE)) "none" else x$metadata$scale, "\n")
  if (!is.null(x$metadata$weights)) {
    cat("  Weights:     ", paste(round(x$metadata$weights, 3), collapse = ", "), "\n")
  }
  if (!is.null(x$block_id)) {
    cat("  Blocking:    ", x$block_id, "\n")
  }
  cat("  Computed:    ", format(x$metadata$computed_at, "%Y-%m-%d %H:%M:%S"), "\n\n")

  # Distance statistics
  if (is_lazy_cost_spec(x$cost_matrix)) {
    # A full pass over every pair to report min/median/mean/max is exactly
    # the O(n*m) cost lazy mode exists to avoid; a streaming (single-pass)
    # equivalent is a documented future addition, not implemented yet.
    cat("Distance Summary:\n")
    cat("  (memory_mode = \"lazy\": distance statistics not computed;",
        "would require a full O(n*m) pass)\n")
  } else {
    finite_dists <- x$cost_matrix[is.finite(x$cost_matrix)]
    if (length(finite_dists) > 0) {
      cat("Distance Summary:\n")
      cat("  Valid pairs: ", length(finite_dists), " (",
          round(100 * length(finite_dists) / length(x$cost_matrix), 1), "%)\n", sep = "")
      cat("  Min:         ", round(min(finite_dists), 3), "\n")
      cat("  Median:      ", round(median(finite_dists), 3), "\n")
      cat("  Mean:        ", round(mean(finite_dists), 3), "\n")
      cat("  Max:         ", round(max(finite_dists), 3), "\n")
    } else {
      cat("Distance Summary:\n")
      cat("  No valid pairs (all Inf)\n")
    }
  }

  if (!is.null(x$metadata$constraints_applied)) {
    cat("\nConstraints Applied:\n")
    if (!is.infinite(x$metadata$constraints_applied$max_distance)) {
      cat("  Max distance: ", x$metadata$constraints_applied$max_distance, "\n")
    }
    if (!is.null(x$metadata$constraints_applied$calipers)) {
      cat("  Calipers:     ", paste(names(x$metadata$constraints_applied$calipers),
                                    "<=", x$metadata$constraints_applied$calipers, collapse = ", "), "\n")
    }
  }

  cat("\nUse with:\n")
  cat("  - match_couples(dist_obj, ...)\n")
  cat("  - match_couples(dist_obj, method = 'greedy', ...)\n")
  cat("  - update_constraints(dist_obj, ...)\n")

  invisible(x)
}


#' Summary Method for Distance Objects
#'
#' @param object A distance_object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input object.
#' @export
summary.distance_object <- function(object, ...) {
  cat("Distance Object Summary\n")
  cat("=======================\n\n")

  print(object)

  # Additional statistics
  cat("\nDetailed Statistics:\n")

  if (is_lazy_cost_spec(object$cost_matrix)) {
    # Quantiles, standard deviation, skewness, and the forbidden-pair count
    # below all require a full O(n*m) pass -- not implemented yet for a lazy
    # cost source (a streaming, single-pass equivalent is a documented
    # future addition). Skip rather than materialize the matrix these
    # statistics exist to avoid materializing in the first place.
    cat("  (memory_mode = \"lazy\": detailed statistics not computed;",
        "would require a full O(n*m) pass)\n")
    return(invisible(object))
  }

  finite_dists <- object$cost_matrix[is.finite(object$cost_matrix)]

  if (length(finite_dists) > 0) {
    cat("  Quantiles:\n")
    probs <- c(0.05, 0.25, 0.50, 0.75, 0.95)
    q <- quantile(finite_dists, probs = probs)
    for (i in seq_along(q)) {
      cat(sprintf("    %3.0f%%: %8.3f\n", probs[i] * 100, q[i]))
    }

    cat("\n  Standard deviation: ", round(sd(finite_dists), 3), "\n")

    # Check for skewness
    if (requireNamespace("e1071", quietly = TRUE)) {
      skew <- e1071::skewness(finite_dists)
      cat("  Skewness:           ", round(skew, 3))
      if (abs(skew) > 2) {
        cat(" (highly skewed)")
      }
      cat("\n")
    }
  }

  # Sparsity analysis
  n_inf <- sum(is.infinite(object$cost_matrix))
  pct_inf <- 100 * n_inf / length(object$cost_matrix)

  cat("\nSparsity:\n")
  cat("  Forbidden pairs: ", n_inf, " (", round(pct_inf, 1), "%)\n", sep = "")

  if (pct_inf > 50) {
    cat("  Note: >50% forbidden. method = \"lapmod\" and \"sap\" store only the\n")
    cat("        finite pairs; \"jv\" matched their times on the package's\n")
    cat("        sparse benchmarks, so method = \"auto\" does not divert on it\n")
  }

  invisible(object)
}
