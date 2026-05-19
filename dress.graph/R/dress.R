# ========================================================================
#  dress — R interface to the DRESS on Graphs library
# ========================================================================

# ---- Variant constants ------------------------------------------------

#' Graph variant constants.
#'
#' Determine how adjacency lists are constructed from the input edge list.
#' @export
DRESS_UNDIRECTED <- 0L

#' @rdname DRESS_UNDIRECTED
#' @export
DRESS_DIRECTED   <- 1L

#' @rdname DRESS_UNDIRECTED
#' @export
DRESS_FORWARD    <- 2L

#' @rdname DRESS_UNDIRECTED
#' @export
DRESS_BACKWARD   <- 3L

# ---- Main entry point -------------------------------------------------

#' Compute DRESS edge similarity
#'
#' Build a DRESS graph from an edge list and run iterative fitting.
#' Returns per-edge similarity values along with per-vertex norms.
#'
#' @param n_vertices Integer. Number of vertices (vertex ids must be in
#'   \code{0 .. n_vertices - 1}).
#' @param sources Integer vector of length E — edge source endpoints (0-based).
#' @param targets Integer vector of length E — edge target endpoints (0-based).
#' @param weights Optional numeric vector of length E — per-edge weights.
#'   \code{NULL} (default) gives every edge weight 1.
#' @param vertex_weights Optional numeric vector of length N — per-vertex weights.
#'   \code{NULL} (default) gives every vertex weight 1.
#' @param variant Graph variant (default \code{DRESS_UNDIRECTED}).
#'   One of \code{DRESS_UNDIRECTED} (0), \code{DRESS_DIRECTED} (1),
#'   \code{DRESS_FORWARD} (2), \code{DRESS_BACKWARD} (3).
#' @param max_iterations Maximum number of fitting iterations (default 100).
#' @param epsilon Convergence threshold — stop when the max per-edge
#'   change falls below this value (default 1e-6).
#' @param precompute_intercepts Logical. Pre-compute common-neighbor index
#'   for faster iteration at the cost of more memory (default \code{FALSE}).
#'
#' @return A list with components:
#' \describe{
#'   \item{\code{sources}}{Integer vector [E] — edge source endpoints (0-based).}
#'   \item{\code{targets}}{Integer vector [E] — edge target endpoints (0-based).}
#'   \item{\code{edge_dress}}{Numeric vector [E] — fitted DRESS values.}
#'   \item{\code{edge_weight}}{Numeric vector [E] — variant-adjusted edge weights.}
#'   \item{\code{vertex_dress}}{Numeric vector [N] — aggregated vertex dress norms.}
#'   \item{\code{iterations}}{Integer — iterations until convergence.}
#'   \item{\code{delta}}{Numeric — final maximum change.}
#'   \item{\code{vertex_weights}}{Numeric vector [N] — vertex weights (if provided).}
#' }
#'
#' @examples
#' # Triangle + pendant: 0-1, 1-2, 2-0, 2-3
#' res <- fit(4L, c(0L,1L,2L,2L), c(1L,2L,0L,3L))
#' res$edge_dress
#'
#' # Weighted, directed variant
#' res2 <- fit(4L, c(0L,1L,2L,2L), c(1L,2L,0L,3L),
#'                   weights = c(1.0, 2.0, 1.0, 0.5),
#'                   variant = DRESS_DIRECTED)
#'
#' @useDynLib dress.graph, .registration = TRUE
#' @export
fit <- function(n_vertices,
                      sources,
                      targets,
                      weights              = NULL,
                      vertex_weights         = NULL,
                      variant              = DRESS_UNDIRECTED,
                      max_iterations       = 100L,
                      epsilon              = 1e-6,
                      precompute_intercepts = FALSE) {

  # ---- input validation ----
  n_vertices     <- as.integer(n_vertices)
  sources        <- as.integer(sources)
  targets        <- as.integer(targets)
  variant        <- as.integer(variant)
  max_iterations <- as.integer(max_iterations)
  epsilon        <- as.double(epsilon)
  precompute     <- as.integer(precompute_intercepts)

  stopifnot(length(sources) == length(targets))
  stopifnot(n_vertices >= 1L)
  stopifnot(variant >= 0L && variant <= 3L)
  stopifnot(max_iterations >= 1L)
  stopifnot(epsilon > 0)

  if (!is.null(weights)) {
    weights <- as.double(weights)
    stopifnot(length(weights) == length(sources))
  }

  if (!is.null(vertex_weights)) {
    vertex_weights <- as.double(vertex_weights)
    stopifnot(length(vertex_weights) == n_vertices)
  }

  .Call(C_dress_fit,
        n_vertices,
        sources,
        targets,
        weights,
        vertex_weights,
        variant,
        max_iterations,
        epsilon,
        precompute)
}

# ---- Delta-k-DRESS ---------------------------------------------------

#' Compute Delta-k-DRESS histogram
#'
#' Build a DRESS graph from an edge list and compute the Delta-k-DRESS
#' distribution by exhaustively removing all k-vertex subsets and
#' measuring the change in edge similarity values.
#'
#' @param n_vertices Integer. Number of vertices (vertex ids in 0..N-1).
#' @param sources Integer vector of length E — edge source endpoints (0-based).
#' @param targets Integer vector of length E — edge target endpoints (0-based).
#' @param weights Numeric vector of length E — per-edge weights, or NULL for
#'   unweighted (all weights = 1). Default NULL.
#' @param vertex_weights Numeric vector of length N — per-vertex weights, or NULL
#'   (all weights = 1). Default NULL.
#' @param k Integer. Number of vertices to remove (0 = original graph).
#' @param variant Graph variant (default \code{DRESS_UNDIRECTED}).
#' @param max_iterations Maximum fitting iterations (default 100).
#' @param epsilon Convergence threshold (default 1e-6).
#' @param precompute Logical. Pre-compute intercept index (default FALSE).
#' @param keep_multisets Logical. If TRUE, return per-subgraph edge DRESS
#'   values in a C(N,k) x E matrix (NaN for removed edges). Default FALSE.
#'
#' @return A list with components:
#' \describe{
#'   \item{\code{histogram}}{Data frame with columns \code{value} and \code{count},
#'     containing the exact sparse histogram entries sorted by \code{value}.}
#'   \item{\code{multisets}}{Matrix (C(N,k) x E) of per-subgraph edge values
#'     (only present when \code{keep_multisets = TRUE}; NaN = removed edge).}
#'   \item{\code{num_subgraphs}}{Integer — C(N,k) (only when \code{keep_multisets = TRUE}).}
#' }
#'
#' @examples
#' # Triangle K3, delta-1
#' res <- delta_fit(3L, c(0L,1L,2L), c(1L,2L,0L), k = 1L)
#' res$histogram
#'
#' @useDynLib dress.graph, .registration = TRUE
#' @export
delta_fit <- function(n_vertices,
                            sources,
                            targets,
                            weights          = NULL,
                            vertex_weights     = NULL,
                            k                = 0L,
                            variant          = DRESS_UNDIRECTED,
                            max_iterations   = 100L,
                            epsilon          = 1e-6,
                            n_samples        = 0L,
                            seed             = 0L,
                            precompute       = FALSE,
                            keep_multisets   = FALSE,
                            compute_histogram = TRUE) {

  n_vertices     <- as.integer(n_vertices)
  sources        <- as.integer(sources)
  targets        <- as.integer(targets)
  if (!is.null(weights)) weights <- as.double(weights)
  if (!is.null(vertex_weights)) vertex_weights <- as.double(vertex_weights)
  k              <- as.integer(k)
  variant        <- as.integer(variant)
  max_iterations <- as.integer(max_iterations)
  epsilon        <- as.double(epsilon)
  n_samples      <- as.integer(n_samples)
  seed           <- as.integer(seed)
  precompute     <- as.integer(precompute)
  keep_multisets <- as.integer(keep_multisets)
  compute_histogram <- as.integer(compute_histogram)

  stopifnot(length(sources) == length(targets))
  if (!is.null(weights)) stopifnot(length(weights) == length(sources))
  if (!is.null(vertex_weights)) stopifnot(length(vertex_weights) == n_vertices)
  stopifnot(n_vertices >= 1L)
  stopifnot(k >= 0L)
  stopifnot(variant >= 0L && variant <= 3L)
  stopifnot(max_iterations >= 1L)
  stopifnot(epsilon > 0)

  .Call(C_delta_dress_fit,
        n_vertices,
        sources,
        targets,
        weights,
        vertex_weights,
        k,
        variant,
        max_iterations,
        epsilon,
        n_samples,
        seed,
        precompute,
        keep_multisets,
        compute_histogram)
}

# ---- Nabla-k-DRESS ---------------------------------------------------

#' Compute Nabla-k-DRESS histogram
#'
#' Build a DRESS graph from an edge list and compute the Nabla-k-DRESS
#' distribution by sampling vertex k-tuples and measuring the change
#' in edge similarity values.
#'
#' @param n_vertices Integer. Number of vertices (vertex ids in 0..N-1).
#' @param sources Integer vector of length E — edge source endpoints (0-based).
#' @param targets Integer vector of length E — edge target endpoints (0-based).
#' @param weights Numeric vector of length E — per-edge weights, or NULL for
#'   unweighted (all weights = 1). Default NULL.
#' @param vertex_weights Numeric vector of length N — per-vertex weights, or NULL
#'   (all weights = 1). Default NULL.
#' @param k Integer. Tuple size (0 = original graph).
#' @param variant Graph variant (default \code{DRESS_UNDIRECTED}).
#' @param max_iterations Maximum fitting iterations (default 100).
#' @param epsilon Convergence threshold (default 1e-6).
#' @param n_samples Integer. Number of random tuples to sample (default 0 = all).
#' @param seed Integer. Random seed (default 0).
#' @param precompute Logical. Pre-compute intercept index (default FALSE).
#' @param keep_multisets Logical. If TRUE, return per-tuple edge DRESS
#'   values in a matrix. Default FALSE.
#' @param compute_histogram Logical. If TRUE, compute and return the
#'   histogram. Default TRUE.
#'
#' @return A list with components:
#' \describe{
#'   \item{\code{histogram}}{Data frame with columns \code{value} and \code{count},
#'     containing the exact sparse histogram entries sorted by \code{value}.}
#'   \item{\code{multisets}}{Matrix of per-tuple edge values
#'     (only present when \code{keep_multisets = TRUE}).}
#'   \item{\code{num_tuples}}{Integer — number of tuples
#'     (only when \code{keep_multisets = TRUE}).}
#' }
#'
#' @examples
#' # Triangle K3, nabla-1
#' res <- nabla_fit(3L, c(0L,1L,2L), c(1L,2L,0L), k = 1L)
#' res$histogram
#'
#' @useDynLib dress.graph, .registration = TRUE
#' @export
nabla_fit <- function(n_vertices,
                            sources,
                            targets,
                            weights          = NULL,
                            vertex_weights     = NULL,
                            k                = 0L,
                            variant          = DRESS_UNDIRECTED,
                            max_iterations   = 100L,
                            epsilon          = 1e-6,
                            n_samples        = 0L,
                            seed             = 0L,
                            precompute       = FALSE,
                            keep_multisets   = FALSE,
                            compute_histogram = TRUE) {

  n_vertices     <- as.integer(n_vertices)
  sources        <- as.integer(sources)
  targets        <- as.integer(targets)
  if (!is.null(weights)) weights <- as.double(weights)
  if (!is.null(vertex_weights)) vertex_weights <- as.double(vertex_weights)
  k              <- as.integer(k)
  variant        <- as.integer(variant)
  max_iterations <- as.integer(max_iterations)
  epsilon        <- as.double(epsilon)
  n_samples      <- as.integer(n_samples)
  seed           <- as.integer(seed)
  precompute     <- as.integer(precompute)
  keep_multisets <- as.integer(keep_multisets)
  compute_histogram <- as.integer(compute_histogram)

  stopifnot(length(sources) == length(targets))
  if (!is.null(weights)) stopifnot(length(weights) == length(sources))
  if (!is.null(vertex_weights)) stopifnot(length(vertex_weights) == n_vertices)
  stopifnot(n_vertices >= 1L)
  stopifnot(k >= 0L)
  stopifnot(variant >= 0L && variant <= 3L)
  stopifnot(max_iterations >= 1L)
  stopifnot(epsilon > 0)

  .Call(C_nabla_dress_fit,
        n_vertices,
        sources,
        targets,
        weights,
        vertex_weights,
        k,
        variant,
        max_iterations,
        epsilon,
        n_samples,
        seed,
        precompute,
        keep_multisets,
        compute_histogram)
}

# ---- Library version -------------------------------------------------

#' DRESS library version string
#' @return Character scalar.
#' @useDynLib dress.graph, .registration = TRUE
#' @export
dress_version <- function() {
  .Call(C_dress_version)
}

# ---- Persistent DRESS object -----------------------------------------

#' Create a persistent DRESS graph object
#'
#' Initialises the underlying C graph and returns an environment-based
#' object with \code{$fit()}, \code{$get()}, \code{$result()} and
#' \code{$close()} methods.  The C graph is freed automatically when
#' garbage-collected, or explicitly via \code{$close()}.
#'
#' @param n_vertices Integer. Number of vertices (0-based).
#' @param sources Integer vector [E] — edge source endpoints (0-based).
#' @param targets Integer vector [E] — edge target endpoints (0-based).
#' @param weights Optional numeric vector [E] (NULL = unweighted).
#' @param variant Graph variant (default \code{DRESS_UNDIRECTED}).
#' @param precompute_intercepts Logical (default FALSE).
#'
#' @return An environment (class \code{"DRESS"}) with methods:
#' \describe{
#'   \item{\code{$fit(max_iterations, epsilon)}}{Fit the DRESS model.
#'     Returns list(iterations, delta).}
#'   \item{\code{$get(u, v, max_iterations, epsilon, edge_weight)}}{
#'     Query the DRESS value for an existing or virtual edge.}
#'   \item{\code{$result()}}{Extract current results (sources, targets,
#'     edge_dress, edge_weight, vertex_dress).}
#'   \item{\code{$close()}}{Explicitly free the C graph.}
#' }
#'
#' @examples
#' g <- DRESS(4L, c(0L,1L,2L,2L), c(1L,2L,0L,3L))
#' g$fit(100L, 1e-6)
#' g$get(0L, 3L, 100L, 1e-6, 1.0)
#' g$close()
#'
#' @useDynLib dress.graph, .registration = TRUE
#' @export
DRESS <- function(n_vertices,
                       sources,
                       targets,
                       weights               = NULL,
                       vertex_weights          = NULL,
                       variant               = DRESS_UNDIRECTED,
                       precompute_intercepts = FALSE) {

  n_vertices <- as.integer(n_vertices)
  sources    <- as.integer(sources)
  targets    <- as.integer(targets)
  variant    <- as.integer(variant)
  precompute <- as.integer(precompute_intercepts)

  stopifnot(length(sources) == length(targets))
  stopifnot(n_vertices >= 1L)
  stopifnot(variant >= 0L && variant <= 3L)

  if (!is.null(weights)) {
    weights <- as.double(weights)
    stopifnot(length(weights) == length(sources))
  }

  if (!is.null(vertex_weights)) {
    vertex_weights <- as.double(vertex_weights)
    stopifnot(length(vertex_weights) == n_vertices)
  }

  ptr <- .Call(C_dress_init,
               n_vertices, sources, targets, weights, vertex_weights,
               variant, precompute)

  self <- new.env(parent = emptyenv())
  self$.ptr <- ptr

  self$fit <- function(max_iterations = 100L, epsilon = 1e-6) {
    .Call(C_dress_fit_obj,
          self$.ptr,
          as.integer(max_iterations),
          as.double(epsilon))
  }

  self$delta_fit <- function(k                = 0L,
                             max_iterations   = 100L,
                             epsilon          = 1e-6,
                             n_samples        = 0L,
                             seed             = 0L,
                             keep_multisets   = FALSE,
                             compute_histogram = TRUE) {
    delta_fit(n_vertices, sources, targets,
                    weights          = weights,
                    vertex_weights     = vertex_weights,
                    k                = as.integer(k),
                    variant          = variant,
                    max_iterations   = as.integer(max_iterations),
                    epsilon          = as.double(epsilon),
                    n_samples        = as.integer(n_samples),
                    seed             = as.integer(seed),
                    precompute       = as.logical(precompute),
                    keep_multisets   = keep_multisets,
                    compute_histogram = compute_histogram)
  }

  self$nabla_fit <- function(k                = 0L,
                             max_iterations   = 100L,
                             epsilon          = 1e-6,
                             n_samples        = 0L,
                             seed             = 0L,
                             keep_multisets   = FALSE,
                             compute_histogram = TRUE) {
    nabla_fit(n_vertices, sources, targets,
                    weights          = weights,
                    vertex_weights     = vertex_weights,
                    k                = as.integer(k),
                    variant          = variant,
                    max_iterations   = as.integer(max_iterations),
                    epsilon          = as.double(epsilon),
                    n_samples        = as.integer(n_samples),
                    seed             = as.integer(seed),
                    precompute       = as.logical(precompute),
                    keep_multisets   = keep_multisets,
                    compute_histogram = compute_histogram)
  }

  self$get <- function(u, v, max_iterations = 100L, epsilon = 1e-6,
                       edge_weight = 1.0) {
    .Call(C_dress_get_obj,
          self$.ptr,
          as.integer(u),
          as.integer(v),
          as.integer(max_iterations),
          as.double(epsilon),
          as.double(edge_weight))
  }

  self$result <- function() {
    .Call(C_dress_result, self$.ptr)
  }

  self$close <- function() {
    .Call(C_dress_close, self$.ptr)
    invisible(NULL)
  }

  class(self) <- "DRESS"
  self
}
