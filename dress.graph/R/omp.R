# ========================================================================
#  omp — OpenMP-parallel DRESS, same API as the CPU functions.
#
#  Usage (import-based switching):
#
#    library(dress.graph)
#
#    # CPU
#    result <- fit(4L, sources, targets)
#
#    # OpenMP — same function name, accessed via omp$ environment
#    result <- omp$fit(4L, sources, targets)
# ========================================================================

#' OpenMP-parallel DRESS functions.
#'
#' An environment containing OpenMP-parallel versions of \code{fit}
#' and \code{delta_fit} with identical signatures.  Switch from
#' CPU to OpenMP by prefixing calls with \code{omp$}.
#'
#' \code{omp$fit} parallelises edges within each iteration.
#' \code{omp$delta_fit} parallelises the outer subgraph loop.
#'
#' @examples
#' \dontrun{
#'   # CPU
#'   r1 <- fit(4L, c(0L,1L,2L,2L), c(1L,2L,0L,3L))
#'
#'   # OpenMP — same signature
#'   r2 <- omp$fit(4L, c(0L,1L,2L,2L), c(1L,2L,0L,3L))
#' }
#' @export
omp <- local({

  env <- new.env(parent = emptyenv())

  env$fit <- function(n_vertices,
                            sources,
                            targets,
                            weights              = NULL,
                            vertex_weights         = NULL,
                            variant              = DRESS_UNDIRECTED,
                            max_iterations       = 100L,
                            epsilon              = 1e-6,
                            precompute_intercepts = FALSE) {

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

    .Call(C_dress_fit_omp,
          n_vertices, sources, targets, weights, vertex_weights,
          variant, max_iterations, epsilon, precompute)
  }

  env$delta_fit <- function(n_vertices,
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

    .Call(C_delta_dress_fit_omp,
          n_vertices, sources, targets, weights, vertex_weights,
          k, variant, max_iterations, epsilon,
          n_samples, seed,
          precompute, keep_multisets,
          compute_histogram)
  }

  env$nabla_fit <- function(n_vertices,
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

    .Call(C_nabla_dress_fit_omp,
          n_vertices, sources, targets, weights, vertex_weights,
          k, variant, max_iterations, epsilon,
          n_samples, seed,
          precompute, keep_multisets,
          compute_histogram)
  }

  lockEnvironment(env)
  env
})
