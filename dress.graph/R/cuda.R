# ========================================================================
#  cuda — GPU-accelerated DRESS, same API as the CPU functions.
#
#  Usage (import-based switching):
#
#    library(dress.graph)
#
#    # CPU
#    result <- fit(4L, sources, targets)
#
#    # CUDA — same function name, accessed via cuda$ environment
#    result <- cuda$fit(4L, sources, targets)
# ========================================================================

#' CUDA-accelerated DRESS functions.
#'
#' An environment containing GPU-accelerated versions of \code{fit}
#' and \code{delta_fit} with identical signatures.  Switch from
#' CPU to GPU by prefixing calls with \code{cuda$}.
#'
#' @examples
#' \dontrun{
#'   # CPU
#'   r1 <- fit(4L, c(0L,1L,2L,2L), c(1L,2L,0L,3L))
#'
#'   # CUDA — same signature
#'   r2 <- cuda$fit(4L, c(0L,1L,2L,2L), c(1L,2L,0L,3L))
#' }
#' @export
cuda <- local({

  env <- new.env(parent = emptyenv())

  .check_cuda <- function() {
    ok <- tryCatch(
      { getNativeSymbolInfo("C_dress_fit_cuda", "dress.graph"); TRUE },
      error = function(e) FALSE
    )
    if (!ok)
      stop("CUDA support not available. Rebuild dress.graph with DRESS_CUDA=1.",
           call. = FALSE)
  }

  env$fit <- function(n_vertices,
                            sources,
                            targets,
                            weights              = NULL,
                            vertex_weights         = NULL,
                            variant              = DRESS_UNDIRECTED,
                            max_iterations       = 100L,
                            epsilon              = 1e-6,
                            precompute_intercepts = FALSE) {

    .check_cuda()

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

    .Call("C_dress_fit_cuda",
          n_vertices, sources, targets, weights, vertex_weights,
          variant, max_iterations, epsilon, precompute,
          PACKAGE = "dress.graph")
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

    .check_cuda()

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

    .Call("C_delta_dress_fit_cuda",
          n_vertices, sources, targets, weights, vertex_weights,
          k, variant, max_iterations, epsilon,
          n_samples, seed,
          precompute, keep_multisets,
          compute_histogram,
          PACKAGE = "dress.graph")
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

    .check_cuda()

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

    .Call("C_nabla_dress_fit_cuda",
          n_vertices, sources, targets, weights, vertex_weights,
          k, variant, max_iterations, epsilon,
          n_samples, seed,
          precompute, keep_multisets,
          compute_histogram,
          PACKAGE = "dress.graph")
  }

  env$DRESS <- function(n_vertices,
                        sources,
                        targets,
                        weights               = NULL,
                        vertex_weights          = NULL,
                        variant               = DRESS_UNDIRECTED,
                        precompute_intercepts = FALSE) {

    .check_cuda()

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
      .Call("C_dress_fit_cuda_obj",
            self$.ptr,
            as.integer(max_iterations),
            as.double(epsilon),
            PACKAGE = "dress.graph")
    }
    self$delta_fit <- function(k                = 0L,
                               max_iterations   = 100L,
                               epsilon          = 1e-6,
                               n_samples        = 0L,
                               seed             = 0L,
                               keep_multisets   = FALSE,
                               compute_histogram = TRUE) {
      cuda$delta_fit(n_vertices, sources, targets,
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
      cuda$nabla_fit(n_vertices, sources, targets,
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

  env
})
