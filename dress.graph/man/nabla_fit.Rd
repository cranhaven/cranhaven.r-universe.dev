\name{nabla_fit}
\alias{nabla_fit}
\title{Compute the Nabla-k-DRESS Histogram}
\description{
Compute the \eqn{\nabla^k}{Nabla^k}-DRESS histogram by enumerating all
\eqn{P(N,k)}{P(N,k)} ordered k-tuples of vertices, marking each tuple with
distinct generic vertex weights, running DRESS on each marked graph, and
pooling all converged edge values into a sparse exact histogram.
}
\usage{
nabla_fit(n_vertices, sources, targets, weights = NULL,
                vertex_weights = NULL, k = 0L, variant = DRESS_UNDIRECTED,
                max_iterations = 100L, epsilon = 1e-6,
                n_samples = 0L, seed = 0L,
                precompute = FALSE,
                keep_multisets = FALSE, compute_histogram = TRUE)
}
\arguments{
  \item{n_vertices}{Integer. Number of vertices (vertex ids must be in
    \code{0 .. n_vertices - 1}).}
  \item{sources}{Integer vector of length E -- edge source endpoints (0-based).}
  \item{targets}{Integer vector of length E -- edge target endpoints (0-based).}
  \item{weights}{Numeric vector of length E -- edge weights, or \code{NULL}
    for unweighted (all ones). Default \code{NULL}.}
  \item{vertex_weights}{Numeric vector of length N -- vertex weights, or \code{NULL}
    (all ones). Default \code{NULL}.}
  \item{k}{Integer. Number of vertices to individualize per tuple
    (0 = original graph, default 0).}
  \item{variant}{Graph variant (default \code{DRESS_UNDIRECTED}).
    One of \code{DRESS_UNDIRECTED} (0), \code{DRESS_DIRECTED} (1),
    \code{DRESS_FORWARD} (2), \code{DRESS_BACKWARD} (3).}
  \item{max_iterations}{Maximum number of fitting iterations per marked graph
    (default 100).}
  \item{epsilon}{Convergence threshold (default 1e-6).}
  \item{n_samples}{Integer. Number of random tuples to sample
    (0 = exhaustive enumeration, default 0).}
  \item{seed}{Integer. Random seed for sampling (default 0).}
  \item{precompute}{Logical. Pre-compute common-neighbor intercept index
    for faster iteration at the cost of more memory (default \code{FALSE}).}
  \item{keep_multisets}{Logical. If \code{TRUE}, return per-tuple edge
    DRESS values in a P(N,k) x E matrix.
    Default \code{FALSE}.}
  \item{compute_histogram}{Logical. If \code{FALSE}, skip histogram
    construction (default \code{TRUE}).}
}
\value{
A list with components:
  \item{histogram}{Data frame with columns \code{value} and \code{count},
    containing the exact sparse histogram entries sorted by \code{value}.}
  \item{multisets}{Matrix (P(N,k) x E) of per-tuple edge DRESS values
    (only present when \code{keep_multisets = TRUE}).}
  \item{num_tuples}{Integer -- P(N,k) (only present when
    \code{keep_multisets = TRUE}).}
}
\examples{
# Triangle K3, nabla-1: individualize 1 vertex at a time
res <- nabla_fit(3L, c(0L, 1L, 2L), c(1L, 2L, 0L), k = 1L)
res$histogram

# K4, nabla-0 (original graph, no individualization)
res0 <- nabla_fit(4L, c(0L,0L,0L,1L,1L,2L), c(1L,2L,3L,2L,3L,3L))
sum(res0$histogram$count)  # 6 edge values
}
\seealso{
\code{\link{fit}} for the standard DRESS computation.
\code{\link{delta_fit}} for the vertex-deletion variant.
}
