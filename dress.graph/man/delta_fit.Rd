\name{delta_fit}
\alias{delta_fit}
\title{Compute the Delta-k-DRESS Histogram}
\description{
Compute the \eqn{\Delta^k}{Delta^k}-DRESS histogram by exhaustively removing
all k-vertex subsets and measuring the change in edge similarity values.
The result is a sparse exact histogram of edge DRESS values pooled across all
\eqn{\binom{N}{k}}{C(N,k)} subgraphs.
}
\usage{
delta_fit(n_vertices, sources, targets, weights = NULL,
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
  \item{k}{Integer. Number of vertices to remove per subset
    (0 = original graph, default 0).}
  \item{variant}{Graph variant (default \code{DRESS_UNDIRECTED}).
    One of \code{DRESS_UNDIRECTED} (0), \code{DRESS_DIRECTED} (1),
    \code{DRESS_FORWARD} (2), \code{DRESS_BACKWARD} (3).}
  \item{max_iterations}{Maximum number of fitting iterations per subgraph
    (default 100).}
  \item{epsilon}{Convergence threshold (default 1e-6).}
  \item{n_samples}{Integer. Number of random subgraphs to sample
    (0 = exhaustive enumeration, default 0).}
  \item{seed}{Integer. Random seed for sampling (default 0).}
  \item{precompute}{Logical. Pre-compute common-neighbor intercept index
    for faster iteration at the cost of more memory (default \code{FALSE}).}
  \item{keep_multisets}{Logical. If \code{TRUE}, return per-subgraph edge
    DRESS values in a C(N,k) x E matrix (\code{NaN} for removed edges).
    Default \code{FALSE}.}
  \item{compute_histogram}{Logical. If \code{FALSE}, skip histogram
    construction (default \code{TRUE}).}
}
\value{
A list with components:
  \item{histogram}{Data frame with columns \code{value} and \code{count},
    containing the exact sparse histogram entries sorted by \code{value}.}
  \item{multisets}{Matrix (C(N,k) x E) of per-subgraph edge DRESS values
    (only present when \code{keep_multisets = TRUE}; \code{NaN} = removed edge).}
  \item{num_subgraphs}{Integer -- C(N,k) (only present when
    \code{keep_multisets = TRUE}).}
}
\examples{
# Triangle K3, delta-1: remove 1 vertex at a time
res <- delta_fit(3L, c(0L, 1L, 2L), c(1L, 2L, 0L), k = 1L)
res$histogram

# K4, delta-0 (original graph)
res0 <- delta_fit(4L, c(0L,0L,0L,1L,1L,2L), c(1L,2L,3L,2L,3L,3L))
sum(res0$histogram$count)  # 6 edge values
}
\seealso{
\code{\link{fit}} for the standard DRESS computation.
}
