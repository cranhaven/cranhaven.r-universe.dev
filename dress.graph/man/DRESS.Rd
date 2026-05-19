\name{DRESS}
\alias{DRESS}
\title{Persistent DRESS Graph Object}
\description{
Create a persistent DRESS graph that stays alive across multiple
\code{fit} / \code{get} (virtual-edge query) calls without
rebuilding the graph each time.
}
\usage{
DRESS(n_vertices, sources, targets, weights = NULL,
      vertex_weights = NULL, variant = DRESS_UNDIRECTED,
      precompute_intercepts = FALSE)
}
\arguments{
  \item{n_vertices}{Integer. Number of vertices (vertex ids must be in
    \code{0 .. n_vertices - 1}).}
  \item{sources}{Integer vector of length E -- edge source endpoints (0-based).}
  \item{targets}{Integer vector of length E -- edge target endpoints (0-based).}
  \item{weights}{Optional numeric vector of length E -- per-edge weights.
    \code{NULL} (default) gives every edge weight 1.}
  \item{vertex_weights}{Optional numeric vector of length N -- per-vertex weights.
    \code{NULL} (default) gives every vertex weight 1.}
  \item{variant}{Graph variant (default \code{DRESS_UNDIRECTED}).
    One of \code{DRESS_UNDIRECTED} (0), \code{DRESS_DIRECTED} (1),
    \code{DRESS_FORWARD} (2), \code{DRESS_BACKWARD} (3).}
  \item{precompute_intercepts}{Logical. Pre-compute common-neighbor index
    for faster iteration at the cost of more memory (default \code{FALSE}).}
}
\value{
An environment of class \code{"DRESS"} with the following methods:
  \item{\code{$fit(max_iterations = 100L, epsilon = 1e-6)}}{Run iterative
    fitting.  Returns a list with \code{iterations} and \code{delta}.}
  \item{\code{$get(u, v, max_iterations = 100L, epsilon = 1e-6,
    edge_weight = 1.0)}}{Query the DRESS value for an existing or
    virtual edge between vertices \code{u} and \code{v}.}
  \item{\code{$result()}}{Extract current results as a list with
    \code{sources}, \code{targets}, \code{edge_dress}, \code{edge_weight},
    and \code{vertex_dress}.}
  \item{\code{$close()}}{Explicitly free the underlying C graph.  Called
    automatically by the garbage collector if not invoked manually.}
}
\examples{
g <- DRESS(4L, c(0L,1L,2L,2L), c(1L,2L,0L,3L))
g$fit(100L, 1e-6)
g$get(0L, 3L, 100L, 1e-6, 1.0)
r <- g$result()
g$close()
}
\references{
E. Castrillo, E. Leon, J. Gomez. Dynamic Structural Similarity on Graphs.
arXiv:1805.01419, 2018.
}
\keyword{graphs}
