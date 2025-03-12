\name{TukeyRegion}
\alias{TukeyRegion}
\title{Computation of the Tukey Region}
\usage{
TukeyRegion(data, depth, method = "bfs",
  trgFacets = FALSE, checkInnerPoint = TRUE,
  retHalfspaces = TRUE, retHalfspacesNR = FALSE,
  retInnerPoint = FALSE, retVertices = FALSE,
  retFacets = FALSE, retVolume = FALSE, retBarycenter = FALSE,
  halfspaces = matrix(0), innerPoint = numeric(1),
  verbosity = 0L)
}
\description{
Computes the Tukey depth trimmed region for a given depth level.
}
\arguments{
\item{data}{
data set for which the Tukey region shall be computed, a matrix having \code{> 2} columns and more rows than columns.
}
\item{depth}{
depth of the Tukey region, an integer between \code{1} and the half of the number of rows in \code{data}.
}
\item{method}{
the method to use to compute the Tukey region, a string containing \code{"bfs"}, or \code{"cmb"}, or \code{"bf"}, see `Details', \code{"bfs"} by default.
}
\item{trgFacets}{
whether to triangulate facets, logical, \code{FALSE} by default. In this case no facet ridges are plotted, see `Value'.
}
\item{checkInnerPoint}{
whether to check correctness of the inner point in case it is provided, logical, \code{TRUE} by default.
}
\item{retHalfspaces}{
whether to return all found halfspaces, logical, \code{TRUE} by default.
}
\item{retHalfspacesNR}{
whether to return non-redundant halfspaces, logical, \code{FALSE} by default.
}
\item{retInnerPoint}{
whether to return inner point, logical, \code{FALSE} by default.
}
\item{retVertices}{
whether to return vertices, logical, \code{FALSE} by default.
}
\item{retFacets}{
whether to return facets, logical, \code{FALSE} by default.
}
\item{retVolume}{
whether to return volume, logical, \code{FALSE} by default.
}
\item{retBarycenter}{
whether to return the region's barycenter, logical, \code{FALSE} by default.
}
\item{halfspaces}{
halfspaces defining the Tukey region by their intersection, a matrix with number of columns equal to space dimension and where each row corresponds to a halfspace defined by three point numbers in \code{data}.
}
\item{innerPoint}{
inner point, a vector of length equal to dimension.
}
\item{verbosity}{
level of details to print during execution, integer, from \code{0} (= print nothing) to \code{2} (= print all details).
}
}
\details{
The function computes the Tukey region (upper-level set of the Tukey depth function) for \code{n} points in the Euclidean \code{d}-variate space contained in \code{data} at the depth value \code{depth}.

Three methods are implemented: Method \code{"bfs"} is the most efficient, it starts with an initial set of ridges and traverses all facets using the \bold{breadth-first search} algorithm. Method \code{"cmb"} considers all subspaces spanned by \bold{combinations} of \code{d - 1} points, projects \code{data} onto their orthogonal complements (planes), and searches for bivariate quantiles these planes. Method \code{"bf"} employs the \bold{brute-force} strategy by checking all halfspaces defined by hyperplanes containing \code{d} points from \code{data}. If \code{d = 2}, method \code{"bf"} is used. See Liu, Mosler, and Mozharovskyi (2019) for details on algorithms.

The function proceeds in three main steps. Step 1: Calculate all the halfspaces defining Tukey region in their intersection. Many of them are usually redundant. Step 2: Find the inner point of the Tukey region, \emph{i.e.} a point which lies simultaneously in all the before calculated halfspaces. If such a point does not exist neither does the Tukey region exist for this depth level. The algorithm stops and returns \code{FALSE} in the field \code{innerPointFound}. If the inner point has been found, the algorithm proceeds to Step 3: Filter the halfspaces leaving only those containing the facets of the Tukey region. Step 3 provides infirmation to compute vertices, facets, volume, and barycenter of the Tukey region.

\code{halfspaces} and/or \code{innerPoint} can be provided as function arguments.

The function tries to fulfill all the requirements indicated by the input flags. Step 1 is performed anyway (even if \code{retHalfspaces} is unset, which means the halfspaces just should not be output, except they are provided by the argument \code{halfspaces}). If any further \code{ret...}-flag is set Step 2 is performed, except \code{retHalfspacesNR} is unset and the argument \code{innerPoint} provided. If any of \code{retVertices}, \code{retFacets}, \code{retVolume}, \code{retBarycenter} is set, Step 3 is performed.

The region can be visualized in 2- and 3-dimensional space by \code{plot(...)}, general information can be printed by \code{print(...)}, statistics can be summarized by \code{summary(...)}.
}
\value{
The function returns an object of class \code{TukeyRegion} with fields specified by \code{ret...}-flags in the arguments:
\item{data}{
the input data set.
}
\item{depth}{
chosen depth level.
}
\item{halfspacesFound}{
whether at least one of the determining Tukey region halfspaces has been found.
}
\item{halfspaces}{
if requested, halfspaces defining the Tukey region by their intersection, a matrix with number of columns equal to space dimension and where each row corresponds to a halfspace defined by three point numbers in \code{data}.
}
\item{innerPointFound}{
a logical indicating whether an inner point of the region has been found. If \code{FALSE} then the region of the given depth does not exist. If the field is absent then the inner point has not been requested by the input arguments.
}
\item{innerPoint}{
coordinates of a point inside of the Tukey region. If the field is absent then either no halfspaces or no inner point have been found or facet computation has not been requested by the input arguments.
}
\item{halfspacesNR}{
non-redundant halfspaces (\emph{i.e.} those containing Tukey region's facets), a matrix with number of columns equal to space dimension and where each row corresponds to a halfspace defined by three point numbers in \code{data}. If the field is absent then either no halfspaces or no inner point have been found or facet computation has not been requested by the input arguments.
}
\item{vertices}{
vertices of the Tukey region, a matrix with number of columns equal to space dimension and where each row represents vertex coordinates. If the field is absent then either no halfspaces or no inner point have been found or facet computation has not been requested by the input arguments. If field \code{facets} is returned, this field is returned as well.
}
\item{triangulated}{
a logical repeating the \code{trgFacets} input argument. If the field is absent then either no halfspaces or no inner point have been found or facet computation has not been requested by the input arguments.
}
\item{facets}{
facets of the Tukey region. If input argument \code{trgFacets} is set, then this is a list where each element is an array enumerating numbers of the rows in field \code{vertices}; their number for each facet can vary. If input argument \code{trgFacets} is unset, then this is a matrix with each row corresponding to a triangulated facet, and no facets' ridges reconstruction is performed. So it cannot be visualized. If the field is absent then either no halfspaces or no inner point have been found or facet computation has not been requested by the input arguments.
}
\item{volume}{
volume of the Tukey region. If the field is absent then either no halfspaces or no inner point have been found or facet computation has not been requested by the input arguments.
}
\item{barycenter}{
the barycenter of the Tukey region. If the field is absent then either no halfspaces or no inner point have been found or facet computation has not been requested by the input arguments.
}
\item{numRidges}{
number of used ridges.
}
}
\references{
Liu, X., Mosler, K., and Mozharovskyi, P. (2019). Fast computation of Tukey trimmed regions and median in dimension p > 2. \emph{Journal of Computational and Graphical Statistics}, 28, 682-697.
}
\author{
Pavlo Mozharovskyi <pavlo.mozharovskyi@ensai.fr>
}
\seealso{
\code{\link{TukeyMedian}}
}
\examples{
# Load required packages
require(TukeyRegion)
require(MASS)
# Generate data
set.seed(1)
X <- mvrnorm(500, rep(0, 3),
  matrix(c(1, 1, 1, 1, 2, 2, 1, 2, 4), nrow = 3))
# Compute the Tukey region
Tr <- TukeyRegion(X, 10, "bfs",
  retFacets = TRUE, retVolume = TRUE, retBarycenter = TRUE)
summary(Tr)
# Visualize the Tukey region
plot(Tr)
}
