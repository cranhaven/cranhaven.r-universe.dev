\name{TukeyRegions}
\alias{TukeyRegions}
\title{Computation of the Tukey Region}
\usage{
TukeyRegions(data, depths, method = "bfs",
  trgFacets = FALSE, checkInnerPoint = TRUE,
  retHalfspaces = TRUE, retHalfspacesNR = FALSE,
  retInnerPoint = FALSE, retVertices = FALSE,
  retFacets = FALSE, retVolume = FALSE, retBarycenter = FALSE,
  verbosity = 0L)
}
\description{
Computes the Tukey depth trimmed regions for given depth levels.
}
\arguments{
\item{data}{
data set for which the Tukey region shall be computed, a matrix having \code{> 2} columns and more rows than columns.
}
\item{depths}{
depths of the Tukey regions to compute, a vector of integers between \code{1} and the half of the number of rows in \code{data}.
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
\item{verbosity}{
level of details to print during execution, integer, from \code{0} (= print nothing) to \code{2} (= print all details).
}
}
\details{
The function computes the Tukey regions (upper-level set of the Tukey depth function) for \code{n} points in the Euclidean \code{d}-variate space contained in \code{data} at the depth values specified in the argument \code{depths}. This function calls multiple times function \code{\link[TukeyRegion]{TukeyRegion}} and provides computational convenience by shifting the loop on the \code{C++} level.

The main difference with \code{\link{TukeyMedian}} is that arguments \code{halfspaces} and/or \code{innerPoint} cannot be provided here anymore.

}
\value{
The function returns a list of objects of class \code{TukeyRegion} with fields specified by \code{ret...}-flags in the arguments:
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
\code{\link{TukeyRegion}}
\code{\link{TukeyMedian}}
}
\examples{
# Load required packages
require(TukeyRegion)
require(MASS)
# Generate data
set.seed(1)
X <- mvrnorm(500, rep(0, 3),
  matrix(c(1, 0.25, 0.25, 0.25, 1, 0.25, 0.25, 0.25, 1), nrow = 3))
# Compute the Tukey region
Trs <- TukeyRegions(X, c(5, 25), "bfs",
  retFacets = TRUE, retVolume = TRUE, retBarycenter = TRUE)
summary(Trs[[1]])
summary(Trs[[2]])
# Visualize the Tukey region
plot(Trs[[2]], drawRidges = FALSE, colorFacets = "green", alpha = 1)
plot(Trs[[1]], drawRidges = FALSE, newPlot = FALSE, colorFacets = "blue",
  alpha = 0.5)
}
