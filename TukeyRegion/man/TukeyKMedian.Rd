\name{TukeyKMedian}
\alias{TukeyKMedian}
\title{Computation of the Tukey median set and Tukey median}
\usage{
TukeyKMedian(data, algMedian = "upwards", method = "bfs",
  trgFacets = TRUE, retHalfspaces = FALSE, retHalfspacesNR = FALSE,
  retInnerPoint = FALSE, retVertices = TRUE, retFacets = TRUE,
  retVolume = FALSE, retBarycenter = TRUE, verbosity = 0)
}
\description{
Computes the Tukey median set and its barycenter, the Tukey median, starting from the region with dpeth \code{1} iteratively increasing it, according to the algorithm by Fojtik et al. (2022).
}
\arguments{
\item{data}{
data set for which the Tukey median shall be computed, a matrix having > 2 columns and more rows than columns.
}
\item{algMedian}{
the algorithm used to compute the Tukey median, a string containing \code{bsbarydepth}, or \code{"cutintwo"}, or \code{"downwards"}, or \code{"upwards"}, see `Details', \code{"bsbarydepth"} by default.
}
\item{method}{
the method to use to compute the Tukey region, a string containing \code{"bfs"}, or \code{"cmb"}, or \code{"bf"}, see \code{\link[TukeyRegion]{TukeyRegion}}, \code{"bfs"} by default.
}
\item{trgFacets}{
whether to triangulate facets, logical, \code{FALSE} by default. In this case no facet ridges are plotted, see `Value'.
}
\item{retHalfspaces}{
whether to return all found halfspaces, logical, \code{FALSE} by default.
}
\item{retHalfspacesNR}{
whether to return non-redundant halfspaces, logical, \code{FALSE} by default.
}
\item{retInnerPoint}{
whether to return inner point, logical, \code{FALSE} by default.
}
\item{retVertices}{
whether to return vertices, logical, \code{TRUE} by default.
}
\item{retFacets}{
whether to return facets, logical, \code{TRUE} by default.
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
The function computes the Tukey median set, \emph{i.e.} the region with the highest depth value, for \code{n} points in the Euclidean \code{d}-variate space contained in \code{data}.

It also computes this set's barycenter, which is the Tukey median. Four search algorithms are implemented: Algorithm \code{bsbarydepth} is the most efficient, it is the bisection algorithm starting with the lower bound as the maximum of the theoretical minimum and the depth of the componentwise median, and updating lower bound with the depth of the barycenter of the last found region. Algorithm \code{"cutintwo"} sequntially cuts the range of remaining depths into two parts starting with the range from \code{1} to the upper bound obtained by Liu, Luo, and Zuo (2016). Algorithm \code{"downwards"} is checking each depth value decrementally with step \code{1} starting with the upper bound obtained by Liu, Luo, and Zuo (2016) until the first existing region is found. Algorithm \code{"upwards"} is checking each depth value incrementally until the first non-existing region is found.

The main goal of the function is to provide the polytope (Tukey median set) and its barycenter (Tukey median); the settings can be adjusted though. After the median depth is found, the \code{\link[TukeyRegion]{TukeyRegion}} function is called.

See function \code{\link[TukeyRegion]{TukeyRegion}} for details regarding the output.
}
\value{
The function returns an object of class \code{TukeyRegion} with fields specified by \code{ret...}-flags in the arguments:
\item{data}{
the input data set.
}
\item{depth}{
chosen depth level.
}
\item{numRegions}{
number of time the depth region has been computed.
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
facets of the Tukey region. If input argument \code{trgFacets} is set, then this is a list where each element is an array enumerating numbers of the rows in field \code{vertices}, their number for each facet can vary. If input argument \code{trgFacets} is unset, then this is a matrix with each row corresponding to a triangulated facet, and no facets' ridges reconstruction is performed, so it cannot be visualized. If the field is absent then either no halfspaces or no inner point have been found or facet computation has not been requested by the input arguments.
}
\item{volume}{
volume of the Tukey region. If the field is absent then either no halfspaces or no inner point have been found or facet computation has not been requested by the input arguments.
}
\item{barycenter}{
the barycenter of the Tukey region. If the field is absent then either no halfspaces or no inner point have been found or facet computation has not been requested by the input arguments.
}
\item{numRidges}{
number of used ridges (for computing the last region).
}
}
\references{
Liu, X., Luo, S., and Zuo, Y. (2020). Some results on the computing of Tukey's halfspace median. \emph{Statistical Papers}, 61, 303-316.

Liu, X., Mosler, K., and Mozharovskyi, P. (2019). Fast computation of Tukey trimmed regions and median in dimension p > 2. \emph{Journal of Computational and Graphical Statistics}, 28, 682-697.

Vit Fojtik, Petra Laketa, Pavlo Mozharovskyi, and Stanislav Nagy (2022). On exact computation of Tukey depth central regions. \emph{arXiv:2208.04587}.

}
\author{
Pavlo Mozharovskyi <pavlo.mozharovskyi@ensai.fr>
}
\seealso{
\code{\link[TukeyRegion]{TukeyRegion}}, \code{\link{depth.wm}}
}
\examples{
# Load required packages
require(TukeyRegion)
require(MASS)
# Generate data
set.seed(1)
X <- mvrnorm(100, rep(0, 3),
  matrix(c(1, 1, 1, 1, 2, 2, 1, 2, 4), nrow = 3))
# Compute the Tukey median
Tm <- TukeyKMedian(X)
summary(Tm)
# Visualize the Tukey median
plot(Tm)
}
