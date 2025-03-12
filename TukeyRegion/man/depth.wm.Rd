\name{depth.wm}
\alias{depth.wm}
\title{Computation of the Tukey depth weighted and/or trimmed mean}
\usage{
depth.wm(data, depth.level = 1/nrow(data), weighted = TRUE, 
  break.ties = "atRandom", ...)
}
\description{
Computes the Tukey depth weighted and/or trimmed for a given depth level or for a given number of deepest points.
}
\arguments{
\item{data}{
data set for which the weighted mean should be computed, a matrix having > 2 columns and more rows than columns.
}
\item{depth.level}{
either Tukey depth level for trimming (a numeric between 1/(number of rows in \code{data}) and 1) or the number of deepest points to take into account (an integer between one and the number of rows in \code{data}).
}
\item{weighted}{
whether the trimmed mean should be weighted by depth, logical, \code{TRUE} by default.
}
\item{break.ties}{
the way to break ties if the number of deepest points is given, character. If \code{"atRandom"} (by default) ties are broken at random, for any other value input points' order is used.
}
\item{...}{
further agruments passed to function \code{\link{depth.halfspace}} of package \code{\link{ddalpha}}. See `Details' for additional information.
}
}
\details{
After having computed the Tukey depth of each point in \code{data} the function operates in two possible modes. If \code{depth.level} lies between 0 and 1 then the function computes trimmed (weighted if specified by flag \code{weighted}) mean of all points having at least given depth level. If \code{depth.level} specifies the number of points (an integer between 1 and number of rows in \code{data}) then the trimmed (weighted) mean of \code{depth.level} deepest points are calculated breaking ties due to argument \code{break.ties} (ties can occur due to discrete nature of the Tukey depth). This follows the idea of Donoho and Gasko (1992), also see this article for the breakdown point.

 Depth of points is calculated by means of external function \code{\link{depth.halfspace}} from package \code{\link{ddalpha}}, whose arguments can be specified as well. In particular, argument \code{exact} specifies whether Tukey depth is computed exactly (\code{TRUE}) or approximated (\code{FALSE}) by random projections; for the latter case argument \code{num.directions} specifies the number of random directions to use. For further details about the algorithm see Dyckerhoff and Mozharovskyi (2016).
}
\value{
The function returns the weighted and/or trimmed mean, a point in the \code{d}-variate Euclidean space (\code{d} is the number of columns in \code{data}), a numeric vector.
}
\references{
Donoho, D.L. and Gasko, M (1992). Breakdown properties of location estimates based on halfspace depth and projected outlyingness. \emph{The Annals of Statistics}, 20(4), 1803-1827.

Dyckerhoff, R. and Mozharovskyi, P. (2016). Exact computation of the halfspace depth. \emph{Computational Statistics and Data Analysis}, 98, 19-30.
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
require(bfp)
# Generate data
set.seed(1)
X <- bfp:::rmvt(150, diag(3), rep(0, 3), 1)
# Compute arithmetic mean
T.mean <- colMeans(X)
(T.mean)
# Compute Tukey depth trimmed weighted mean (approximate depth)
T.approx1 <- depth.wm(X, 0.25)
(T.approx1)
T.approx2 <- depth.wm(X, 25)
(T.approx2)
# Compute Tukey depth trimmed weighted mean (exact depth)
T.exact1 <- depth.wm(X, 0.25, exact = TRUE)
(T.exact1)
T.exact2 <- depth.wm(X, 25, exact = TRUE)
(T.exact2)
# Compute Tukey median
Tm <- TukeyMedian(X)
(Tm$barycenter)
}
