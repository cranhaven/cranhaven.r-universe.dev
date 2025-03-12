\name{TukeyRegion-package}
\alias{TukeyRegion-package}
\alias{TukeyRegion-package}
\docType{package}
\title{
Computation of the Tukey Region and the Tukey Median
}

\description{
Tukey regions are polytopes in the Euclidean space, \emph{viz.} upper-level sets of the Tukey depth function on given data. The bordering hyperplanes of a Tukey region are computed as well as its vertices, facets, centroid, and volume. In addition, the Tukey median set, which is the non-empty Tukey region having highest depth level, and its barycenter (= Tukey median) are calculated. Tukey regions are visualized in dimension two and three. For details see Liu, Mosler, and Mozharovskyi (2019).
}

\details{
Proposed initially by John W. Tukey in 1975 (see also Donoho and Gasko, 1992) Tukey depth measures centrality of an arbitrary point in the Euclidean space w.r.t. a data cloud. For a point, Tukey (also halfspace or location) depth is defined as the smallest portion of observations that can be cut off by a closed halfspace containing this. For a given depth level Tukey (trimmed) region is defined as the upper level set of the Tukey depth function; it constitutes a closed polytope. The Tukey region of the highest level is referred to as the Tukey median set, while its barycenter is mentioned as the Tukey median. Due to properties of affine invariance, quasiconcavity, vanishing at infinity, high breakdown point of the median set, the Tukey depth attracted attention of statisticians and experienced substantial theoretical development. It is used in numerous applications including multivariate data analysis, outlier detection, tests for location (also scale and symmetry), classification, statistical quality control, imputation of missing data, etc.

Package \code{\link{TukeyRegion}} provides routines for computation (\code{\link[TukeyRegion]{TukeyRegion}}) and visualization (\code{\link{plot}}) of the Tukey depth trimmed region, the Tukey median set and Tukey median (\code{\link{TukeyMedian}}), and Tukey depth weighted and/or trimmed mean (\code{\link{depth.wm}}).

For cumputation of Tukey depth see function \code{\link{depth.halfspace}} of package \code{\link{ddalpha}}.

\tabular{ll}{
Package: \tab TukeyRegion\cr
Type: \tab Package\cr
Version: \tab 0.1.6.3\cr
Date: \tab 2023-04-17\cr
License: \tab GPL (>= 3)\cr
}

}

\author{
Authors:
C.B. Barber [aut, cph] (Qhull library), The Geometry Center of University of Minnesota [cph]  (Qhull library), Pavlo Mozharovskyi [aut, cre]

Maintainer: Pavlo Mozharovskyi, <pavlo.mozharovskyi@ensai.fr>
}

\references{

Donoho, D.L. and Gasko, M (1992). Breakdown properties of location estimates based on halfspace depth and projected outlyingness. \emph{The Annals of Statistics}, 20(4), 1803-1827.

Dyckerhoff, R. and Mozharovskyi, P. (2016). Exact computation of the halfspace depth. \emph{Computational Statistics and Data Analysis}, 98, 19-30.

Hallin, M., Paindaveine, D., and Siman, M. (2010). Multivariate quantiles and multiple-output regression quantiles: from L1-optimization to halfspace depth. \emph{The Annals of Statistics}, 38, 635-669.

Kong, L. and Mizera, I. (2012). Quantile tomography: using quantiles with multivariate data. \emph{Statistica Sinica}, 22, 1589-1610. Published online as \code{arXiv:0805.0056 [stat.ME]} (2008).

Liu, X., Luo, S., and Zuo, Y. (2020). Some results on the computing of Tukey's halfspace median. \emph{Statistical Papers}, 61, 303-316.

Liu, X., Mosler, K., and Mozharovskyi, P. (2019). Fast computation of Tukey trimmed regions and median in dimension p > 2. \emph{Journal of Computational and Graphical Statistics}, 28, 682-697.

Tukey, J.W. (1975). Mathematics and the picturing of data. In: James, R.D. (Ed.), \emph{Proceeding of the International Congress of Mathematicians (Volume 2)}, Canadian Mathematical Congress, Vancouver, 523-531.

}

\seealso{
\code{\link[TukeyRegion]{TukeyRegion}}, \code{\link{TukeyMedian}}, \code{\link{depth.wm}},

\code{\link{depth.halfspace}}, \code{\link{ddalpha}}.
}

\examples{
# Load required packages
require(TukeyRegion)
require(MASS)
# Generate data
set.seed(1)
X <- mvrnorm(100, rep(0, 3), diag(3))
# Compute and visualize two Tukey regions
Tr1 <- TukeyRegion(X, 5, "bfs",
  retFacets = TRUE, retVolume = TRUE, retBarycenter = TRUE)
Tr2 <- TukeyRegion(X, 25, "bfs",
  retFacets = TRUE, retVolume = TRUE, retBarycenter = TRUE)
plot(Tr1, colorFacets = "red", colorRidges = "red",
  colorPoints = "blue", alpha = 0.35)
plot(Tr2, newPlot = FALSE, drawPoints = FALSE, colorFacets = "green",
  colorRidges = "green", alpha = 0.65)
(Tr1$barycenter)
(Tr2$barycenter)
# Compute arithmetic mean
T.mean <- colMeans(X)
(T.mean)
# Compute Tukey depth trimmed weighted mean (approximate depth)
T.approx1 <- depth.wm(X, 0.25)
(T.approx1)
T.approx2 <- depth.wm(X, 75)
(T.approx2)
# Compute Tukey depth trimmed weighted mean (exact depth)
T.exact1 <- depth.wm(X, 0.25, exact = TRUE)
(T.exact1)
T.exact2 <- depth.wm(X, 75, exact = TRUE)
(T.exact2)
# Compute and visualize Tukey median
Tm <- TukeyMedian(X)
(Tm$barycenter)
plot(Tm, newPlot = FALSE, drawPoints = FALSE)
}

\keyword{ Tukey region }
\keyword{ Tukey depth }
\keyword{ Halfspace depth }
\keyword{ Location depth }
\keyword{ Halfspace contour }
\keyword{ Tukey median }
\keyword{ Barycenter }
\keyword{ Depth-weighted trimmed mean }
