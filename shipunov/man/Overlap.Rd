\name{Overlap}
\alias{Overlap}
\alias{summary.Overlap}

\title{Calculates overlap between polygons}

\description{

Calculates overlaps between polygons (typically, convex hulls or
confidence ellipses from some scatterplot). Requires 'PBSmapping'
package.

}

\usage{
Overlap(ppts, symmetric=FALSE, negative=FALSE)
\method{summary}{Overlap}(object, ...)
}

\arguments{

\item{ppts}{List with hulls information (e.g., output from Hulls())}

\item{symmetric}{Make overlaps symmetric (like in distance matrix)?}

\item{negative}{Calculate "negative overlaps" (relative distance between
non-overlapped hulls)?}

\item{object}{Object of the class 'Overlap'}

\item{...}{Additional arguments}

}

\details{

The main idea of Overlap() is to provide the measurement of the
separation between groups in 2D space.

Overlap() employs calculations of areas of polygons and their intersects
provided by 'PBSmapping' package. Initially, it was based on the code
provided by J. Oksanen for his "ordihulldist" function.

By default, overlaps are asymmetric, so overlap between a and b is not
necessarily equal to the overlap between b and a. If 'symmetric=TRUE',
then Overlap() will calculate symmetric overlaps, less precise but more
suitable, e.g., for interpreting overlaps as distances.

When 'negative=TRUE', Overlap() calculates also negative polygon-based
distances between non-overlapping polygons. They are symmetric and might
be used as similarities too (please look on examples).

summary.Overlap() provides some general numbers, including mean and total
overlaps for each hull. In these calculations, hulls without overlaps are
ignored. Note that summary.Overlap() calculates the arithmetic, not
geometric, mean (whereras symmetric Overlap() uses geometric mean). The
average of all overlaps could serve as the reliable measure of the
quality of dimension reduction.

Please also check out vegan::ordiareatest() function; this studies the
one-side hypothesis that actuall hull areas are smaller than with
randomized groups (i.e., that actuall hulls are better than random).

}
\value{
Object (square matrix) of class 'Overlap', or nothing.
}

\references{

Serebryanaya A., Shipunov A. 2009. Morphological variation of plants on
the uprising islands of northern Russia. Annales Botanici Fennici. 2009.
46: 81-89.

}

\author{Alexey Shipunov}

\seealso{\code{\link{Hulls}}, \code{\link{Ellipses}}}

\examples{

iris.p <- prcomp(iris[, -5])$x[, 1:2]
iris.h <- Hulls(iris.p, iris$Species, plot=FALSE)

Overlap(iris.h)
Overlap(iris.h, negative=TRUE)
Overlap(iris.h, symmetric=TRUE)

(iris.o <- Overlap(iris.h, symmetric=TRUE, negative=TRUE))
as.dist(1 - iris.o) # how to convert overlaps into distance-like objects

summary(Overlap(iris.h))
summary(Overlap(iris.h, negative=TRUE))
summary(Overlap(iris.h, symmetric=TRUE))
summary(iris.o)

iris.e <- Ellipses(iris.p, iris$Species, plot=FALSE, centers=TRUE)
Overlap(iris.e, negative=TRUE)

}

\keyword{multivariate}
