\name{Squares}
\alias{Squares}
\title{Areas of polygons}
\description{Calculates areas of polygons}

\usage{Squares(ppts, relative=FALSE)}

\arguments{

\item{ppts}{Output from Hulls() or Ellipses(), or just a list of
polygons}

\item{relative}{Calculate relative squares?}

}

\details{

"List of polygons" must be the list which contains any number of
two-column numerical matrices of data frames, each represents the
vertices of one polygon.

Might be useful to understand the variability of groups.

}

\value{

Numerical (possibly named) vector of polygon areas, one element per
polygon.

}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{Hulls}}, \code{\link{Ellipses}}}

\examples{

iris.pca <- prcomp(iris[, -5])
iris.p <- iris.pca$x[, 1:2]

iris.h <- Hulls(iris.p[, 1:2], as.numeric(iris[, 5]), plot=FALSE)
Squares(iris.h, relative=TRUE)

iris.e <- Ellipses(iris.p, iris$Species, plot=FALSE, centers=TRUE)
Squares(iris.e)

}

\keyword{multivariate}
