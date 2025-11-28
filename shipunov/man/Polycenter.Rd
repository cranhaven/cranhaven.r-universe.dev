\name{Polycenter}
\alias{Polycenter}
\title{Center of the polygon}
\description{Finds polygon center}

\usage{Polycenter(x)}

\arguments{

\item{x}{Polygon vertices: two-column numerical matrix or data frame}

}

\details{

Based on vegan::summary.ordihulls().

}

\value{

Named numerical vector of length 2 (x and y coordinates of the center).

}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{Squares}}, \code{\link{Hulls}}, \code{\link{Ellipses}}}

\examples{

x <- c(1:9, 8:1) # from ?polygon
y <- c(1, 2*(5:3), 2, -1, 17, 9, 8, 2:9)
Polycenter(cbind(x, y)) # numerical matrix
Polycenter(data.frame(x, y)) # numerical data frame

iris.p <- prcomp(iris[, -5])$x[, 1:2]
iris.h <- Hulls(iris.p, iris$Species, plot=FALSE)
sapply(iris.h, Polycenter)

}

\keyword{multivariate}
