\name{Polyarea}
\alias{Polyarea}
\title{Area of the polygon}
\description{Calculates area of polygon}

\usage{Polyarea(x)}

\arguments{

\item{x}{Polygon vertices: two-column numerical matrix or data frame}

}

\details{

Based on vegan::summary.ordihulls().

}

\value{

Numerical vector of length 1.

}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{Squares}}, \code{\link{Hulls}}, \code{\link{Ellipses}}}

\examples{

x <- c(1:9, 8:1) # from ?polygon
y <- c(1, 2*(5:3), 2, -1, 17, 9, 8, 2:9)
Polyarea(cbind(x, y)) # numerical matrix
Polyarea(data.frame(x, y)) # numerical data frame

}

\keyword{multivariate}
