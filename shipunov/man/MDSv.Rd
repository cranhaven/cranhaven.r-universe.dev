\name{MDSv}
\alias{MDSv}
\title{MDS: dimension importance ("explained variance" surrogate)}

\description{
Calculates R-squared coefficients of the linear relationships between
each of derived variables and original data
}

\usage{MDSv(scores)}

\arguments{
\item{scores}{Data frame or matrix with values (e.g., result of isoMDS())}
}

\details{

MDSv() converts each of the derived variables and original data into
distance matrices, and then uses lm() to calculate adjusted R-squared
coefficients. These coefficients may be used to understand the
"importance" of each new dimension. They work for any dimension
reduction techique including multidimensional scaling.

}

\value{Numeric vector, one values per column of scores}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{

iris.dist <- dist(unique(iris[, -5]), method="manhattan")

iris.cmd <- cmdscale(iris.dist)
MDSv(iris.cmd)

iris.p <- prcomp(iris[, -5])
MDSv(iris.p$x)
100*summary(iris.p)$importance[2, ] # compare with MDSv() results

}

\keyword{multivariate}

