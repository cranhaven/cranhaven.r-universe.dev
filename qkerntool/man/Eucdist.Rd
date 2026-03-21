

\name{Eucdist}
\alias{Eucdist}
\alias{Eucdist,matrix-method}

\title{Computes the Euclidean(square Euclidean) distance matrix}

\description{
  \code{Eucdist} Computes the Euclidean(square Euclidean) distance matrix.
 }
\arguments{
\item{x}{ (NxD) matrix  (N samples, D features)}
\item{y}{ (MxD) matrix  (M samples, D features)}
\item{sEuclidean}{can be TRUE or FALSE, FALSE to Compute the Euclidean distance matrix.}
}
\value{
E - (MxN) Euclidean (square Euclidean) distances between vectors in x and y
}
\author{Yusen Zhang \cr
\email{yusenzhang@126.com}}

\examples{
###
data(iris)
testset <- sample(1:150,20)
x <- as.matrix(iris[-testset,-5])
y <- as.matrix(iris[testset,-5])

##
res0 <- Eucdist(x)
res1 <- Eucdist(x, x, sEuclidean = FALSE)
res2 <- Eucdist(x, y = NULL, sEuclidean = FALSE)
res3 <- Eucdist(x, x, sEuclidean = TRUE)
res4 <- Eucdist(x, y = NULL)
res5 <- Eucdist(x, sEuclidean = FALSE)
}
