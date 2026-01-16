\name{LPDistance}
\alias{LPDistance}

\title{
Lp distances. 
}
\description{
Computes the distance based on the chosen Lp norm between a pair of numeric vectors.
}

\usage{
LPDistance(x, y, method="euclidean", ...)
}

\arguments{
  \item{x}{
  Numeric vector containing the first time series.
}

  \item{y}{
  Numeric vector containing the second time series.
}

\item{method}{
  A value in "euclidean", "manhattan", "infnorm", "minkowski".
}

  \item{...}{
  	If method="minkowski" a positive integer value must be specified for \code{p}. 
  }
}
\details{
The distances based on Lp norms are computed between
two numeric vectors using the following formulas: 

Euclidean distance: \eqn{\sqrt{(x_i-y_i)^2)}} 

Manhattan distance: \eqn{\sum{|x_i-y_i|}} 

Infinite norm distance: \eqn{\max{|x_i-y_i|}} 

Minkowski distance: \eqn{\sqrt[p]{(x_i-y_i)^p)}} 


The two series must have the same length. Furthermore, in the case of the Minkowski distance, \code{p} must be specified as a positive integer value.

%\bold{Note:} These distance measures are all metrics. This means that they can be used directly within kernel machines, for example by inserting them in the Gaussian RBF kernel (Lei and Sun, 2007), because they will provide positive definite Gram matrices. 


}

\value{
\item{d}{
 The computed distance between the pair of series.
}
}

\author{
Usue Mori, Alexander Mendiburu, Jose A. Lozano.
}


\seealso{

These distances are also implemeted in separate functions. For more information see \code{\link{EuclideanDistance}}, \code{\link{ManhattanDistance}}, \code{\link{MinkowskiDistance}}
and \code{\link{InfNormDistance}}

To calculate this distance measure using \code{ts}, \code{zoo} or \code{xts} objects see \code{\link{TSDistances}}. To calculate distance matrices of time series databases using this measure see \code{\link{TSDatabaseDistances}}.
}

\examples{

# The objects example.series1 and example.series2 are two 
# numeric series of length 100 contained in the TSdist package. 

data(example.series1)
data(example.series2)

# For information on their generation and shape see help 
# page of example.series.

help(example.series)

# Compute the different Lp distances

# Euclidean distance

LPDistance(example.series1, example.series2, method="euclidean")

# Manhattan distance

LPDistance(example.series1, example.series2, method="manhattan")

# Infinite norm distance

LPDistance(example.series1, example.series2, method="infnorm")

# Minkowski distance with p=3.

LPDistance(example.series1, example.series2, method="minkowski", p=3)

}
