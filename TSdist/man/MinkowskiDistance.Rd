\name{MinkowskiDistance}
\alias{MinkowskiDistance}

\title{
Minkowski distance. 
}
\description{
Computes the Minkowski distance between two numeric vectors for a given p.
}

\usage{
MinkowskiDistance(x, y, p)
}

\arguments{
  \item{x}{
  Numeric vector containing the first time series.
}

  \item{y}{
  Numeric vector containing the second time series.
}
\item{p}{
    A strictly positive integer value that defines the chosen \eqn{L_p} norm.
}
}

\details{
The Minkowski distance is computed between the two numeric series using the following formula: 

\deqn{D=\sqrt[p]{(x_i-y_i)^p)}} 

The two series must have the same length and \code{p} must be a positive integer value. This distance is calculated with the help of the \code{\link[proxy]{dist}} function of the \code{proxy} package.

%\bold{Note:} This distance measure is a metric. This means that it will provide positive definite Gram matrices and, thus, it can be used directly within kernel machines, e.g. by inserting it in the Gaussian RBF kernel (Lei and Sun, 2007).

}

\value{
\item{d}{
  The computed distance between the pair of series.
}
}

\references{
David Meyer and Christian Buchta (2015). proxy: Distance and Similarity Measures. R package
  version 0.4-14. http://CRAN.R-project.org/package=proxy

}

\author{
Usue Mori, Alexander Mendiburu, Jose A. Lozano.
}


\seealso{
This function can also be invoked by the wrapper function \code{\link{LPDistance}}.

Furthermore, to calculate this distance measure using \code{ts}, \code{zoo} or \code{xts} objects see \code{\link{TSDistances}}. To calculate distance matrices of time series databases using this measure see \code{\link{TSDatabaseDistances}}.
}

\examples{

# The objects example.series1 and example.series2 are two 
# numeric series of length 100 contained in the TSdist package. 

data(example.series1)
data(example.series2)

# For information on their generation and shape see help 
# page of example.series.

help(example.series)


# Compute the Minkowski distance between them:

MinkowskiDistance(example.series1, example.series2, p=3)
}