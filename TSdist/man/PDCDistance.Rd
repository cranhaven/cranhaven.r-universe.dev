\name{PDCDistance}
\alias{PDCDistance}

\title{
Permutation Distribution Distance
}
\description{
Calculates the permutation distribution distance between two numerical series of
the same length.
}

\usage{
PDCDistance(x, y, ...)
}

\arguments{
  \item{x}{
  Numeric vector containing the first time series.
}

  \item{y}{
  Numeric vector containing the second time series.
}
  \item{...}{
   Additional parameters for the function. See  \code{\link[pdc]{pdcDist}} for more 
  information.
}
}

\details{
This is simply a wrapper for the \code{\link[pdc]{pdcDist}} function of package \pkg{pdc}. As such, all the functionalities of the \code{\link[pdc]{pdcDist}} function are also available when using this function. 

%\bold{Note:} This distance measure is a metric (Brandmaier, 2015). This means that it will provide positive definite Gram matrices and, thus, it can be used directly within kernel machines, e.g. by inserting it in the Gaussian RBF kernel (Lei and Sun, 2007).

}

\value{
\item{d}{
  The computed distance between the pair of series.
}


}

\references{

Andreas M. Brandmaier (2015). pdc: An R package for Complexity-Based Clustering
of Time Series. Journal of Statistical Software, Vol 67, Issue 5.
  
Pablo Montero, José A. Vilar (2014). TSclust: An R Package for Time Series
  Clustering. Journal of Statistical Software, 62(1), 1-43. URL
  http://www.jstatsoft.org/v62/i01/.
}

\author{
Usue Mori, Alexander Mendiburu, Jose A. Lozano.
}



\seealso{

To calculate this distance measure using \code{ts}, \code{zoo} or \code{xts} objects see \code{\link{TSDistances}}. To calculate distance matrices of time series databases using this measure see \code{\link{TSDatabaseDistances}}.

}

\examples{

# The objects example.series1 and example.series2 are two 
# numeric series of length 100. 

data(example.series1)
data(example.series2)

# For information on their generation and shape see 
# help page of example.series.

help(example.series)

# Calculate the normalized compression based distance between the two series 
# using the default parameters.

PDCDistance(example.series1, example.series2)

}
