\name{IntPerDistance}
\alias{IntPerDistance}

\title{
Integrated Periodogram based dissimilarity
}
\description{
Calculates the dissimilarity between two numerical series of the same length based on the distance between their integrated periodograms.
}

\usage{
IntPerDistance(x, y, ...)
}

\arguments{
  \item{x}{
  Numeric vector containing the first time series.
}

  \item{y}{
  Numeric vector containing the second time series.
}

  \item{...}{
   Additional parameters for the function. See  \code{\link[TSclust]{diss.INT.PER}} for more 
  information.
}
}

\details{
This is simply a wrapper for the \code{\link[TSclust]{diss.INT.PER}} function of package \pkg{TSclust}. As such, all the functionalities of the \code{\link[TSclust]{diss.INT.PER}} function are also available when using this function. 

%\bold{Note:} This distance measure is based on the Euclidean distance. This means that it can be used directly within kernel machines, e.g. by inserting it in the Gaussian RBF kernel, because it will provide positive definite Gram matrices. . 
}

\value{
\item{d}{
  The computed distance between the pair of series.
}


}

\references{
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

# Calculate the ar.mah distance between the two series using
# the default parameters. 

IntPerDistance(example.series1, example.series2)

}
