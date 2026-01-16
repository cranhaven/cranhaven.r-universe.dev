\name{ARLPCCepsDistance}
\alias{ARLPCCepsDistance}

\title{
Dissimilarity Based on LPC Cepstral Coefficients
}
\description{
Computes the dissimilarity between two numeric time series in terms of their Linear Predictive Coding (LPC) ARIMA processes.
}

\usage{
ARLPCCepsDistance(x, y, ...)
}

\arguments{
  \item{x}{
  Numeric vector containing the first time series.
}

  \item{y}{
  Numeric vector containing the second time series.
}

  \item{...}{
   Additional parameters for the function. See  \code{\link[TSclust]{diss.AR.LPC.CEPS}} for more 
  information.
}
}

\details{
This is simply a wrapper for the \code{\link[TSclust]{diss.AR.LPC.CEPS}} function of package \pkg{TSclust}. As such, all the functionalities of the \code{\link[TSclust]{diss.AR.LPC.CEPS}} function are also available when using this function. 

%\bold{Note:} This distance measure is based on the Euclidean distance, hence, it can be used directly within kernel machines, e.g. by inserting it in the Gaussian RBF kernel, because it will provide positive definite Gram matrices.

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

# The objects example.series3 and example.series4 are two 
# numeric series of length 100 and 120 contained in the 
# TSdist package obtained from an ARIMA(3,0,2) process. 

data(example.series3)
data(example.series4)

# For information on their generation and shape see 
# help page of example.series.

help(example.series)

# Calculate the ar.lpc.ceps distance between the two series using
# the default parameters. In this case an AR model is automatically 
# selected for each of the series:

ARLPCCepsDistance(example.series3, example.series4)

# Calculate the ar.lpc.ceps distance between the two series
# imposing the order of the ARIMA model of each series:

ARLPCCepsDistance(example.series3, example.series4, order.x=c(3,0,2), 
order.y=c(3,0,2))

}
