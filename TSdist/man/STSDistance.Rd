\name{STSDistance}
\alias{STSDistance}

\title{
Short time series distance (STS). 
}
\description{
Computes the Short Time Series Distance between a pair of 
numeric time series.
}
\usage{
STSDistance(x, y, tx, ty)
}
\arguments{
  \item{x}{
  Numeric vector containing the first time series.
}
  \item{y}{
  Numeric vector containing the second time series.
}
  \item{tx}{
  If not constant, a numeric vector that specifies the sampling index of series \code{x}.
}

\item{ty}{
  If not constant, a numeric vector that specifies the sampling index of series \code{y}.
}
}
\details{
The short time series distance between two series is designed specially for series with an equal but uneven sampling rate. However, it can also be used for time series with a constant sampling rate. It is calculated as follows:

\deqn{STS= \sqrt{\sum_{k=\{1,...,N-1\}} (((y_{k+1} - y_{k}) / (tx_{k+1} - tx_{k}) - (x_{k+1} - x_{k}) / (ty_{k+1} - ty_{k}))^2)}}

where \eqn{N} is the length of series \eqn{x} and \eqn{y}. 

\code{tx} and \code{ty} must be positive and strictly increasing. Furthermore, the sampling rate in both indexes must be equal: 

\deqn{tx[k+1]-tx[k]=ty[k+1]-ty[k], \, \, \, for \, \, \, k=0 ,..., N-1}

%\bold{Note:} This distance measure is a metric (\enc{Möller}{Moller}-Levet et al., 2003). This means that it will provide positive definite Gram matrices and, thus, it can be used directly within kernel machines, for example by inserting it in the Gaussian RBF kernel (Lei and Sun, 2007).

}

\value{
\item{d}{
  The computed distance between the pair of series.
}

}

\references{
\enc{Möller}{Moller}-Levet, C. S., Klawonn, F., Cho, K., & Wolkenhauer, O. (2003). \emph{Fuzzy Clustering of Short Time-Series and Unevenly Distributed Sampling Points}. In Proceedings of the 5th International Symposium on Intelligent Data Analysis.

}

\author{
Usue Mori, Alexander Mendiburu, Jose A. Lozano.

}



\seealso{
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

# Calculate the STS distance assuming even sampling:

STSDistance(example.series1, example.series2)

# Calculate the STS distance providing an uneven sampling:

tx<-unique(c(seq(2, 175, 2), seq(7, 175, 7)))
tx <- tx[order(tx)]
ty <- tx
STSDistance(example.series1, example.series2, tx, ty)



}

