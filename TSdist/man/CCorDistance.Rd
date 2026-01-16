\name{CCorDistance}
\alias{CCorDistance}

\title{
Cross-correlation based distance.
}

\description{
Computes the distance measure based on the cross-correlation between a pair of
numeric time series.
}

\usage{
CCorDistance(x, y, lag.max=(min(length(x), length(y))-1))
}

\arguments{
  \item{x}{
  Numeric vector containing the first time series.
}

  \item{y}{
  Numeric vector containing the second time series.
}

  \item{lag.max}{
  Positive integer that defines the maximum lag considered in the 
  cross-correlation calculations (default=\code{min(length(x), length(y))-1}).
}
}

\details{
The cross-correlation based distance between two numeric time series
is calculated as follows:

\deqn{D= \sqrt{ ((1 - CC(x, y, 0) ^ 2) / \sum (1 - CC(x, y, k) ^ 2)) }}


 where \eqn{CC(x,y,k)} is the cross-correlation between \eqn{x} and \eqn{y} 
 at lag \eqn{k}.
 
 The summatory in the denominator goes from 1 to \code{lag.max}. In view of this, the parameter must be a positive integer no larger than the length of the series.

%\bold{Note:} The negative definiteness of this distance measure is not explicitly mentioned in the literature, to the best of our knowledge, and so can not be assured. As such, when using it within kernel based classifiers such as Support Vector Machines or Gaussian Processes (i.e by inserting this distance in the Gaussian RBF kernel) the user should make sure that the obtained Gram matrix is positive semi-definite. More information and references to some solutions to this problem can be found in (Pree et al. 2014).
}

\value{
\item{d}{
  The computed distance between the pair of series.
}
}

\references{
Liao, T. W. (2005). \emph{Clustering of time series data-a survey}. Pattern Recognition, 38(11), 1857-1874.

Pree, H., Herwig, B., Gruber, T., Sick, B., David, K., & Lukowicz, P. (2014). On general purpose time series similarity measures and their use as kernel functions in support vector machines. Information Sciences, 281, 478–495.
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
# TSdist package. 

data(example.series3)
data(example.series4)

# For information on their generation and shape see 
# help page of example.series.

help(example.series)

# Calculate the cross-correlation based distance 
# using the default lag.max.

CCorDistance(example.series3, example.series4)

# Calculate the cross-correlaion based distance 
# with lag.max=50.

CCorDistance(example.series3, example.series4, lag.max=50)

}
