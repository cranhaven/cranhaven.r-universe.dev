\name{ERPDistance}
\alias{ERPDistance}

\title{
Edit Distance with Real Penalty (ERP). 
}
\description{
Computes the Edit Distance with Real Penalty between a pair of 
numeric time series.
}
\usage{
ERPDistance(x, y, g, sigma)
}

\arguments{
  \item{x}{
  Numeric vector containing the first time series.
}
  \item{y}{
  Numeric vector containing the second time series.
}

\item{g}{
  The reference value used to penalize gaps.
}
\item{sigma}{
  If desired, a Sakoe-Chiba windowing contraint can be added by specifying 
  a positive integer representing the window size.
}

}
\details{
The basic Edit Distance with Real Penalty between two numeric series is calculated. Unlike other edit based distances included in this package, this distance is a metric and fulfills the triangle inequality.

The idea is to search for the minimal path in a distance matrix that describes the mapping between the two series. This distance matrix is built by using the Euclidean distance. However, unlike DTW, this distance permits gaps or sequences of points that are not matched with any other point. These gaps will be penalized  based on the distance of the unmatched points from a reference value \eqn{g}.

As with other edit based distances, the length of \code{x} and \code{y} may be different.

Furthermore, if desired, a temporal constraint may be added to the ERP
distance. In this package, only the most basic windowing function, introduced 
by H.Sakoe and S.Chiba (1978), is implemented. This function sets a band around the main diagonal of the distance matrix and avoids the matching of the points that are farther in time than a specified \eqn{\sigma}. 

The size of the window must be a positive integer value. Furthermore, the following condition must be fulfilled:

\deqn{|length(x)-length(y)| < sigma}


%\bold{Note:} This distance measure is a metric (Chen and Ng, 2004). This means that it will provide positive definite Gram matrices and, thus, it can be used directly within kernel machines, for example by inserting it in the Gaussian RBF kernel (Lei and Sun, 2007).


}

\value{
\item{d}{
  The computed distance between the pair of series.
}

}

\references{
Chen, L., & Ng, R. (2004). \emph{On The Marriage of Lp-norms and Edit Distance}. In Proceedings of the Thirtieth International Conference on Very Large Data Bases (pp. 792-803).

}

\author{
Usue Mori, Alexander Mendiburu, Jose A. Lozano.
}



\seealso{
To calculate this distance measure using \code{ts}, \code{zoo} or \code{xts} objects see \code{\link{TSDistances}}. To calculate distance matrices of time series databases using this measure see \code{\link{TSDatabaseDistances}}.
}
\examples{

#The objects example.series3 and example.series4 are two 
#numeric series of length 100 and 120 contained in the TSdist  
#package. 

data(example.series3)
data(example.series4)

#For information on their generation and shape see 
#help page of example.series.

help(example.series)

#Calculate the ERP distance for two series of different length
#with no windowing constraint:

ERPDistance(example.series3, example.series4, g=0)

#Calculate the ERP distance for two series of different length
#with a window of size 30:

ERPDistance(example.series3, example.series4, g=0, sigma=30)

}
