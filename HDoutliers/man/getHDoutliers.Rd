\name{getHDoutliers}
\alias{getHDoutliers}
\title{
  Outlier Detection Stage of Wilkinson's \emph{hdoutliers} Algorithm
}
\description{
Detects outliers based on a probability model.
}

\usage{
getHDoutliers(data, memberLists, alpha = 0.05, transform = TRUE) 
}

\arguments{
  \item{data}{
    A vector, matrix, or data frame consisting of numeric and/or categorical
    variables.
  }
  \item{memberLists}{
   A list following the structure of the output to \code{getHDmembers},
   in which each component is a vector of observation indexes.
   The first index in each list is the index of the \emph{exemplar}
   representing that list, and any remaining indexes are the
   associated \emph{members}, considered `close to' the \emph{exemplar}.
  }
  \item{alpha}{
    Threshold for determining the cutoff for outliers.
    Observations are considered outliers
    outliers if they fall in the \eqn{(1- alpha)} tail of the distribution
    of the nearest-neighbor distances between \emph{exemplars}.
  }
 \item{transform}{
     A logical variable indicating whether or not the data needs to be
     transformed to conform to Wilkinson's specifications before outlier
     detection. The default is to transform the data using function
     \code{dataTrans}.
     In Wilksinson's algorithm, \code{memberLists} would have been created 
     with transformed data.
   }
}
\details{
  An exponential distribution is fitted to the upper tail of the 
  nearest-neighbor distances between \emph{exemplars} (the observations
  considered representatives of each component of \code{memberLists}). 
  Observations are considered
  outliers if they fall in the \eqn{(1- alpha)} tail of the fitted CDF.
}

\value{
  The indexes of the observations determined to be outliers.
}

\note{
A call to \code{getHDoutliers} in which \code{membersLists} result from
a call to \code{getHDmembers} is equivalent to calling \code{HDoutliers}.
}
\section{References}{
  Wilkinson, L. (2016). Visualizing Outliers.
<https://www.cs.uic.edu/~wilkinson/Publications/outliers.pdf>.
}
\seealso{
  \code{\link{HDoutliers}},
  \code{\link{getHDmembers}},
  \code{\link{dataTrans}}
}

\examples{

data(dots)
mem.W <- getHDmembers(dots$W)
out.W <- getHDoutliers(dots$W,mem.W)
\dontrun{
plotHDoutliers( dots.W, out.W)}

data(ex2D)
mem.ex2D <- getHDmembers(ex2D)
out.ex2D <- getHDoutliers( ex2D, mem.ex2D)
\dontrun{
plotHDoutliers( ex2D, out.ex2D)}

\dontrun{
n <- 100000 # number of observations
set.seed(3)
x <- matrix(rnorm(2*n),n,2)
nout <- 10 # number of outliers
x[sample(1:n,size=nout),] <- 10*runif(2*nout,min=-1,max=1)

mem.x <- getHDmembers(x)
out.x <- getHDoutliers(x)}

}

\keyword{cluster}
