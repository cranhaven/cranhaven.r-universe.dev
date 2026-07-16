\name{getHDmembers}
\alias{getHDmembers}
\title{
     Partitioning Stage of the \emph{hdoutliers} Algorithm
}
\description{
Implements the first stage of the \emph{hdoutliers} Algorithm, in which the
data is partitioned according to \emph{exemplars}
and their associated lists of \emph{members}.
}

\usage{
getHDmembers(data, maxrows = 10000, radius = NULL) 
}

\arguments{
  \item{data}{
    A vector, matrix, or data frame consisting of numeric and/or categorical
    variables.
  }
 \item{maxrows}{
    If the number of observations is greater than \code{maxrows},
    \code{HDoutliers} reduces the number used in nearest-neighbor 
    computations to a set of \emph{exemplars}. The default value is 10000.
  }
  \item{radius}{
    Threshold for determining membership in the \emph{exemplars}'s lists
   (used only when the number of observations is greater than \code{maxrows}).
    An observation is added to an \emph{exemplars}' list if its distance
    to that \emph{exemplar} is less than \code{radius}.
    The default value is \eqn{.1/(log n)^(1/p)}, where \eqn{n} is the 
    number of observations and \eqn{p} is the dimension of the data.
  }
}

\details{
  If the number of observations exceeds \code{maxrows}, the data is 
  partitioned into lists corresponding to \emph{exemplars} 
  and their \emph{members} within \code{radius} of each \emph{exemplar},
  to reduce the number of nearest-neighbor computations required for
  outlier detection. \cr
  When there are fewer observations, the result is a list whose elements are 
  the individual observations (each observation is an exemplar, with  no
  other members). 
}

\value{
   A list in which each component is a vector of observation indexes.
   The first index in each list is the index of the \emph{exemplar}
   defining that list, and any remaining indexes are the
   associated \emph{members}, within \code{radius} of the \emph{exemplar}.
}

\section{References}{
  Wilkinson, L. (2016). Visualizing Outliers.
<https://www.cs.uic.edu/~wilkinson/Publications/outliers.pdf>.
}
\seealso{
  \code{\link{HDoutliers}},
  \code{\link{getHDoutliers}}
}

\examples{

data(dots)
mem.W <- getHDmembers(dots$W)
out.W <- getHDoutliers(dots$W,mem.W)

data(ex2D)
mem.ex2D <- getHDmembers(ex2D)
out.ex2D <- getHDoutliers(ex2D,mem.ex2D)

\dontrun{
n <- 100000 # number of observations
set.seed(3)
x <- matrix(rnorm(2*n),n,2)
nout <- 10 # number of outliers
x[sample(1:n,size=nout),] <- 10*runif(2*nout,min=-1,max=1)

mem.x <- getHDmembers(x)
out.x <- getHDoutliers(x,mem.x)}

}

\keyword{cluster}
