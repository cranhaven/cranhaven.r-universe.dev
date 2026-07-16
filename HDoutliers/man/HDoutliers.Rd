\name{HDoutliers}
\alias{HDoutliers}
\title{
  Leland Wilkinson's \emph{hdoutliers} Algorithm for Outlier Detection
}
\description{
Detects outliers based on a probability model.
}

\usage{
HDoutliers(data, maxrows=10000, radius=NULL, alpha=0.05, transform=TRUE) 
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
    (used only when the number of observations is greater than \eqn{maxrows}).
    An observation is added to an \emph{exemplars}' lists if its distance
    to that \emph{exemplar} is less than \code{radius}.
    The default value is \eqn{.1/(log n)^(1/p)}, where \eqn{n} is the 
    number of observations and \eqn{p} is the dimension of the data.
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
   }
}

\details{
  Wilkinson replaces categorical variables with the leading component from
  correspondence analysis, and maps the data to the unit square. This is
  done as a preprocessing step if \code{transform = TRUE} (the default). \cr
  If the number of observations exceeds \code{maxrows}, 
  the data is first partitioned into lists associated with \emph{exemplars} 
  and their \emph{members} within \code{radius} of each \emph{exemplar},
  to reduce the number of nearest-neighbor computations required for
  outlier detection.\cr
  An exponential distribution is then fitted to the upper tail of the 
  nearest-neighbor distances between \emph{exemplars}. 
  Observations are considered
  outliers if they fall in the \eqn{(1- alpha)} tail of the fitted CDF.
}

\value{
  The indexes of the observations determined to be outliers.
}

\section{References}{
  Wilkinson, L. (2016). Visualizing Outliers.
}
\seealso{
  \code{\link{getHDmembers}},
  \code{\link{getHDoutliers}},
  \code{\link{dataTrans}}
}

\examples{

data(dots)
out.W <- HDoutliers(dots$W)
\dontrun{
plotHDoutliers(dots$W,out.W)}

data(ex2D)
out.ex2D <- HDoutliers(ex2D)
\dontrun{
plotHDoutliers(ex2D,out.ex2D)}

\dontrun{
n <- 100000 # number of observations
set.seed(3)
x <- matrix(rnorm(2*n),n,2)
nout <- 10 # number of outliers
x[sample(1:n,size=nout),] <- 10*runif(2*nout,min=-1,max=1)

out.x <- HDoutliers(x)}
}

\keyword{cluster}
