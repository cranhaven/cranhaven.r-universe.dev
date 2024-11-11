\name{svdGP}
\alias{svdGP}

\title{
  Full SVD-Based GP Models}
\description{
  This function fits a full SVD-based GP model with test set \code{X0},
  design set \code{design} and response matrix \code{resp}.
}
\usage{
svdGP(design,resp,X0=design,nstarts=5,gstart=0.0001,
      frac=.95,centralize=FALSE,nthread=1,clutype="PSOCK")
}
\arguments{
    \item{design}{ An \eqn{N} by \eqn{d} matrix of \eqn{N} training/design
    inputs.}

  \item{resp}{ An \eqn{L} by \eqn{N} response matrix of \code{design},
    where \eqn{L} is the length of the time series outputs, \eqn{N} is
    the number of design points.}

  \item{X0}{ An \eqn{M} by \eqn{d} matrix of \eqn{M} test inputs. The
    default value of \code{X0} is \code{design}.}

  \item{nstarts}{
    The number of starting points used in the numerical maximization of
    the posterior density function. The larger \code{nstarts} will
    typically lead to more accurate prediction but longer computational
    time. The default value is 5.
  }

  \item{gstart}{
    The starting number and upper bound for estimating the nugget
    parameter. If \code{gstart = sqrt(.Machine$double.eps)}, the nugget
    parameter will be fixed at \code{sqrt(.Machine$double.eps)}, since
    \code{sqrt(.Machine$double.eps)} is the lower bound of the nugget
    term. The default value is 0.0001.
  }

  \item{frac}{
    The threshold in the cumulative percentage criterion to select the
    number of SVD bases. The default value is 0.95.
  }
  \item{centralize}{
    If \code{centralize=TRUE} the response matrix will be centralized
    (subtract the mean) before the start of the algorithm. The mean will
    be added to the predictive mean at the finish of the algorithm. The
    default value is \code{FALSE}.
  }
  \item{nthread}{
    The number of threads (processes) used in parallel execution of this
    function. \code{nthread=1} implies no parallelization. The default
    value is 1.
  }
  \item{clutype}{
    The type of cluster in the R package "parallel" to perform
    parallelization. The default value is "PSOCK". Required only if
    \code{nthread}>1.
  }
}
\value{
  \item{pmean}{
    An \eqn{L} by \eqn{M} matrix of posterior predicted mean for the response at
    the test set \code{X0}.
  }
  \item{ps2}{
    An \eqn{L} by \eqn{M} matrix of posterior predicted variance for the response at
    the test set \code{X0}.
  }
}
\author{Ru Zhang \email{heavenmarshal@gmail.com},

  C. Devon Lin \email{devon.lin@queensu.ca},

  Pritam Ranjan \email{pritamr@iimidr.ac.in}}
\seealso{\code{\link{knnsvdGP}}, \code{\link{lasvdGP}}.}
\examples{
library("lhs")
forretal <- function(x,t,shift=1)
{
    par1 <- x[1]*6+4
    par2 <- x[2]*16+4
    par3 <- x[3]*6+1
    t <- t+shift
    y <- (par1*t-2)^2*sin(par2*t-par3)
}
timepoints <- seq(0,1,len=200)
design <- lhs::randomLHS(50,3)
test <- lhs::randomLHS(50,3)

## evaluate the response matrix on the design matrix
resp <- apply(design,1,forretal,timepoints)

## fit full SVD-based GP model
ret <- svdGP(design,resp,test,frac=.95,nstarts=1,
             centralize=TRUE,nthread=2)
}
\keyword{GP model}
\keyword{SVD}
\keyword{prediction}
