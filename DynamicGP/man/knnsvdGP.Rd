\name{knnsvdGP}
\alias{knnsvdGP}

\title{K-nearest neighbor SVD-Based GP model}

\description{Fits a K-nearest neighbour SVD-based GP model on a test set
  \code{X0}, training set \code{design} and response matrix \code{resp}. The
  local neighbourhood sets consist of \code{nn} points which are selected
  by the Euclidean distance with respect to the test points. See Zhang et
  al. (2018) for details.This function supports the
  parallelization via both the R packages "parallel" and the OpenMP
  library.}

\usage{
knnsvdGP(design,resp, X0=design, nn=20, nsvd = nn, frac = .95,
         gstart = 0.0001, nstarts = 5,centralize=FALSE, maxit=100,
         errlog = "", nthread = 1, clutype="PSOCK")
}

\arguments{
  \item{design}{ An \eqn{N} by \eqn{d} matrix of \eqn{N} training/design
    inputs.}

  \item{resp}{ An \eqn{L} by \eqn{N} response matrix of \code{design},
    where \eqn{L} is the length of the time series outputs, \eqn{N} is
    the number of design points.}

  \item{X0}{ An \eqn{M} by \eqn{d} matrix of \eqn{M} test inputs. The
    localized SVD-based GP models will be fitted on every point (row) of
    \code{X0}. The default value of \code{X0} is \code{design}.}

  \item{nn}{
    The number of neighborhood points selected by the Euclidean
    distance. the default value is 20.
  }
  \item{nsvd}{
    The number of design points closest to the test points on whose
    response matrix to perform the initial singular value
    decomposition. The default value is \code{nn}.
  }
  \item{frac}{
    The threshold in the cumulative percentage criterion to select the
    number of SVD bases. The default value is 0.95.
  }
  \item{gstart}{
    The starting number and upper bound for estimating the nugget
    parameter. If \code{gstart = sqrt(.Machine$double.eps)}, the nugget
    parameter will be fixed at \code{sqrt(.Machine$double.eps)}, since
    \code{sqrt(.Machine$double.eps)} is the lower bound of the nugget
    term. The default value is 0.0001.
  }
  \item{nstarts}{
    The number of starting points used in the numerical maximization of
    the posterior density function. The larger \code{nstarts} will
    typically lead to more accurate prediction but longer computational
    time. The default value is 5.
  }
  \item{centralize}{
    If \code{centralize=TRUE} the response matrix will be centralized
    (subtract the mean) before the start of the algorithm. The mean will
    be added to the predictive mean at the finish of the algorithm. The
    default value is \code{FALSE}.
  }
  \item{maxit}{
    Maximum number of iterations in the numerical optimization algorithm
    for maximizing the posterior density function. The default value is
    100.
  }
  \item{errlog}{
    The path of a log file that records the errors occur in the process of fitting
    local SVD-based GP models. If an empty string is provided, no log file will be
    produced.
  }
  \item{nthread}{
    The number of threads (processes) used in parallel execution of this
    function. \code{nthread=1} implies no parallelization. The default
    value is 1.
  }
  \item{clutype}{
    The type of parallization utilized by this function. If \code{clutype="OMP"},
    it will use the OpenMP parallelization. Otherwise, it indicates the
    type of cluster in the R package "parallel" . The default value is "PSOCK".
    Required only if \code{nthread}>1.
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
  \item{flags}{
    A vector of integers of length \eqn{M} which indicates the status for fitting the
    local SVD-based GP models for each of the \eqn{M} input points in the test set.
    The value \eqn{0} indicates successful fitting, the value \eqn{1} indicates an
    error in Cholesky decomposition of the correlation matrices, the value \eqn{2}
    indicates an error in SVD of the local response matrix, the value \eqn{3} indicates
    an error in optimizing the nugget term.
  }
}

\author{Ru Zhang \email{heavenmarshal@gmail.com},

  C. Devon Lin \email{devon.lin@queensu.ca},

  Pritam Ranjan \email{pritamr@iimidr.ac.in}}

\seealso{\code{\link{lasvdGP}}, \code{\link{svdGP}}.}

\references{Zhang, R., Lin, C. D. and Ranjan, P. (2018) \emph{Local Gaussian
    Process Model for Large-scale Dynamic Computer Experiments},
  Journal of Computational and Graphical Statistics, \cr DOI:
  10.1080/10618600.2018.1473778.}

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
design <- lhs::randomLHS(100,3)
test <- lhs::randomLHS(20,3)

## evaluate the response matrix on the design matrix
resp <- apply(design,1,forretal,timepoints)

nn <- 15
gs <- sqrt(.Machine$double.eps)

## knnsvdGP with mutiple (5) start points for GP model estimation
## It use the R package "parallel" for parallelization
retknnmsp <- knnsvdGP(design,resp,test,nn,frac=.95,gstart=gs,
                      centralize=TRUE,nstarts=5,nthread=2,clutype="PSOCK")

## knnsvdGP with single start point for GP model estimation
## It does not use parallel computation
retknnss <- knnsvdGP(design,resp,test,nn,frac=.95,gstart=gs,
                     centralize=TRUE,nstarts=1,nthread=1)
}
\keyword{GP model}
\keyword{SVD}
\keyword{neighborhood}
\keyword{prediction}
