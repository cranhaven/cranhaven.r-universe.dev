\name{lasvdGP}
\alias{lasvdGP}

\title{
  Local Approximate SVD-Based GP Models
}

\description{Fits a local approximate SVD-based GP model on a test set
  \code{X0}, training/design set \code{design} and response matrix
  \code{resp}. The local neighborhood sets consist of \code{nn} out of
  which \code{n0} points are selected by the Euclidean distance with
  respect to the test points. The remaining \code{nn}-\code{n0}
  neighborhood points are selected sequentially by a greedy algorithm
  proposed by Zhang et al. (2018). This function supports the
  parallelization via both the R packages "parallel" and the OpenMP
  library.}

\usage{
lasvdGP(design, resp, X0=design, n0=10, nn=20,
        nfea = min(1000,nrow(design)),
        nsvd = nn, nadd = 1, frac = .95, gstart = 0.0001,
        resvdThres = min(5, nn-n0), every = min(5,nn-n0),
        nstarts = 5,centralize=FALSE, maxit=100,
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

  \item{n0}{
    The number of points in the initial neighborhood set. The initial
    neighborhood set is selected by the Euclidean distance. The default
    value is 10.}

  \item{nn}{
    The total number of neighborhood points. The \code{nn-n0} points are
    selected sequentially by the proposed algorithm. The default value
    is 20.
  }
  \item{nfea}{
    The number of feasible points within which to select the
    neighborhood points. This function will only consider the
    \code{nfea} design points closest to the test point in terms of
    Euclidean distance when selecting neighborhood points. The default
    value is the minimum of \eqn{N} and 1000.
  }
  \item{nsvd}{
    The number of design points closest to the test points on whose
    response matrix to perform the initial singular value
    decomposition. The default value is \code{nn}.
  }

  \item{nadd}{
    The number of neighborhood points selected at one iteration. The
    default value is 1.
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

  \item{resvdThres}{
    The threshold to re-perform SVD. After every \code{resvdThres}
    points have been included into the neighborhood set, the SVD of the
    response matrix will be re-performed and the SVD-based GP model will
    be refitted. The default value is the minimum of \code{nn}-\code{n0}
    and 5.
  }
  \item{every}{
    The threshold to refit GP models without re-perform SVD. After every
    \code{every} points have been included into the neighborhood set,
    the GP models will be refitted. But the SVD will not be
    re-performed. It is suggested \code{every} <= \code{resvdThres}. The
    default value is the minimum of \code{nn}-\code{n0} and 5.
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

\seealso{\code{\link{knnsvdGP}}, \code{\link{svdGP}}.}

\references{Zhang, R., Lin, C. D. and Ranjan, P. (2018) \emph{Local Gaussian
    Process Model for Large-scale Dynamic Computer Experiments},
  Journal of Computational and Graphical Statistics, \cr DOI:
  10.1080/10618600.2018.1473778.
}

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

n0 <- 14
nn <- 15
gs <- sqrt(.Machine$double.eps)

## lasvdGP with mutiple (5) start points for GP model estimation,
## It use the R package "parallel" for parallelization
retlamsp <- lasvdGP(design,resp,test,n0,nn,frac=.95,gstart=gs,
                    centralize=TRUE,nstarts=5,nthread=2,clutype="PSOCK")

## lasvdGP with single start point for GP model estimation,
## It does not use parallel computation
retlass <- lasvdGP(design,resp,test,n0,nn,frac=.95,gstart=gs,
                   centralize=TRUE,nstarts=1,nthread=1)

}
\keyword{GP model}
\keyword{SVD}
\keyword{neighborhood}
\keyword{prediction}
