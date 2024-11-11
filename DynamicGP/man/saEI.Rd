\name{saEI}
\alias{saEI}

\title{Saddlepoint Approximate Expected Improvement Criterion for
  the Sequential Design for Inverse Problems}

\description{This function performs the sequential design procedure for
  the inverse problem. It starts from an initial design set \code{xi}
  and selects the follow-up design points from the candidate set
  \code{candei} as per the expected improvement (EI) criterion which is
  numerically approximated by the saddlepoint approximation technique in
  Huang and Oosterlee (2011). The surrogate is refitted using the
  augmented data via \code{svdGP}. After the selection of \code{nadd}
  follow-up points, the solution of the inverse problem is estimated
  either by the \code{ESL2D} approach or by the \code{SL2D}
  approach. Details are provided in Chapter 4 of Zhang (2018).  }

\usage{
saEI(xi,yi,yobs,nadd,candei,candest,func,...,
     mtype=c("zmean","cmean","lmean"),
     estsol=c("ESL2D","SL2D"),
     frac=.95, nstarts=5, gstart=0.0001,
     nthread=1, clutype="PSOCK")
}

\arguments{
  \item{xi}{ An \code{N0} by \eqn{d} matrix of \code{N0} initial design
  points.  }

  \item{yi}{ An \eqn{L} by \code{N0} response matrix of \code{xi},
    where \eqn{L} is the length of the time series outputs, \code{N0} is
    the number of design points.}

  \item{yobs}{ A vector of length \eqn{L} of the time-series valued
    field observations or the target response.}

  \item{nadd}{ The number of the follow-up design points selected by
    this function.}

  \item{candei}{An \code{M1} by \eqn{d} matrix of \code{M1} candidate points
    on which the follow-up design points are selected.}

  \item{candest}{ An \code{M2} by \eqn{d} matrix of \code{M2} candidate points
    on which the (final) estimated solution to the inverse problem is extracted.
  }

  \item{func}{ An R function of the dynamic computer simulator. The
  first argument of \code{func} should be a vector of \eqn{d}-dimensional
  inputs. The simulator \code{func} should return a vector of length
  \eqn{L} as the output.}

\item{...}{ The remaining arguments of the simulator \code{func}.}

\item{mtype}{ The type of mean functions for the GP models. The choice
    "zmean" denotes zero-mean, "cmean" indicates constant-mean, "lmean" indicates
    linear-mean. The default choice is "zmean".}

  \item{estsol}{ The method for estimating the final solution to the inverse
    problem after all follow-up design points are included, "ESL2D"
    denotes the ESL2D approach, "SL2D" denotes the SL2D approach. The
    default choice is "ESL2D".}

  \item{frac}{ The threshold in the cumulative percentage criterion to select the
    number of SVD bases. The default value is 0.95.
  }

  \item{nstarts}{
    The number of starting points used in the numerical maximization of
    the posterior density function. The larger \code{nstarts} will
    typically lead to more accurate prediction but longer computational
    time. The default value is 5.
  }

  \item{gstart}{ The starting number and upper bound for estimating the
    nugget parameter. If \code{gstart = sqrt(.Machine$double.eps)}, the
    nugget parameter will be fixed at \code{sqrt(.Machine$double.eps)},
    since \code{sqrt(.Machine$double.eps)} is the lower bound of the
    nugget term. The default value is 0.0001.  }

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
  \item{xx}{The design set selected by the sequential design approach,
    which includes both the initial and the follow-up design points.}
  \item{yy}{The response matrix collected on the design set \code{xx}.}
  \item{xhat}{The estimated solution to the inverse problem obtained on the
    candidate set \code{candest} from the final fitted surrogate.}
  \item{maxei}{ A vector of length \code{nadd}, it collects the maximum
    value of the EI criterion in each iteration of the sequential design
    approach.}
}

\author{Ru Zhang \email{heavenmarshal@gmail.com},

  C. Devon Lin \email{devon.lin@queensu.ca},

  Pritam Ranjan \email{pritamr@iimidr.ac.in}}

\seealso{\code{\link{ESL2D}}, \code{\link{SL2D}}, \code{\link{svdGP}}.}

\references{
Huang, X. and Oosterlee, C. W. (2011) \emph{Saddlepoint approximations
  for expectations and an application to CDO pricing}, SIAM Journal on
Financial Mathematics, 2(1) 692-714.

Zhang, R. (2018) \emph{Modeling and Analysis of Dynamic Computer Experiments},
PhD thesis, Queen's University, ON, Canada.}
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
  xi <- lhs::randomLHS(30,3)
  candei <- lhs::randomLHS(500,3)
  candest <- lhs::randomLHS(500,3)
  candest <- rbind(candest, xi)

  ## evaluate the response matrix on the design matrix
  yi <- apply(xi,1,forretal,timepoints)
  x0 <- runif(3)
  y0 <- forretal(x0,timepoints)
  yobs <- y0+rnorm(200,0,sd(y0)/sqrt(50))
  ret <- saEI(xi,yi,yobs,1,candei,candest,forretal,timepoints,
              nstarts=1, nthread=1)
  yhat <- forretal(ret$xhat,timepoints)

  ## draw a figure to illustrate
  plot(y0,ylim=c(min(y0,yhat),max(y0,yhat)))
  lines(yhat,col="red")
}
\keyword{GP model}
\keyword{Inverse problem}
\keyword{SVD}
