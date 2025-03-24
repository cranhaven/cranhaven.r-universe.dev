\name{get_bugs_wishart_scalemat}
\alias{get_bugs_wishart_scalemat}

\title{Compute a BUGS-compliant scale matrix for a Wishart prior
distribution for precision matrix that is consistent with target
variances.}

\description{Computes a scale matrix in the BUGS parameterization that
  corresponds to a minimally-informative Wishart prior distribution for
  a precision matrix, with the property that the medians of the
  diagonals of the implied prior distribution for the corresponding
  covariance matrix are approximately equal to specified target
  variances.
}

\usage{
get_bugs_wishart_scalemat(target, nsim=25000, reltol = 0.05, quietly=TRUE)
}

\arguments{
  \item{target}{
    A \code{p}-dimensional vector of target variances.  These are the
  variances that you would like your BUGS Wishart prior distribution to
  support.
  }

  \item{nsim}{
    Number of Monte-Carlo simulations used to set target scale matrix.
    Default is 25,000.
  }

  \item{reltol}{
    Relative tolerance determining when the algorithm stops trying to
    find a better scale matrix.  Default is 0.05.
  }

  \item{quietly}{
    If \code{quietly} is \code{FALSE}, prints iterative and final
    diagnostic information.  Default is \code{TRUE}.
  }
}

\details{
  When using WinBUGS/OpenBUGS/JAGS, it is often necessary to provide a
  Wishart prior distribution for the precision matrix of a
  \code{p}-dimensional random vector.  It is common to use a Wishart
  distribution with \code{p+1} degrees of freedom in this case.  The
  question is what scale matrix to use.  The BUGS languages parameterize the
  Wishart distribution such that if a precision matrix \code{M} is given
  the prior distribution \code{M ~ dwish(S,p+1)} for a \code{pxp} scale
  matrix \code{S} and \code{p+1} degrees of freedom, the expected value
  of \code{M} is \code{p+1} times the inverse of \code{S}.

  The current function determines a diagonal scale matrix \code{S} such
  that the implied prior distribution for the inverse of \code{M}, the
  variance/covariance matrix of the random vector, under the
  distribution \code{M ~ dwish(S,p+1)} in the BUGS parameterization, has
  medians of the diagonal elements approximately equal to some target
  variances specified by \code{target}.  It iteratively tries values of
  \code{S} via Monte Carlo simulation to select a value of \code{S} with
  the desired property.

  The value of \code{reltol} determines how close the match must be.
  Larger values of \code{nsim} and smaller values of \code{reltol} will
  lead to smaller Monte Carlo error in the estimate scale matrix.
}

\value{A list with elements

  \itemize{
    \item{bugs.df: }{Degrees of freedom to use for Wishart prior
    distribution in BUGS, equal to \code{p+1} where \code{p} is the
    dimension of \code{target}.}

  \item{bugs.scalemat: }{Scale matrix to use for Wishart prior
    distribution in BUGS.}

  \item{varsum: }{Summary of prior distribution of implied variances;
    medians should approximately equal \code{target}.}

  \item{corsum: }{Summary of prior distribution of implied
    correlations.}
  }
}

\author{
  J.R. Lockwood \email{jrlockwood@ets.org}
}

\examples{
\donttest{
tmp <- get_bugs_wishart_scalemat(target = c(10,4,4,8), nsim = 30000,
reltol = 0.02, quietly=FALSE)
print(tmp)

## if you now model precison matrix M ~ dwish(tmp$bugs.scalemat,
## tmp$bugs.df) in a BUGS language, this will imply a prior distribution
## for the inverse of M that has medians of the diagonal elements
## approximately equal to 'target'
}
}

\keyword{models}
