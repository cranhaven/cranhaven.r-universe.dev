\name{bfUpdatedGLPK}
\alias{bfUpdatedGLPK}
\alias{glp_bf_updated}

\title{
  Check if the basis factorization has been updated
}

\description{
  Low level interface function to the GLPK function \code{glp_bf_updated}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  bfUpdatedGLPK(lp)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
}

\details{
  Interface to the C function \code{bfUpdated} which calls the GLPK
  function \code{glp_bf_updated}.
}

\value{
  Returns non-zero if the basis factorization has been updated at least once.
  Otherwise the routine returns zero.
}

\references{
  Based on the package \pkg{glpk} by Lopaka Lee.
    
  The GNU GLPK home page at \url{http://www.gnu.org/software/glpk/glpk.html}.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\keyword{ optimize }
