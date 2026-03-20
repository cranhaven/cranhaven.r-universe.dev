\name{bfExistsGLPK}
\alias{bfExistsGLPK}
\alias{glp_bf_exists}

\title{
  Check if the basis factorization exists
}

\description{
  Low level interface function to the GLPK function \code{glp_bf_exists}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  bfExistsGLPK(lp)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
}

\details{
  Interface to the C function \code{bfExists} which calls the GLPK
  function \code{glp_bf_exists}.
}

\value{
  Returns non-zero if the basis factorization for the specified problem object
  exists. Otherwise the routine returns zero.
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
