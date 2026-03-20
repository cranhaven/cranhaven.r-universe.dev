\name{getBheadGLPK}
\alias{getBheadGLPK}
\alias{glp_get_bhead}

\title{
  Retrieve Basis Header Information
}

\description{
  Low level interface function to the GLPK function \code{glp_get_bhead}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  getBheadGLPK(lp, k)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{k}{
    Index of the basic variable.
  }
}

\details{
  Interface to the C function \code{getBhead} which calls the GLPK
  function \code{glp_get_bhead}.
}

\value{
    Index of the auxiliary/structural variable.
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
