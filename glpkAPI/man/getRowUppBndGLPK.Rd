\name{getRowUppBndGLPK}
\alias{getRowUppBndGLPK}
\alias{glp_get_row_ub}

\title{
  Retrieve Row Upper Bound
}

\description{
  Low level interface function to the GLPK function \code{glp_get_row_ub}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  getRowUppBndGLPK(lp, i)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{i}{
    Row number i.
  }
}

\details{
  Interface to the C function \code{getRowUppBnd} which calls the GLPK
  function \code{glp_get_row_ub}.
}

\value{
  The upper bound of the \code{i}-th row (the corresponding auxiliary
  variable) is returned.
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

