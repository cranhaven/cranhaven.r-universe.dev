\name{getRowsPrimGLPK}
\alias{getRowsPrimGLPK}

\title{
  Retrieve Row Primal Value of all Rows
}

\description{
  This is an advanced version of \code{\link{getRowPrimGLPK}}.
}
\usage{
  getRowsPrimGLPK(lp)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
}

\details{
  Interface to the C function \code{getRowsPrim} which calls the GLPK
  function \code{glp_get_row_prim}.
}

\value{
  The row primal values for all rows are returned.
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

