\name{setRhsZeroGLPK}
\alias{setRhsZeroGLPK}

\title{
  Set/Change all Row Bounds to Zero
}

\description{
  This is an advanced version of \code{\link{setRowsBndsGLPK}}.
}

\usage{
  setRhsZeroGLPK(lp)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
}

\details{
  Interface to the C function \code{setRowsBnds} which calls the GLPK
  function \code{glp_set_col_bnds}. All row bounds are fixed at zero.
}

\value{
  NULL
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

