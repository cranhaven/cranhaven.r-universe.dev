\name{getCbindGLPK}
\alias{getCbindGLPK}
\alias{glp_get_col_bind}

\title{
  Retrieve Column Index in the Basis Header
}

\description{
  Low level interface function to the GLPK function \code{glp_get_col_bind}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  getCbindGLPK(lp, j)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{j}{
    Structural variable \code{j}.
  }
}

\details{
  Interface to the C function \code{getCbind} which calls the GLPK
  function \code{glp_get_col_bind}.
}

\value{
  Index of the basic variable.
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
