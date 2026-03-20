\name{getRbindGLPK}
\alias{getRbindGLPK}
\alias{glp_get_row_bind}

\title{
  Retrieve Row Index in the Basis Header
}

\description{
  Low level interface function to the GLPK function \code{glp_get_row_bind}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  getRbindGLPK(lp, i)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{i}{
    Auxiliary variable \code{i}.
  }
}

\details{
  Interface to the C function \code{getRbind} which calls the GLPK
  function \code{glp_get_row_bind}.
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
