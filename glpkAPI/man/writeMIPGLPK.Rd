\name{writeMIPGLPK}
\alias{writeMIPGLPK}
\alias{glp_write_mip}

\title{
  Write MIP Solution to Text File
}

\description{
  Low level interface function to the GLPK function \code{glp_write_mip}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  writeMIPGLPK(lp, fname)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{fname}{
    The name of the text file to be written out.
  }
}

\details{
  Interface to the C function \code{writeMIP} which calls the GLPK
  function \code{glp_write_mip}.
}

\value{
  Returns zero on success, otherwise it returns non-zero.
}

\references{
  Based on the package \pkg{glpk} by Lopaka Lee.
    
  The GNU GLPK home page at \url{http://www.gnu.org/software/glpk/glpk.html}.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\seealso{
  \code{\link{printSolGLPK}}, \code{\link{readSolGLPK}},
  \code{\link{writeSolGLPK}}, \code{\link{printIptGLPK}},
  \code{\link{readIptGLPK}}, \code{\link{writeIptGLPK}},
  \code{\link{printMIPGLPK}} and \code{\link{readMIPGLPK}}.
}

\keyword{ optimize }

