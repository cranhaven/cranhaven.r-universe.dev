\name{readLPGLPK}
\alias{readLPGLPK}
\alias{glp_read_lp}

\title{
  Read Problem Data in CPLEX LP Format
}

\description{
  Low level interface function to the GLPK function \code{glp_read_lp}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  readLPGLPK(lp, fname)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{fname}{
    The name of the text file to be read in.
  }
}

\details{
  Interface to the C function \code{readLP} which calls the GLPK
  function \code{glp_read_lp}.
}

\value{
  Returns zero on success, otherwise it returns non-zero
  and prints an error message.
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
  \code{\link{readMPSGLPK}}, \code{\link{readProbGLPK}}, 
  \code{\link{writeMPSGLPK}}, \code{\link{writeLPGLPK}} and
  \code{\link{writeProbGLPK}}.
}

\keyword{ optimize }

