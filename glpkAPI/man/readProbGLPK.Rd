\name{readProbGLPK}
\alias{readProbGLPK}
\alias{glp_read_prob}

\title{
  Read Problem Data in GLPK F
  ormat
}

\description{
  Low level interface function to the GLPK function \code{glp_read_prob}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  readProbGLPK(lp, fname)
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
  Interface to the C function \code{readProb} which calls the GLPK
  function \code{glp_read_prob}.
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
  \code{\link{readMPSGLPK}}, \code{\link{readLPGLPK}}, 
  \code{\link{writeMPSGLPK}}, \code{\link{writeLPGLPK}} and
  \code{\link{writeProbGLPK}}.
}

\keyword{ optimize }

