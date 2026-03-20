\name{printIptGLPK}
\alias{printIptGLPK}
\alias{glp_print_ipt}

\title{
  Write Interior-Point Solution in Printable Format
}

\description{
  Low level interface function to the GLPK function \code{glp_print_ipt}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  printIptGLPK(lp, fname)
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
  Interface to the C function \code{printIpt} which calls the GLPK
  function \code{glp_print_ipt}.
}

\value{
  Returns zero on success, otherwise it returns non-zero and prints an
  error message.
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
  \code{\link{writeSolGLPK}}, \code{\link{readIptGLPK}},
  \code{\link{writeIptGLPK}}, \code{\link{printMIPGLPK}},
  \code{\link{readMIPGLPK}} and \code{\link{writeMIPGLPK}}.
}

\keyword{ optimize }

