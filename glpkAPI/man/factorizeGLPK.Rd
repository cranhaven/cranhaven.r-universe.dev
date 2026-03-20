\name{factorizeGLPK}
\alias{factorizeGLPK}
\alias{glp_factorize}

\title{
  Compute the basis factorization
}

\description{
  Low level interface function to the GLPK function \code{glp_factorize}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  factorizeGLPK(lp)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
}

\details{
  Interface to the C function \code{factorize} which calls the GLPK
  function \code{glp_factorize}.
}

\value{
  Returns zero if the basis factorization has been successfully computed.
  Otherwise the routine returns non-zero.
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
  \code{\link{glpkConstants}}, section \sQuote{return codes}.
}

\keyword{ optimize }
