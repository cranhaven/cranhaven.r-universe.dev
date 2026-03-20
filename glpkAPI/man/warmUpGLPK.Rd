\name{warmUpGLPK}
\alias{warmUpGLPK}
\alias{glp_warm_up}

\title{
  Warm Up LP Basis
}
\description{
  Low level interface function to the GLPK function \code{glp_warm_up}.
  Consult the GLPK documentation for more detailed information.
}
\usage{
  warmUpGLPK(lp)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
}

\details{
  Interface to the C function \code{warmUp} which calls the GLPK
  function \code{glp_warm_up}.
}

\value{
  Status of \dQuote{warming up}.
}

\references{
  Based on the package \pkg{glpk} by Lopaka Lee
    
  The GNU GLPK home page at \url{http://www.gnu.org/software/glpk/glpk.html}
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\keyword{ optimize }

