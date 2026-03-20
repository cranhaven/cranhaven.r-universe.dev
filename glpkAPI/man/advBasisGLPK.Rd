\name{advBasisGLPK}
\alias{advBasisGLPK}
\alias{glp_adv_basis}

\title{
  Contruct Advanced Initial LP Basis
}

\description{
  Low level interface function to the GLPK function \code{glp_adv_basis}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  advBasisGLPK(lp)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
}

\details{
  Interface to the C function \code{advBasis} which calls the GLPK
  function \code{glp_adv_basis}.
}

\value{
  NULL
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

