\name{versionGLPK}
\alias{versionGLPK}
\alias{glp_version}

\title{
  Determine GLPK Callable Library Version
}

\description{
  Low level interface function to the GLPK function \code{glp_version}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  versionGLPK()
}

\details{
  Interface to the C function \code{version} which calls the GLPK
  function \code{glp_version}.
}

\value{
  Returns a single character value containing the GLPK version number.
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

