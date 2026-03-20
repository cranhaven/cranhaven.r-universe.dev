\name{getUnbndRayGLPK}
\alias{getUnbndRayGLPK}
\alias{glp_get_unbnd_ray}

\title{
  Determine Variable Causing Unboundedness
}

\description{
  Low level interface function to the GLPK function \code{glp_get_unbnd_ray}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  getUnbndRayGLPK(lp)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
}

\details{
  Interface to the C function \code{getUnbndRay} which calls the GLPK
  function \code{glp_get_unbnd_ray}.
}

\value{
  Returns the number k of a variable, which causes primal or dual unboundedness.
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
