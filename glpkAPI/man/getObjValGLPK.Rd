\name{getObjValGLPK}
\alias{getObjValGLPK}
\alias{glp_get_obj_val}

\title{
  Retrieve Objective Value
}

\description{
  Low level interface function to the GLPK function \code{glp_get_obj_val}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  getObjValGLPK(lp)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
}

\details{
  Interface to the C function \code{getObjVal} which calls the GLPK
  function \code{glp_get_obj_val}.
}

\value{
  Returns the current value of the objective function.
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

