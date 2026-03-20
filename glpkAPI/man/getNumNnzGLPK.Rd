\name{getNumNnzGLPK}
\alias{getNumNnzGLPK}
\alias{glp_get_num_nz}

\title{
  Retrieve the Number of Constraint Coefficients
}

\description{
  Low level interface function to the GLPK function \code{glp_get_num_nz}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  getNumNnzGLPK(lp)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
}

\details{
  Interface to the C function \code{getNumNnz} which calls the GLPK
  function \code{glp_get_num_nz}.
}

\value{
  Returns the number of non-zero elements in the constraint
  matrix of the specified problem object.
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

