\name{getObjCoefGLPK}
\alias{getObjCoefGLPK}
\alias{glp_get_obj_coef}

\title{
  Retrieve Objective Coefficient or Constant Term
}

\description{
  Low level interface function to the GLPK function \code{glp_get_obj_coef}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  getObjCoefGLPK(lp, j)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{j}{
    Column number j.
  }
}

\details{
  Interface to the C function \code{getObjCoef} which calls the GLPK
  function \code{glp_get_obj_coef}.
}

\value{
  The objective coefficient at the \code{j}-th column (the
  corresponding   structural variable) is returned. If \code{j}
  is \code{0}, the constant term \dQuote{shift} of the
  objective function is returned.
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

