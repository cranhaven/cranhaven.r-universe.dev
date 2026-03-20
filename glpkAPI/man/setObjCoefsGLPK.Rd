\name{setObjCoefsGLPK}
\alias{setObjCoefsGLPK}

\title{
  Set/Change Objective Coefficients and/or Constant Term
}

\description{
  This is an advanced version of \code{\link{setColBndGLPK}}.
  Here, \code{j} can be an integer vector, \code{obj_coef}
  can be a numeric vector.
}

\usage{
  setObjCoefsGLPK(lp, j, obj_coef)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{j}{
    Vector of column numbers.
  }
  \item{obj_coef}{
    Vector of objective coefficients.
  }
}

\details{
  Interface to the C function \code{setObjCoefs} which calls the
  GLPK function \code{glp_set_obj_coef}.
}

\value{
  NULL
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

