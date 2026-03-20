\name{getObjCoefsGLPK}
\alias{getObjCoefsGLPK}

\title{
  Retrieve Objective Coefficients at Specified Columns and/or Constant Term
}

\description{
  This is an advanced version of \code{\link{getObjCoefGLPK}}.
  Here, \code{j} can be an integer vector.
}

\usage{
  getObjCoefsGLPK(lp, j)
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
}

\details{
  Interface to the C function \code{getObjCoef} which calls the GLPK
  function \code{glp_get_obj_coef}.
}

\value{
  The objective coefficient at all specified columns (\code{j})
  (the corresponding structural variable) is returned. If
  \code{j} is \code{0}, the constant term \dQuote{shift} of the
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

