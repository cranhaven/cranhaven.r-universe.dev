\name{setColsBndsObjCoefsGLPK}
\alias{setColsBndsObjCoefsGLPK}

\title{
  Set/Change Column Bounds and Objective Coefficients and/or Constant Term
}

\description{
  This is an combined version of \code{\link{setColsBndsGLPK}} and
  \code{\link{setObjCoefsGLPK}}. Here, \code{j} can be an integer
  vector, \code{lb}, \code{ub} and \code{obj_coef} can be numeric
  vectors.
}

\usage{
  setColsBndsObjCoefsGLPK(lp, j, lb, ub, obj_coef, type = NULL)
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
  \item{lb}{
    Vector of lower bounds.
  }
  \item{ub}{
    Vector of upper bounds.
  }
  \item{obj_coef}{
    Vector of objective coefficients.
  }
  \item{type}{
    Vector of variable types (default: NULL). For possible values, see
    \code{\link{glpkConstants}}, section \sQuote{LP/MIP problem object}.
  }
}

\details{
  Interface to the C function \code{setColsBndsObjCoefs} which
  calls the GLPK functions \code{glp_set_col_bnds} and
  \code{glp_set_obj_coef}.
  
  If \code{type} is set to NULL, the type of the variables will be estimated.
  If lb[i] equals ub[i], variable j[i] is fixed, otherwise double bounded.
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


\seealso{
  \code{\link{glpkConstants}}
}

\keyword{ optimize }

