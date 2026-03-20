\name{setColsBndsGLPK}
\alias{setColsBndsGLPK}

\title{
  Set/Change Column Bounds
}

\description{
  This is an advanced version of \code{\link{setColBndGLPK}}.
  Here, \code{j} can be an integer vector, \code{lb} and \code{ub}
  can be numeric vectors.
}

\usage{
  setColsBndsGLPK(lp, j, lb, ub, type = NULL)
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
  \item{type}{
    Vector of variable types (default: NULL). For possible values, see
    \code{\link{glpkConstants}}, section \sQuote{LP/MIP problem object}.
  }
}

\details{
  Interface to the C function \code{setColsBnds} which calls the GLPK
  function \code{glp_set_col_bnds}.
  
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

