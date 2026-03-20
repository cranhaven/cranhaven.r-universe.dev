\name{setMatRowGLPK}
\alias{setMatRowGLPK}
\alias{glp_set_mat_row}

\title{
  Set (Replace) Row of the Constraint Matrix
}
\description{
  Low level interface function to the GLPK function \code{glp_set_mat_row}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  setMatRowGLPK(lp, i, len, ind, val)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{i}{
    Replace the \code{i}-th row of the constraint matrix of the specified
    problem object.
  }
  \item{len}{
    Number of new row elements.
  }
  \item{ind}{
    Column indices of the new row elements.
  }
  \item{val}{
    Numerical values of the new row elements.
  }
}

\details{
  Interface to the C function \code{setMatRow} which calls the GLPK
  function \code{glp_set_mat_row}.
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

