\name{setMatColGLPK}
\alias{setMatColGLPK}
\alias{glp_set_mat_col}

\title{
  Set (Replace) Column of the Constraint Matrix
}
\description{
  Low level interface function to the GLPK function \code{glp_set_mat_col}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  setMatColGLPK(lp, j, len, ind, val)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{j}{
    Replace the \code{j}-th column of the constraint matrix of the specified
    problem object.
  }
  \item{len}{
    Number of new column elements.
  }
  \item{ind}{
    Row indices of the new column elements.
  }
  \item{val}{
    Numerical values of the new column elements.
  }
}

\details{
  Interface to the C function \code{setMatCol} which calls the GLPK
  function \code{glp_set_mat_col}.
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

