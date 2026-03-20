\name{getColLowBndGLPK}
\alias{getColLowBndGLPK}
\alias{glp_get_col_lb}

\title{
  Retrieve Column Lower Bound
}

\description{
  Low level interface function to the GLPK function \code{glp_get_col_lb}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  getColLowBndGLPK(lp, j)
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
  Interface to the C function \code{getColLowBnd} which calls the GLPK
  function \code{glp_get_col_lb}.
}

\value{
  The lower bound of the \code{j}-th column (the corresponding structural
  variable) is returned.
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

