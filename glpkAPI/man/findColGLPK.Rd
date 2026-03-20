\name{findColGLPK}
\alias{findColGLPK}
\alias{glp_find_col}

\title{
  Find Column by its Name
}

\description{
  Low level interface function to the GLPK function \code{glp_find_col}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  findColGLPK(lp, cname)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{cname}{
    A column name.
  }
}

\details{
  Interface to the C function \code{findCol} which calls the GLPK
  function \code{glp_find_column}.
}

\value{
  Returns the ordinal number of a column, which is assigned the specified cname.
}

\note{
  Before calling \code{findColGLPK} for the first time on a problem object
  \code{lp}, an index has to created via a call to
  \code{\link{createIndexGLPK}}.
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

