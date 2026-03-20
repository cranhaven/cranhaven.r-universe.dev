\name{setColNameGLPK}
\alias{setColNameGLPK}
\alias{glp_set_col_name}

\title{
  Set/Change Column Name
}

\description{
  Low level interface function to the GLPK function \code{glp_set_col_name}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  setColNameGLPK(lp, j, cname = NULL)
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
  \item{cname}{
    Column name.
  }
}

\details{
  Interface to the C function \code{setColName} which calls the GLPK
  function \code{glp_set_col_name}.
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

