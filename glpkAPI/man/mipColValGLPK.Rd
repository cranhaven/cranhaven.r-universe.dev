\name{mipColValGLPK}
\alias{mipColValGLPK}
\alias{glp_mip_col_val}

\title{
  Retrieve Column Value
}

\description{
  Low level interface function to the GLPK function \code{glp_mip_col_val}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  mipColValGLPK(lp, j)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{j}{
    Column number \code{j}.
  }
}

\details{
  Interface to the C function \code{mipColVal} which calls the GLPK
  function \code{glp_mip_col_val}.
}

\value{
  Column value of column \code{j}.
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
