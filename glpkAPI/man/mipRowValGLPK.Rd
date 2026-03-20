\name{mipRowValGLPK}
\alias{mipRowValGLPK}
\alias{glp_mip_row_val}

\title{
  Retrieve Row Value
}

\description{
  Low level interface function to the GLPK function \code{glp_mip_row_val}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  mipRowValGLPK(lp, i)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{i}{
    Row number \code{i}.
  }
}

\details{
  Interface to the C function \code{mipRowVal} which calls the GLPK
  function \code{glp_mip_row_val}.
}

\value{
  Row value of row \code{i}.
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
