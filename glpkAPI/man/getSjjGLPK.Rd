\name{getSjjGLPK}
\alias{getSjjGLPK}
\alias{glp_get_sjj}

\title{
  Retrieve column scale factor
}

\description{
  Low level interface function to the GLPK function \code{glp_get_sjj}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  getSjjGLPK(lp, j)
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
  Interface to the C function \code{getSjj} which calls the GLPK
  function \code{glp_get_sjj}.
}

\value{
  Returns the current scale factor \eqn{s_{jj}} for column \code{j} of the
  specified problem object.
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
