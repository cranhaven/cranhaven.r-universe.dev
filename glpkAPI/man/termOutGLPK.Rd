\name{termOutGLPK}
\alias{termOutGLPK}
\alias{glp_term_out}

\title{
  Enable/Disable Terminal Output
}

\description{
  Low level interface function to the GLPK function \code{glp_term_out}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  termOutGLPK(flag)
}

\arguments{
  \item{flag}{
    GLPK enable/disable flag: \code{GLP_ON} or \code{GLP_OFF}.
  }
}

\details{
  Interface to the C function \code{termOut} which calls the GLPK
  function \code{glp_term_out}.
}

\value{
  Returns the previous value of the terminal output flag.
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
  \code{\link{glpkConstants}}, section \sQuote{enable/disable flag}.
}

\keyword{ optimize }

