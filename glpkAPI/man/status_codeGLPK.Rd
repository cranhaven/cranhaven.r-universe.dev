\name{status_codeGLPK}
\alias{status_codeGLPK}

\title{
  Translates a GLPK Status Value into a Human Readable String
}

\description{
  Translates a GLPK status code into a human readable string.
}

\usage{
  status_codeGLPK(code)
}

\arguments{
  \item{code}{
    Status code from GLPK.
  }
}

\value{
  A character string associated with the GLPK status code.
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
  \code{\link{glpkConstants}}, section \sQuote{LP/MIP problem object}.
}

\keyword{ optimize }

