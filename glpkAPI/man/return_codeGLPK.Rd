\name{return_codeGLPK}
\alias{return_codeGLPK}

\title{
  Translates a GLPK Return Code into a Human Readable String
}

\description{
  Translates a GLPK return code into a human readable string.
}

\usage{
  return_codeGLPK(code)
}

\arguments{
  \item{code}{
    Return code from GLPK.
  }
}

\value{
  A character string associated with the GLPK return code.
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
  \code{\link{glpkConstants}}, section \sQuote{return codes}.
}

\keyword{ optimize }

