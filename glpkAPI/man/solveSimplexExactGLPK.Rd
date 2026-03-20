\name{solveSimplexExactGLPK}
\alias{solveSimplexExactGLPK}
\alias{glp_exact}

\title{
  Solve LP Problem in Exact Arithmetic
}

\description{
  Low level interface function to the GLPK function \code{glp_exact}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  solveSimplexExactGLPK(lp)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
}

\details{
  Interface to the C function \code{solveSimplexExact} which calls the GLPK
  function \code{glp_exact}.
}

\value{
  A return code.
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
  \code{\link{glpkConstants}}, section \sQuote{return codes} and
  \code{\link{return_codeGLPK}}.
}

\keyword{ optimize }

