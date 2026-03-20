\name{getBfcpGLPK}
\alias{getBfcpGLPK}
\alias{glp_get_bfcp}

\title{
  Retrieve Basis Factorization Control parameters
}

\description{
  Returns the names and values of members in the structure
  \code{glp_bfcp}. Consult the GLPK documentation for
  more detailed information.
}

\usage{
  getBfcpGLPK(lp)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
}

\details{
  Interface to the C function \code{getBfcp}.
}

\value{
  The function returns a list.
    \item{integer}{The names and corresponding values of all integer
                   control parameters in \code{glp_bfcp}.}
    \item{double}{The names and corresponding values of all double
                  control parameters in \code{glp_bfcp}.}
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
  \code{\link{glpkConstants}}, section \sQuote{Control Parameters}.
}

\keyword{ optimize }

