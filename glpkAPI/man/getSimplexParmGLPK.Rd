\name{getSimplexParmGLPK}
\alias{getSimplexParmGLPK}

\title{
  Retrives the Control Parameters for the Simplex Method.
}

\description{
  Returns the names and values of members in the structure
  \code{glp_smcp}. Consult the GLPK documentation for
  more detailed information.
}

\usage{
  getSimplexParmGLPK()
}

\details{
  Interface to the C function \code{getSimplexParm}.
}

\value{
  The function returns a list.
    \item{integer}{The names and corresponding values of all integer
                   control parameters in \code{glp_smcp}.}
    \item{double}{The names and corresponding values of all double
                  control parameters in \code{glp_smcp}.}
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

