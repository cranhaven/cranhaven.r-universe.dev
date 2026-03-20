\name{getMIPParmGLPK}
\alias{getMIPParmGLPK}

\title{
  Retrives the Control Parameters for MIP.
}

\description{
  Returns the names and values of members in the structure
  \code{glp_iocp}. Consult the GLPK documentation for
  more detailed information.
}

\usage{
  getMIPParmGLPK()
}

\details{
  Interface to the C function \code{getMIPParm}.
}

\value{
  The function returns a list.
    \item{integer}{The names and corresponding values of all integer
                   control parameters in \code{glp_iocp}.}
    \item{double}{The names and corresponding values of all double
                   control parameters in \code{glp_iocp}.}
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

