\name{setDefaultMIPParmGLPK}
\alias{setDefaultMIPParmGLPK}
\alias{glp_init_iocp}

\title{
  Sets the Default Control Parameters for the MIP Method
}

\description{
  Initializes a new structure \code{glp_iocp}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  setDefaultMIPParmGLPK()
}

\details{
  Interface to the C function \code{setDefaultMIPParm} which calls the GLPK
  function \code{glp_init_iocp}.
}

\value{
  NULL
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

