\name{versionCore}
\alias{versionCore}
\title{ Package version}
\description{
 Prints package version obtained from C code.
  }
\usage{
versionCore()
}

\arguments{None.}

\details{
   The function returns the information about the current version obtained from underlying C library \code{link{CORElearn}}.
}
\value{
   Character string with information about the version.
}

\author{ Marko Robnik-Sikonja, Petr Savicky }

\keyword{ models }
\keyword{ regression }
\keyword{ nonlinear }
\keyword{ tree }
\keyword{ classif }

\seealso{
\code{\link{CORElearn}}.
}
\examples{
# load the package
library(CORElearn)

# print its version 
versionCore()
}

