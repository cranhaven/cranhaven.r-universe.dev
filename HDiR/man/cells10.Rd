\name{cells10}
\alias{cells10}
\docType{data}
\title{
Indices of mesh cells for spherical numerical integration
}
\description{
Indices of mesh cells for spherical numerical integration.
}
\format{
  A data frame with 2000 rows where each one corresponds to the indices (rows) in the data set \code{points10} that characterizes a cell.
  \describe{
    \item{\code{V1}}{Index of the first cell vertex.}
    \item{\code{V2}}{Index of the second cell vertex.}
    \item{\code{V3}}{Index of the third cell vertex.}
  }
}
\examples{
data(cells10)
## maybe str(cells10) ; plot(cells10) ...
}
\keyword{internal}
