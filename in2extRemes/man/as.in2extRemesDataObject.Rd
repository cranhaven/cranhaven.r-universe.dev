\name{as.in2extRemesDataObject}
\alias{as.in2extRemesDataObject}
\title{
Convert Data Frame Object to in2extRemes Data Object
}
\description{
Convert a data frame, matrix or vector into an object readable by the in2extRemes windows.
}
\usage{
as.in2extRemesDataObject(x)
}
\arguments{
  \item{x}{
data frame, matrix or numeric vector.
}
}
\details{
Sets up a list object with a data component that contains the datafram \code{x}, or if \code{x} is a matrix or vector, it is first converted to a data frame using \code{as.data.frame}.  If \code{x} is a vector, an additional column, named \dQuote{obs}, is added that contains an index of the row number.  It is then given the class \dQuote{in2extRemesDataObject} and returned.
}
\value{
A list of class \dQuote{in2extRemesDataObject} with component:
  \item{data }{x as a data frame.}
}
\author{
Eric Gilleland
}
\seealso{
\code{\link{as.data.frame}}
}
\examples{

look <- apply(matrix(rnorm(100 * 100), 100, 100), 2, max, na.rm = TRUE)
look2 <- as.in2extRemesDataObject(look)

}
\keyword{ manip }
\keyword{ list }
