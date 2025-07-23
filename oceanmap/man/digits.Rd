\name{digits}
\alias{digits}
\title{
returns recognized number of digits for colorbar ticklabels}
\description{
returns recognized number of digits for colorbar ticklabels. Used by \link{v.plot} inside \link{v}.
}
\usage{
digits(x)
}
\arguments{
  \item{x}{
a numeric value. If x >= 1 output is set to zero (meaning ticklabels will have no digit positions).
}
}
\seealso{ \link{v.plot}}


\author{
Robert K. Bauer
}

\keyword{internal}

\examples{
## Example 1: ticklabels with no digits
digits(1000)
digits(1)
digits(1.01)

## Example 2: ticklabels with digits
digits(0.1)
digits(0.01)


}