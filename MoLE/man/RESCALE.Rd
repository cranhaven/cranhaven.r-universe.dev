\name{RESCALE}
\alias{RESCALE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Rescale vector values
}
\description{
Rescale vector values to -1:1 range (or 0:1 if there are no negative values)
}
\usage{
RESCALE(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Vector to be rescaled
}
}
\value{
Numeric vector, with abs(max value) of 1
}
\author{
Sander Lestrade
}

\seealso{
\code{CANDIDATESCORE}, \code{FREQUPDATE}
}
\examples{
RESCALE(-10:5)
}
\keyword{misc}
