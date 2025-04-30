\name{acvf}
\Rdversion{1.1}
\alias{acvf}

\title{
Autocovariance of data
}

\description{
Autocovariance of data
}

\usage{
acvf(x, h = 40)
}

\arguments{
\item{x}{Time series data}
\item{h}{Maximum lag}
}

\value{
Returns a vector of length \code{h+1} to accomodate lag 0 at index 1.
}

\seealso{
\code{\link{plota}}
}

\examples{
acvf(Sunspots)
}
