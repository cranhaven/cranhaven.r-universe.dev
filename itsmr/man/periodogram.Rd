\name{periodogram}
\Rdversion{1.1}
\alias{periodogram}

\title{
Plot a periodogram
}

\description{
Plot a periodogram
}

\usage{
periodogram(x, q = 0, opt = 2)
}

\arguments{
\item{x}{Time series data}
\item{q}{MA filter order}
\item{opt}{Plot option (0 silent, 1 periodogram only, 2 periodogram and filter)}
}

\details{
The filter \code{q} can be a vector in which case the overall filter is the
composition of MA filters of the designated orders.
}

\value{
The periodogram vector divided by 2pi is returned invisibly.
}

\seealso{
\code{\link{plots}}
}

\examples{
periodogram(Sunspots,c(1,1,1,1))
}
