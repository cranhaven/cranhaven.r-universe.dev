\name{smooth.rank}
\Rdversion{1.1}
\alias{smooth.rank}

\title{
Apply a spectral filter
}

\description{
Apply a spectral filter
}

\usage{
smooth.rank(x, k)
}

\arguments{
\item{x}{Time series data}
\item{k}{Number of frequencies}
}

\details{
Passes the mean and the \code{k} frequencies with the highest amplitude.
The remainder of the spectrum is filtered out.
}

\value{
Returns a vector the same length as \code{x}.
}

\examples{
y = smooth.rank(deaths,2)
plotc(deaths,y)
}
