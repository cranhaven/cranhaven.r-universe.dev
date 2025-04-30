\name{smooth.exp}
\Rdversion{1.1}
\alias{smooth.exp}

\title{
Apply an exponential filter
}

\description{
Apply an exponential filter
}

\usage{
smooth.exp(x, alpha)
}

\arguments{
\item{x}{Time series data}
\item{alpha}{Smoothness setting, 0-1}
}

\details{
Zero is maximum smoothness.
}

\value{
Returns a vector of smoothed data the same length as \code{x}.
}

\examples{
y = smooth.exp(strikes,.4)
plotc(strikes,y)
}
