\name{hr}
\Rdversion{1.1}
\alias{hr}

\title{
Estimate harmonic components
}

\description{
Estimate harmonic components
}

\usage{
hr(x, d)
}

\arguments{
\item{x}{Time series data}
\item{d}{Vector of harmonic periods}
}

\value{
Returns a vector the same length as \code{x}.
Subtract from \code{x} to obtain residuals.
}

\examples{
y = hr(deaths,c(12,6))
plotc(deaths,y)
}
