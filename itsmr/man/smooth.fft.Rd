\name{smooth.fft}
\Rdversion{1.1}
\alias{smooth.fft}

\title{
Apply a low pass filter
}

\description{
Apply a low pass filter
}

\usage{
smooth.fft(x, f)
}

\arguments{
\item{x}{Time series data}
\item{f}{Cut-off frequency, 0-1}
}

\details{
The cut-off frequency is specified as a fraction.
For example, \code{c=.25} passes the lowest 25\% of the spectrum.
}

\value{
Returns a vector the same length as \code{x}.
}

\examples{
y = smooth.fft(deaths,.1)
plotc(deaths,y)
}
