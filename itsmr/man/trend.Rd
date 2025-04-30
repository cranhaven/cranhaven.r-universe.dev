\name{trend}
\Rdversion{1.1}
\alias{trend}

\title{
Estimate trend component
}

\description{
Estimate trend component
}

\usage{
trend(x, p)
}

\arguments{
\item{x}{Time series data}
\item{p}{Polynomial order (1 linear, 2 quadratic, etc.)}
}

\value{
Returns a vector the same length as \code{x}.
Subtract from \code{x} to obtain residuals.
The returned vector is the least squares fit of a polynomial to the data.
}

\seealso{
\code{\link{season}}
}

\examples{
y = trend(uspop,2)
plotc(uspop,y)
}
