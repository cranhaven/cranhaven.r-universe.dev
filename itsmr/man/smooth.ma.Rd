\name{smooth.ma}
\Rdversion{1.1}
\alias{smooth.ma}

\title{
Apply a moving average filter
}

\description{
Apply a moving average filter
}

\usage{
smooth.ma(x, q)
}

\arguments{
\item{x}{Time series data}
\item{q}{Filter order}
}

\details{
The averaging function uses \code{2q+1} values.
}

\value{
Returns a vector the same length as \code{x}.
}

\examples{
y = smooth.ma(strikes,2)
plotc(strikes,y)
}
