\name{auxTest}
\alias{testTime}
\alias{testClassPseudoRandom}
\title{Test functions for manual usage}
\description{
Test functions for the current state of the development.
}
\usage{
testTime()
testClassPseudoRandom(s, k, m)
}

\arguments{
\item{s}{Seed.}
\item{k}{Length of required output.}
\item{m}{number of streams.}
}

\details{
\code{testTime()} determines the current time.
\code{testClassPseudoRandom(s, k, m)} tests the functionality of multiple streams of RNGs.
}
\value{
Depends on the function.
}

\author{ Marko Robnik-Sikonja, Petr Savicky }

\seealso{
\code{\link{CORElearn}}.
}
\examples{
testTime()
}

