\name{Periodogram}
\alias{Periodogram}

\title{
Periodogram
}
\description{
Computes the raw periodogram defined by,
\deqn{
I(f_j) = \frac{1}{n} | sum z[t] exp(2 \pi f_j) |^2
} 
}
\usage{
Periodogram(z)
}

\arguments{
  \item{z}{
vector, time series
}
}
\details{
The expected value of the periodogram equals the spectral density function.
}
\value{
the periodogram
}

\author{
A. I. McLeod
}

\seealso{
\code{\link{artfimaSDF}}
}
\examples{
data(sunspot.year)
Ip <- Periodogram(sunspot.year)
fr <- (1:length(Ip))/length(sunspot.year)
plot(fr, Ip, xlab="frequency", ylab="Periodogram")
}

\keyword{ ts }
