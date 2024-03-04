\name{am_call_partials}
\alias{am_call_partials}
\title{
Hedge parameters of a standard American call option using a binomial tree
}
\description{
Partials of an American call option using a binomial approximation
}
\usage{
am_call_partials(S, K, r, sigma, t, steps)
}
\arguments{
  \item{S}{spot price}
  \item{K}{exercise price}
  \item{r}{risk-free interest rate}
  \item{sigma}{volatility}
  \item{t}{time to maturity}
  \item{steps}{number of steps in binomial tree}
}

\details{
The binomial method provides for techniques to approximate the partials of a derivative instrument. The computation of the partials for the binomial tree used in this package is discussed by Hull (2006).
}

\value{
\item{hedge$delta}{partial with respect to S}
\item{hedge$gamma}{second partial with respect to S}
\item{hedge$theta}{partial with respect to time}
\item{hedge$vega}{partial with respect to sigma}
\item{hedge$rho}{partial with respect to r}
}
\references{
John Hull, "Options, Futures and Other Derivatives", Prentice-Hall, Sixth Edition, 2006.
}
\author{
Paolo Zagaglia, paolo.zagaglia@gmail.com
}




\examples{
rm(list=ls())

S<-100 
K<-100
r<-0.1 
sigma<-0.25
t<-1.0 
steps<-100

hedge<-am_call_partials(S, K, r, sigma, t, steps)
}
  
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ am_call_partials }
