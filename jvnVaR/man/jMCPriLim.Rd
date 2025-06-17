\name{jMCPriLim}
\alias{jMCPriLim}
\title{
Monte-Carlo Price Simulation (under price limit condition)
}
\description{
Using when you need a series of price to do back-testing.

This function using normal return model to simulate price under price limit condition.

Price limit condition require that the return on price is limited.

Related report: Value at Risk.<researchgate.net>
}
\usage{
jMCPriLim(s0, L, U, mu, sigma,m)
}
\arguments{
  \item{s0}{
The initial price or the price at the first day.
}

  \item{L}{
Lower limit of return.
}

  \item{U}{
Upper limit of return.
}

  \item{mu}{
Expected (or mean) of return.
}

  \item{sigma}{
Standard deviation (or volatility) of return.
}
  \item{m}{
Number of observations.
}
}

\value{
An array of price.
}
\references{
Value at Risk.(reserchgate.net)
}
\author{
Hung Vu
}
\note{
viet-hung.vu@jvn.edu.vn
}

\seealso{
https://www.researchgate.net/profile/Vu_Hung4
}
\examples{
s0 <- 100
mu <- 0.02
sigma <- 0.1
m <- 1000
L <- -0.07
U <- 0.07
jMCPriLim (s0, L, U, mu, sigma, m) 
}
\keyword{ jVaR }
\keyword{ jPrice }
\keyword{ jReturn }