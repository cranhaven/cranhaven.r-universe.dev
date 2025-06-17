\name{jMCPri}
\alias{jMCPri}
\title{
Monte-Carlo Price Simulation
}
\description{
Using when you need a series of price to do back-testing.

This function using normal return model to simulate price.

Related report: Value at Risk.<researchgate.net>
}
\usage{
jMCPri(s0, mu, sigma, m)
}
\arguments{
  \item{s0}{
The initial price or the price at the first day.
}

  \item{mu}{
Expected (or drift) of return.
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
jMCPri (s0, mu, sigma, m) 
}
\keyword{ jVaR }
\keyword{ jPrice }
\keyword{ jReturn }

