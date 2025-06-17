\name{jVaRLim}
\alias{jVaRLim}
\title{
Value at Risk Function(under price limit condition)
}
\description{
Compute VaR under price limit condition.

See the report: Value at Risk.<researchgate.net>
}
\usage{
jVaRLim(Ret, L, U, alpha, type, h)
}
\arguments{
  \item{Ret}{
  
A return series that computed from price series.
}

  \item{L}{
  
Lower limit.
}

  \item{U}{
  
Upper limit.
}

  \item{alpha}{
  
Given probability of the event that loss exceeds VaR.
}

  \item{type}{
  
Computing method.

'model': Garch(1,1) method.

'histl': Historical method with return series adjusted by Garch(1,1) method.

'simul': Simulation method.
}

  \item{h}{
  
Time point of VaR computing (...,-1,0,1,...)

       . -1 : previous day 
	   
       .  0 : present
	   
       .  1 : next day
}
}

\value{
Value at Risk at the time point.
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
y <- c(11, 12, 10, 13, 12, 14, 13, 15, 13, 14, 12)
s <- jReturn(y)
alpha <- 0.2
h <- 0
L <- -0.13
U <- 0.16
v <- jVaRLim(s,L,U,alpha,'model',h)
}
\keyword{ jVaR }
\keyword{ jPrice }
\keyword{ jReturn }