\name{artsim}
\alias{artsim}
\title{Simulation of stationary ARTFIMA
}
\description{
Simulation of stationary ARTFIMA, ARFIMA or ARIMA or bootstrap a fitted model.
Useful for the parametric bootstrap.
}
\usage{
artsim(n = 100, d = 0, lambda = 0, phi = numeric(0), 
    theta = numeric(0), mean = 0, sigma2 = 1, obj = NULL)
}

\arguments{
   \item{n}{
length of time series
}
  \item{d}{
artfima difference parameter, real value greater than zero. 
If d=0, ARIMA model is used.
}
  \item{lambda}{
lambda artfima temper decay parameter, if lambda=0, ARFIMA model is simulated
}
  \item{phi}{
AR coefficients
}
  \item{theta}{
MA coefficients
}
  \item{mean}{
mean of series
}
  \item{sigma2}{
innovation variance
}
 \item{obj}{
output from artfima(). If obj is not output from artfima() 
then the other arguments are used to determine the time series parameters, 
except for the series length n.
}
}

\value{
vector of length n, the simulated time series
}

\author{
A. I. McLeod, aimcleod@uwo.ca
}

\references{
McLeod, A.I., Yu, Hao and Krougly, Z. (2007).  
Algorithms for Linear Time Series Analysis: With R Package.  
Journal of Statistical Software  23/5 1-26.
}  

\examples{
z <- artsim(5000, d=5/6, lambda=0.045)
var(z)
artfimaTACVF(d=5/6, lambda=0.045, maxlag=1)[1]
}

\keyword{ ts }
\keyword{ datagen }