\name{artfimaTACVF}
\alias{artfimaTACVF}

\title{Autocovariance function of ARTFIMA}

\description{Theoretical autocovariance function of ARTFIMA model}

\usage{
artfimaTACVF(d = numeric(0), lambda = numeric(0), phi = numeric(0), 
      theta = numeric(0), maxlag, sigma2 = 1, obj = NULL)
}

\arguments{
  \item{d}{ARTFIMA difference parameter, any real value. When d=0, reduces to 
      ARMA and lambda is ignored.}
  \item{lambda}{ARTFIMA tempered decay parameter. When lambda=0, 
      reduces to ARFIMA} 
  \item{phi}{AR coefficients}
  \item{theta}{MA coefficients, Box-Jenkins definition}
  \item{maxlag}{maxlag+1 lags computed corresponding to 0,1,...,maxlag}
  \item{sigma2}{innovation variance}
  \item{obj}{output from artfima function}
}


\value{vector of length maxlag+1 of the specified autocovariances}


\author{A. I. McLeod, aimcleod@uwo.ca}

\seealso{
\code{\link{ARMAacf}},
\code{\link{artfimaSDF}},
\code{\link{artsim}},
\code{\link{artfima}}
}


\examples{
#ARTFIMA - area under SDF equals theoretical Var(z[t])
#and sample variance = mean of periodogram
#
lambda <- 0.045
d <- 5/6
TVarZ <- artfimaTACVF(d=d, lambda=lambda, maxlag=3)[1]
TVarZ
n <- 256
set.seed(4337751)
z <- artsim(n, lambda=lambda, d=d)
VarZ <- mean((z-mean(z))^2)
Ip <- Periodogram(z)
mean(Ip)
length(Ip)
x <- (1/n)*(1:length(Ip))
plot(x, Ip, xlab="frequency", ylab="Spectral density & Periodogram", 
     main=paste("lambda, d =", lambda, d), type="l", col=rgb(0,0,1,0.5))
n <- 5000
y <- artfimaSDF(n, lambda=lambda, d=d)
x <- (1/n)*(1:length(y))
lines(x, y, type="l", lwd=1.25)
h <- x[2]-x[1] #step length
SimpsonsRule <- function(h, y) {
  n <- length(y)
  h/3*sum(y * c(1, rep(c(4,2), n-1), 1))
}
AreaApprox <- SimpsonsRule(h, y)
text(0.2, 230, labels=paste("Area under SDF using Simpson's Rule =", 
                           round(AreaApprox,4)))
text(0.2, 200, labels=paste("Theoretical ARTFIMA Variance =", round(TVarZ,4)))
text(0.2, 170, labels=paste("mean(Ip) =", round(mean(Ip),4)))
text(0.2, 140, labels=paste("sample variance =", round(VarZ,4)))
}

\keyword{ts}

