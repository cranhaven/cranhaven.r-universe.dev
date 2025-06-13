\name{artfimaSDF}
\alias{artfimaSDF}

\title{Computation of theoretical spectral density function (SDF)}

\description{
Computes the theoretical SDF at the Fourier frequencies for a time series of
length n. Used for Whittle MLE. Assumes model parameters are valid
for a stationary process.
}

\usage{
artfimaSDF(n = 100, d = 0, lambda = 0, phi = numeric(0), theta = numeric(0), 
     obj = NULL, plot=c("loglog", "log", "none"))
}

\arguments{
  \item{n}{length of time series}
  \item{d}{ARTFIMA difference parameter, any real value. When d=numeric(0), 
  reduces to ARMA and lambda is ignored.}
  \item{lambda}{ARTFIMA tempered decay parameter. When lambda=numeric(0), 
      reduces to ARFIMA} 
  \item{phi}{AR coefficients}
  \item{theta}{MA coefficients, Box-Jenkins definition}
  \item{obj}{object of class artfima}
  \item{plot}{type of plot, "log-log", "log" or "none"}
}

\details{
The Fourier frequencies, 2*pi*c(1/n, floor(n/2)/n, 1/n), are used
in the definition of the SDF. 
The SDF is normalized so that the area over (0, 0.5) equals
the variance of the time series assuming unit innovation variance.
The periodogram is normalized in the same way, so the mean of the
periodogram is an estimate of the variance of the time series.
See example below.
}

\section{Warning }{
This function serves as a utility function for Whittle estimation so, for speed, we skip
the checking if the parameters d, phi, or lambda are valid
parameters for a stationary process. 
}


\value{
vector of length floor(n/2) containing the values of the SDF at the 
  Fourier frequencies, 2*pi*c(1/n, floor(n/2)/n, 1/n).
}

\references{TBA}

\author{A. I. McLeod, aimcleod@uwo.ca}

\seealso{
\code{\link{artfimaTACVF}},
\code{\link{Periodogram}}
}


\examples{
phi <- 0.8
n <- 256
set.seed(4337751)
z <- artsim(n, phi=phi)
VarZ <- mean((z-mean(z))^2)
Ip <- Periodogram(z)
length(Ip)
x <- (1/n)*(1:length(Ip))
plot(x, Ip, xlab="frequency", ylab="Spectral density & Periodogram", 
     main=paste("AR(1), phi =", phi), type="l", col=rgb(0,0,1,0.5))
n <- 5000
y <- artfimaSDF(n, phi=phi)
x <- (1/n)*(1:length(y))
lines(x, y, type="l", lwd=1.25)
h <- x[2]-x[1] #step length
SimpsonsRule <- function(h, y) {
  n <- length(y)
  h/3*sum(y * c(1, rep(c(4,2), n-1), 1))
}
AreaApprox <- SimpsonsRule(h, y)
text(0.2, 50, labels=paste("Area under SDF using Simpson's Rule =", 
                           round(AreaApprox,4)))
TVarZ <- 1/(1-phi^2)
text(0.2, 40, labels=paste("Theoretical AR Variance =", round(TVarZ,4)))
text(0.2, 30, labels=paste("mean(Ip) =", round(mean(Ip),4)))
text(0.2, 20, labels=paste("sample variance =", round(VarZ,4)))
}

\keyword{ ts }
