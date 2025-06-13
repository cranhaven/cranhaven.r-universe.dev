\name{tseg}
\alias{tseg}

\title{
Simulate Some Time Series Models of Interest
}
\description{
Time series models are simulated based on some familar characteristics 
described in Details. 
}
\usage{
tseg(n, which = c("BJAR2", "BJAR1", "BJAR3", "PWAR4", "BJARMA11", "MHAR9", 
	"NileMin", "SB32"))
}

\arguments{
  \item{n}{
length of series
}
  \item{which}{
which model
}
}
\details{
BJAR1 is the AR(1) model fit to the sunspot series in BJR
BJAR2 is the AR(2) model fit to the sunspot series in BJR
BJAR3 is the AR(3) model fit to the sunspot series in BJR
BJAR2 is the AR(2) model fit to the sunspot series in BJR
PWAR4 is the AR(4) model, PW,
BJARMA11 is the ARMA(1,1) model fit to Series A in BJR
MHAR9 is the AR(9) model fit to the sunspot series in MHL
NileMin is ARFIMA(0,0,0), d=0.39
SB32 is ARTFIMA(0,0,0), d=5/8, lambda=0.045
}
\value{
vector of time series values
}
\references{
BJR) Box, Jenkins and Reinsel (2005), Table 7.11
PW) Percival and Walden, 1990, p.45
MHL) McLeod, Hipel and Lennox, 1978, p.581
}
\author{
A. I. McLeod
}

\seealso{
\code{\link{artsim}}
}
\examples{
z <- tseg(5000, "MHAR9")
arima(z, order=c(9,0,0), fixed=c(NA,NA,0,0,0,0,0,0,NA,NA), transform.pars=FALSE)
}

\keyword{ ts }
\keyword{ datagen }

