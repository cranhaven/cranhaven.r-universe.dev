\name{nilemin}
\alias{nilemin}
\docType{data}
\title{
Nile Annual Minima, 622 AD to 1284 AD
}
\description{
Annual Minimum flow of Nile River. See below for details.
}
\usage{data("nilemin")}
\format{
  The format is:
 Time-Series [1:663] from 622 to 1284: 11.57 10.88 11.69 11.69 9.84 ...
 - attr(*, "title")= chr "#Nile River minima series"
}
\details{
The minimum annual level of the Nile has been recorded over many 
centuries and was given by Toussoun (1925). The data over the 
period 622 AD to 1284 AD is considered more homogenous and reliable 
than the full dataset and has been analyzed by Beran (1994) and 
Percival and Walden (2000). 
The complete dataset is available StatLib Datasets - see:
hipel-mcleod archive, file: Minimum.

}
\source{
Toussoun, O. (1925). 
Memoire sur l'Histoire du Nil. 
In Memoires a l'Institut d'Egypte, 18, 366-404.
}
\references{
Beran, J. (1994). 
Statistics for Long-Memory Processes. Chapman and Hall, New York.

Percival, D.B. and Walden, A.T. (2000) 
Wavelet Methods for Time Series Analysis. 
Cambridge University Press.
}
\examples{
data(nilemin)
artfima(nilemin, likAlg="Whittle")
\dontrun{
#compare exact and Whittle using bestModel()
start <- proc.time()[3]
ans<-bestModel(nilemin)
tot <- proc.time()[3]-start
start <- proc.time()[3]
ansW <- bestModel(nilemin, likAlg="Whittle")
totW <- proc.time()[3]-start
t <- c(tot, totW)
names(t) <- c("exact", "Whittle")
#compare times - about 100 seconds vs 3 seconds
t
#compare best models
ans
ansW
#AIC/BIC scores similar but rankings to change.
#ARTFIMA(0,0,0) is ranked best by both AIC and BIC
#ARIMA(2,0,1) is ranked second best by both AIC and BIC
#ARFIMA(0,0,0) is ranked 3rd by BIC and is not among top 5 by AIC
}
}
\keyword{datasets}
