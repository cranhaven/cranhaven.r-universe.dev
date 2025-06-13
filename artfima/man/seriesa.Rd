\name{seriesa}
\alias{seriesa}
\docType{data}
\title{
Series A from Box and Jenkins
}
\description{
Chemical process concentration readings every two hours is comprised
of 197 consecutive observations.
Box and Jenkins fit ARMA(1,1) and ARIMA(0,1,1) to this data.
}
\usage{data("seriesa")}
\format{
  The format is:
 Time-Series [1:197] from 1 to 197: 17 16.6 16.3 16.1 17.1 16.9 16.8 17.4 17.1 17 ...
}

\source{
listed in Box and Jenkins book
}
\references{
Box and Jenkins (1970). Time Series Analysis: Forecasting and Control.
}
\examples{
data(seriesa)
#compare ARMA(1,1) models and timings
system.time(arima(seriesa, order=c(1,0,1)))
system.time(artfima(seriesa, arimaOrder=c(1,0,1)))
#Remark: there is a slight difference due to the fact that arima() 
#uses the exact MLE for the mean parameter whereas artfima() uses
#the sample average. In practice, the difference is almost negible.
#
#Find AIC/BIC 3 best models. Takes about 15 sec
\dontrun{
system.time(ans <- bestModels(seriesa, nbest=3))
summary(ans) #summary provides plausibility as well as scores
}
}
\keyword{datasets}
