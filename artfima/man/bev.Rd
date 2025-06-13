\name{bev}
\alias{bev}
\docType{data}
\title{
Beveridge Wheat Price Index, 1500 to 1869
}
\description{
Beveridge Wheat Price Index which gives annual price data from 1500 to 1869.
}
\usage{data("bev")}
\format{
  The format is:
 Time-Series [1:370] from 1500 to 1869: 17 19 20 15 13 14 14 14 14 11 ...
}
\details{
Baille suggests the time series is overdifferenced and is best fit
by an ARFIMA model.
}
\source{
CRAN package tseries.
}
\references{
R. T. Baillie (1996): 
Long Memory Processes and Fractional Integration in Econometrics. 
Journal of Econometrics, 73, 5-59.
}
\examples{
data(bev)
#series needs a log transformation as is evident from the plot
plot(bev)
\dontrun{
w <- diff(bev)
bestModels(w)
}
}
\keyword{datasets}
