\name{Factors}
\alias{Factors}
\docType{data}
\title{
Cryptocurrency Returns and Volume Data
}
\description{
The data set includes lagged log returns, lagged volume denominated in Bitcoin and forward log returns aggregated every 24-hours for a  cross-section of 26 cryptocurrency pairs from the 1st January 2017 to 9th September 2018.  The data was downloaded from CryptoCompare - a free API accessible at \url{https://min-api.cryptocompare.com} 
}
\usage{data("Factors")}
\format{A list of three xts objects including lagged returns (R.Lag), lagged volumes (V.Lag) and forward returns (R.Forward).}
\source{
\url{https://min-api.cryptocompare.com} 
}

\examples{
# Load data
data(Factors)
# Unlist the data
R.Forward = Factors[[1]]; R.Lag = Factors[[2]]; V.Lag = Factors[[3]]
head(V.Lag[1:5,1:5])
}
\keyword{datasets}
