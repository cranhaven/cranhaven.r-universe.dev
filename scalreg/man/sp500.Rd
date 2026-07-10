\name{sp500}
\docType{data}
\alias{sp500}
\title{sp500}
\description{
 The \code{sp500} datafile contains a year's worth of close-of-day data for most of the Standard and Poors 500 stocks. The data is in reverse chronological order, 
 with the top row being Dec 31st, 2008.
}
\usage{sp500}
\format{
  This data file contains the following items:
  \describe{
    \item{sp500.2008}{The raw close-of-day data. The first column is of the DJIA index, the second is the S&P 500 index, the rest are individual labeled stocks.}
    \item{sp500.percent}{The daily percentage change.}
  }
}

\references{
This database was used in the R package "plus".
}
\examples{
## See examples in scalreg
}
\keyword{datasets}
