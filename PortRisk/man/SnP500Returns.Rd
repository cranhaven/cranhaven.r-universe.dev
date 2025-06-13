\name{SnP500Returns}
\alias{SnP500Returns}
\docType{data}
\title{Daily Returns of S&P500 Stocks in 2013}
\description{
Daily log returns corresponding to the ticker names of the stocks of the companies listed in S&P500 List in the year 2013.
}
\usage{data(SnP500Returns)}
\format{
  The format is \code{\link{zoo}} series from 2013-01-02 to 2013-12-31.
\code{rownames} are the dates in the format "yyyy-mm-dd" and \code{colnames} are the ticker names of the stocks.
}

\source{Yahoo Finance <\url{http://finance.yahoo.com}>}

\seealso{
\code{\link{access}} to pick a block from this large \code{\link{zoo}} series.
}

\examples{
data(SnP500Returns)
}
\keyword{datasets}
