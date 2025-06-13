\name{volatility}
\alias{volatility}

\title{Individual Volatility of Stock(s)}

\description{
Volatility of one or more stock(s) for a given time period.
}

\usage{
volatility(tickers, start, end, data)
}

\arguments{
  \item{tickers}{
List of ticker names of companies. A character vector.
}
  \item{start}{
Start date in the format "yyyy-mm-dd".
}
  \item{end}{
End date in the format "yyyy-mm-dd".
}
  \item{data}{
A \code{zoo} object whose \code{rownames} are dates and \code{colnames} are ticker names of the companies. Values of the table corresponds to the daily returns of the stocks of corresponding ticker names.
}
}

\details{
Volatility of a given stock for a time period is defined as the standard deviation of the returns of that stock in that time period.
}

\value{
A named numeric vector of volatility in percentage with names being the ticker names of the stocks given as input in \code{tickers}.
}

\seealso{
\code{\link{zoo}}
}

\examples{
data(SnP500Returns)
tckk <- colnames(SnP500Returns)

# volatility of the stock of the company Apple
# for the time period January 1, 2013 - January 31, 2013
volatility("AAPL", start = "2013-01-01",
            end = "2013-01-31", data = SnP500Returns)

# volatility of the first three stocks in SnP500Returns
# for the time period January 1, 2013 - January 31, 2013
volatility(tickers = tckk[1:3], start = "2013-01-01",
            end = "2013-01-31", data = SnP500Returns)
}
