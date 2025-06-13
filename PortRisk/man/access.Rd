\name{access}
\alias{access}

\title{Access Daily Stock Returns by Dates}

\description{
Access data from a \code{zoo} type daily returns table and returns as a \code{zoo} object. Basically, it returns a table of daily returns of a given list of company ticker names for a time period given as the input.
}

\usage{
access(tickers, start, end, data)
}

\arguments{
  \item{tickers}{
A character vector of ticker names of companies in the portfolio.
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

\value{
Returns a \code{zoo} series as a table of daily returns corresponding to the company ticker names in \code{tickers} for the given time period. Basically, it picks up a block from a large table of daily returns of the stocks corresponding to the dates.
}

\seealso{
\code{\link{zoo}}
}

\examples{
data(SnP500Returns)

# list all the ticker names in a character vector
tckk <- colnames(SnP500Returns)

# access the data corresponding to the first 3 ticker names
# for the time period January 1, 2013 - January 10, 2013
access(tickers = tckk[1:3],
        start = "2013-01-01",
        end = "2013-01-10",
        data = SnP500Returns)
}