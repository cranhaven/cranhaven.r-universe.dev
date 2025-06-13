\name{risk.attrib.Copula}
\alias{risk.attrib.Copula}

\title{Risk Attribution of a Portfolio with t-Copula}

\description{
Combined representation of the risk attributes MCTR, CCTR, Portfolio Volatility, Portfolio Value at Risk (VaR) and individual Volatility of the stocks in a given portfolio for a Markowitz's Optimized weights using t-Copula.
}

\usage{
risk.attrib.Copula(tickers, data, start, end, sim.size=1000, df=10)
}

\arguments{
  \item{tickers}{
A character vector of ticker names of companies in the portfolio.
}
  \item{data}{
A \code{zoo} object whose \code{rownames} are dates and \code{colnames} are ticker names of the companies. Values of the table corresponds to the daily returns of the stocks of corresponding ticker names.
}
  \item{start}{
Start date in the format "yyyy-mm-dd".
}
  \item{end}{
End date in the format "yyyy-mm-dd".
} 
  \item{sim.size}{
Simulation size. Default at 1000.
}
  \item{df}{
Degrees of freedom for t-Copula. Default set at 10.  
}
}

\details{
It calculate portfolio Value at Risk after fitting t-Copula with empirical distribution on marginals. It simulate returns from the fitted t-Copula and uses Markowitz's Optimized weight.
}

\value{
Returns a list of following objects:
  \item{Volatility}{Data frame caontaining Markowitz's optimized weights, individual stock's volatility, MCTR, CCTR for the given tickers.
  }
  \item{Portfolio Volatility}{Portfolio Volatility}
  \item{Portfilio VaR}{Portfolio Value at Risk}
}

\seealso{
\code{\link{volatility}}, 
\code{\link{portvol}}, 
\code{\link{mctr}}, 
\code{\link{cctr}}, 
\code{\link{zoo}}
}

\examples{
# load the data 'SnP500Returns'
data(SnP500Returns)

# consider the portfolio containing the stocks of the companies
# Apple, IBM, Intel, Microsoft
pf <- c("AAPL","IBM","INTC","MSFT")

# risk attribution for the portfolio 'pf' 
# for the time period January 1, 2013 - January 10, 2013
st<-"2013-01-01"
ed<-"2013-10-10"
risk.attrib.Copula(tickers = pf, data = SnP500Returns,
                    start = st, end = ed,
                    sim.size=1000, df=10)
}
