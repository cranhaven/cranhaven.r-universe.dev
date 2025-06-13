\name{risk.attribution}
\alias{risk.attribution}

\title{Risk Attribution of a Portfolio}

\description{
Combined representation of the risk attributes MCTR, CCTR, CCTR percentage, Portfolio Volatility and individual Volatility of the stocks in a given portfolio for a given weight and time period.
}
\usage{
risk.attribution(tickers, weights = rep(1,length(tickers)),
                  start, end, data, CompanyList = NULL)
}

\arguments{
  \item{tickers}{
A character vector of ticker names of companies in the portfolio.
}
  \item{weights}{
A numeric vector of weights assigned to the stocks corresponding to the ticker names in \code{tickers}. The sum of the weights need not to be 1 or 100 (in percentage). By default, equal weights to all the stocks are assigned (i.e., by \code{rep(1, length(tickers))}).
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
  \item{CompanyList}{
A dataframe containing all the Company names corresponding to the ticker names as its \code{rownames}. The input for this argument is optional.
}
}
\details{
For details of the risk attributes refer to the corresponding functions. See \code{\link{volatility}} for individual volatility of the stocks and \code{\link{portvol}} for portfolio volatility, MCTR & CCTR.\deqn{}
CCTR percentage for a stock in the portfolio is defined as the percentage of the portfolio volatility contributed by that stock for the given weight. i.e.,
\deqn{CCTR(\%) = \frac{CCTR}{\sigma}*100}{CCTR(\%) = CCTR/\sigma *100} where \eqn{\sigma} is the portfolio volatility.
}
\value{
Returns a dataframe with \code{rownames} as the ticker names as given in the input \code{tickers} with the last row corresponding to the portfolio values. The result contains the following columns:
  \item{Company Name}{Optional. Available only if the dataframe with the company names corresponding to the ticker names as \code{rownames} is supplied as input in \code{risk.attribution} for the argument \code{CompanyList}.}
  \item{Weight}{Standardized value of the weights assigned to the stocks in the portfolio. Value of this column corresponding to portfolio is the sum of the weights (i.e. 1).}
  \item{MCTR}{Marginal Contribution to Total Risk (MCTR) in percentage. MCTR corresponding to the portfolio will be shown as \code{NA}, since it is meaningless.}
  \item{CCTR}{Conditional Contribution to Total Risk (CCTR) in percentage. CCTR corresponding to the portfolio is the sum of the CCTR values, which is the portfolio volatility.}
  \item{CCTR(\%)}{Percentage of the portfolio volatility contributed by the stock for the given weight. Clearly, CCTR percentage corresponding to the portfolio is 100.}
  \item{Volatility}{Individual volatility of the stocks in percentage. Note that, the value of this column corresponding to the portfolio is not the sum of this column. It is the portfolio volatility.}
}

\note{
In the result or output (see example), both the values of the last row (Portfolio) corresponding to the columns CCTR and Volatility are same (Portfolio Volatility). It should also be noted that, Portfolio Volatility is the sum of CCTR values corresponding to all the stocks but not the sum of individual Volatility of the stocks.
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

# suppose the amount of investments in the above stocks are
# $10,000, $40,000, $20,000 & $30,000 respectively
wt <- c(10000,40000,20000,30000) # weights

# risk attribution for the portfolio 'pf' with weights 'wt'
# for the time period January 1, 2013 - January 31, 2013
risk.attribution(tickers = pf, weights = wt,
                  start = "2013-01-01", end = "2013-01-31",
                  data = SnP500Returns)

# to attach the company names corresponding to the ticker names
# load the dataset containing the company names
data(SnP500List)
risk.attribution(tickers = pf, weights = wt,
                  start = "2013-01-01", end = "2013-01-31",
                  data = SnP500Returns, CompanyList = SnP500List)
}
