\name{portvol.Bayes, mctr.Bayes, cctr.Bayes}
\alias{portvol.Bayes}
\alias{mctr.Bayes}
\alias{cctr.Bayes}

\title{Portfolio Volatility and Contribution to Total Volatility Risk (MCTR & CCTR): Bayesian Approach}

\description{
\code{portvol.Bayes} computes portfolio volatility of a given portfolio for specific weight and time period. \code{mctr.Bayes} & \code{cctr.Bayes} computes the Marginal Contribution to Total Risk (MCTR) & Conditional Contribution to Total Risk (CCTR) for the given portfolio.
}

\usage{
portvol.Bayes(tickers, weights = rep(1,length(tickers)),
              start, end, data, sim.size = 1000)

mctr.Bayes(tickers, weights = rep(1,length(tickers)),
              start, end, data, sim.size = 1000)

cctr.Bayes(tickers, weights = rep(1,length(tickers)),
              start, end, data, sim.size = 1000)
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

  \item{sim.size}{
Simulation size, default 1000.
  }
}

\details{
As any portfolio can be considered as bag of \eqn{p}-many risky assets, it is important to figureout how these assets contributes to total volatility risk of the portfolio. We consider an investment period and suppose \eqn{r_j}{rj} denote return to source \eqn{j} for the same period, where \eqn{j = 1, 2,\ldots, p}. The portfolio return over the period is
\deqn{R_p = \sum_{j=1}^{p} w_j r_j}{Rp = \sum wj rj , j = 1, 2,\ldots, p}
where \eqn{w_j}{wj} is the portfolio exposure to the asset \eqn{j}, i.e., portfolio weight, such that \eqn{w_j \ge 0}{wj \ge 0} and \eqn{\sum_{j=1}^{p} w_j = 1}{\sum wj = 1}. Portfolio manager determines the size of \eqn{w_j}{wj} at the beginning of the investment period. Portfolio volatility is defined as 
\deqn{\sigma = \sqrt{w^T \Sigma w}}{\sigma = \sqrt(w' \Sigma w)}
where \eqn{w = (w_1, w_2,\ldots, w_p)}{w = (w1, w2,\ldots, wp)} and \eqn{\Sigma} being the variance-covariance matrix of the assets in the portfolio.
\eqn{S} is the sample portfolio-covariance matrix. If \deqn{S \sim Wishart(n-1,\Sigma)}{S ~ Wishart(n-1,\Sigma)} and prior distribution on \eqn{\Sigma} is \deqn{\Sigma \sim Inv-Wishart(n_0,\Psi)}{\Sigma ~ Inv-Wishart(n_0,\Psi)}
Then posterior distribution is \deqn{\Sigma | S \sim Inv-Wishart(n_0+n-1,\Psi+S)}{\Sigma | S ~ Inv-Wishart(n_0+n-1,\Psi+S)}
For more detail, see \code{\link{portvol}}, \code{\link{mctr}}, \code{\link{cctr}}
}

\value{
  \item{portvol}{A numeric value. Volatility of a given portfolio in percentage.}
  \item{mctr}{A named numeric vector of Marginal Contribution to Total Risk (MCTR) in percentage with names being the ticker names.}
  \item{cctr}{A named numeric vector of Conditional Contribution to Total Risk (CCTR) in percentage with names being the ticker names.}
}

\seealso{
\code{\link{zoo}}
}

\examples{
data(SnP500Returns)

# consider the portfolio containing the first 4 stocks
pf <- colnames(SnP500Returns)[1:4]

st <- "2013-01-01" # start date
en <- "2013-01-31" # end date

# suppose the amount of investments in the above stocks are
# $1,000, $2,000, $3,000 & $1,000 respectively
wt <- c(1000,2000,3000,1000) # weights

# portfolio volatility for the portfolio 'pf' with equal (default) weights
pv1 <- portvol(pf, start = st, end = en,
                data = SnP500Returns)

# portfolio volatility for the portfolio 'pf' with weights as 'wt'
pv2 <- portvol(pf, weights = wt, start = st, end = en,
                data = SnP500Returns)

# similarly,
# mctr for the portfolio 'pf' with weights as 'wt'
mc <- mctr(pf, weights = wt, start = st, end = en,
            data = SnP500Returns)

# cctr for the portfolio 'pf' with weights as 'wt'
cc <- cctr(pf, weights = wt, start = st, end = en,
            data = SnP500Returns)

sum(cc) == pv2
# note that, sum of the cctr values is the portfolio volatility
}
