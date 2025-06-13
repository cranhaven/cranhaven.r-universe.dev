\name{portvol, mctr, cctr}
\alias{portvol}
\alias{mctr}
\alias{cctr}

\title{Portfolio Volatility and Contribution to Total Volatility Risk (MCTR & CCTR)}

\description{
\code{portvol} computes portfolio volatility of a given portfolio for specific weight and time period. \code{mctr} & \code{cctr} computes the Marginal Contribution to Total Risk (MCTR) & Conditional Contribution to Total Risk (CCTR) for the given portfolio.
}

\usage{
portvol(tickers, weights = rep(1,length(tickers)),
        start, end, data)

mctr(tickers, weights = rep(1,length(tickers)),
        start, end, data)

cctr(tickers, weights = rep(1,length(tickers)),
        start, end, data)
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
}

\details{
As any portfolio can be considered as bag of \eqn{p}-many risky assets, it is important to figureout how these assets contributes to total volatility risk of the portfolio. We consider an investment period and suppose \eqn{r_j}{rj} denote return to source \eqn{j} for the same period, where \eqn{j = 1, 2,\ldots, p}. The portfolio return over the period is
\deqn{R_p = \sum_{j=1}^{p} w_j r_j}{Rp = \sum wj rj , j = 1, 2,\ldots, p}
where \eqn{w_j}{wj} is the portfolio exposure to the asset \eqn{j}, i.e., portfolio weight, such that \eqn{w_j \ge 0}{wj \ge 0} and \eqn{\sum_{j=1}^{p} w_j = 1}{\sum wj = 1}. Portfolio manager determines the size of \eqn{w_j}{wj} at the beginning of the investment period. Portfolio volatility is defined as 
\deqn{\sigma = \sqrt{w^T \Sigma w}}{\sigma = \sqrt(w' \Sigma w)}
where \eqn{w = (w_1, w_2,\ldots, w_p)}{w = (w1, w2,\ldots, wp)} and \eqn{\Sigma} being the variance-covariance matrix of the assets in the portfolio. The weights (\eqn{w_j}{wj}) are the main switches of portfolio's total volatility. Therefore, it is important for a manager to quantify, the sensitivity of the portfolio's volatility with respect to small change in \eqn{w}. This can be achieved by differentiating the portfolio volatility with respect to \eqn{w},
\deqn{\frac{\partial \sigma}{\partial w} = \frac{1}{\sigma} \Sigma w = \rho}{\delta\sigma/\deltaw = 1/\sigma \Sigma w = \rho}
where \eqn{\rho = (\rho_1, \rho_2,\ldots, \rho_p)}{\rho = (\rho1, \rho2,\ldots, \rhop)} is know as 'Marginal Contribution to Total Risk' (MCTR). Note that MCTR of asset \eqn{i} is
\deqn{\rho_i = \frac{1}{\sigma} \sum_{j=1}^{p} \sigma_{ij} w_j.}{\rhoi = 1/\sigma \sum \sigmaij wj , j = 1, 2,\ldots, p.}
The CCTR (aka. Conditional Contribution to Total Risk) is the amount that an asset add to total portfolio volatility. In other words, \eqn{\xi_i = w_i \rho_i}{\xii = wi \rhoi} is the CCTR of asset \eqn{i}, i.e.,
\deqn{\sigma = \sum_{i=1}^{p} w_i \rho_i.}{\sigma = \sum wi \rhoi , i = 1, 2,\ldots, p.}
Therefore portfolio volatility can be viewed as weighted average of MCTR.
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
