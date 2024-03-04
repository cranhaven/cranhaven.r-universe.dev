\name{am_call_bin_propdiv}
\alias{am_call_bin_propdiv}
\title{
Binomial price of an American call with an underlying stock that pays proportional dividends
}
\description{
Pricing of an American call stock option that pays proportional dividends
}
\usage{
am_call_bin_propdiv(S, K, r, sigma, t, steps, 
                dividend_times, dividend_yields)
}
\arguments{
\item{S}{spot price}
\item{K}{exercice price}
\item{r}{interest rate}
\item{sigma}{volatility}
\item{t}{time to maturity}
\item{steps}{number of steps in binomial tree}
\item{dividend_times}{periods when dividend is paid out}
\item{dividend_yields}{dividend yields in each period}
}

\details{
If the underlying asset is a stock paying dividends during the maturity of the option, the terms of the option are not adjusted to reflect this cash payment, which means that the option value will reflect the dividend payments. In the binomial model, the adjustment for dividends depends on whether the dividends are discrete or proportional. In this R package, we deal with the second case. To address this issue, we multiply the stock prices at the ex-dividend date by an adjustment term. Since the structure of the adjusted payoffs along the binomial tree is standard, we can again compute option price backward starting from the final states. 
}

\value{
\item{call_price }{Option price}
}
\references{
John Hull, "Options, Futures and other Derivative Securities", Prentice-Hall, second edition, 1993.
}
\author{
Paolo Zagaglia, paolo.zagaglia@gmail.com
}



\examples{
rm(list=ls())

S<-100 
K<-100
r<-0.10 
sigma<-0.25
t<-1
steps<-100
dividend_times<-matrix(c(0.25, 0.75))
dividend_yields<-matrix(c(0.025, 0.025)) 

call_price_bin_propdiv<-am_call_bin_propdiv(S, K, r, sigma, t, steps,
 dividend_times, dividend_yields)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ am_call_bin_propdiv }
