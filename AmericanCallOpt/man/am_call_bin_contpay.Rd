\name{am_call_bin_contpay}
\alias{am_call_bin_contpay}
\title{
Binomial option price with continuous payout from the underlying asset
}
\description{
Pricing of an American call option with continuous payout from the underlying asset using a binomial approximation
}
\usage{
am_call_bin_contpay(S, K, r, y, sigma, t, steps)
}
\arguments{
  \item{S}{spot price}
  \item{K}{exercise price}
  \item{r}{risk-free interest rate}
  \item{y}{continuous payout}
  \item{sigma}{volatility}
  \item{t}{time to maturity}
  \item{steps}{number of steps in binomial tree}
}

\details{
With this type of option, the underlying asset provides payouts at each period in time. The payoff structure simplifies the computation to a major extent and makes this a case similar to the one of pricing through Black-Scholes.
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
y<-0.02
sigma<-0.25
t<-1
steps<-100

call_price_bin_contpay<-am_call_bin_contpay(S, K, r, y, sigma, t, steps)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ am_call_bin_contpay }

