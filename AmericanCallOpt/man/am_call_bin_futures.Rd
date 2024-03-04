\name{am_call_bin_futures}
\alias{am_call_bin_futures}
\title{
Binomial pricing of a futures American call 
}
\description{
Pricing of American call option on futures using a binomial approximation
}
\usage{
am_call_bin_futures(F, K, r, sigma, t, steps)
}
\arguments{
\item{F}{price of futures contract}
\item{K}{exercise price}
\item{r}{risk-free interest rate}
\item{sigma}{volatility}
\item{t}{time to maturity}
\item{steps}{number of steps in binomial tree}
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

F<-50 
K<-45
r<-0.08 
sigma<-0.2
t<-0.5
steps<-100

call_price_bin_futures<-am_call_bin_futures(F, K, r, sigma, t, steps)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ am_call_bin_futures }
