\name{am_call_perpetual}
\alias{am_call_perpetual}
\title{
Analytical pricing of an American perpetual call
}
\description{
Pricing of an American perpetual call option using an analytical formula
}
\usage{
am_call_perpetual(S, K, r, y, sigma)
}
\arguments{
  \item{S}{spot price}
  \item{K}{exercise price}
  \item{r}{risk-free interest rate}
  \item{y}{dividend yield from underlying asset}
  \item{sigma}{volatility}
}

\details{
A perpetual option is one with no maturity date. This obviously applies only to the case of American-style options. Analytical formulas are available in this case both for call and put options. 
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

S<-50.0 
K<-40.0
r<-0.05 
y<-0.02
sigma<-0.05

call_price_perpetual<-am_call_perpetual(S, K, r, y, sigma)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ am_call_perpetual }
