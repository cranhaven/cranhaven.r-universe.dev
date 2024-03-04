\name{am_call_bin_currency}
\alias{am_call_bin_currency}
\title{
Binomial pricing of an American call on currency futures
}
\description{
Pricing of currency futures American option using a binomial approximation
}
\usage{
am_call_bin_currency(S, K, r, r_f, sigma, t, steps)
}
\arguments{
\item{S}{spot price}
\item{K}{exercice price}
\item{r}{domestic interest rate}
\item{r_f}{foreign interest rate}
\item{sigma}{volatility}
\item{t}{time to maturity}
\item{steps}{number of steps in binomial tree}
}

\details{
American options written on foreign currencies are priced using a standard binomial tree. The notable point is that early exercise is driven by the difference between national interest rates. 
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

S<-50 
K<-52
r<-0.08 
r_f<-0.05
sigma<-0.2 
t<-0.5
steps<-100

call_price_bin_currency<-am_call_bin_currency(S, K, r, r_f, sigma, t, steps)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ am_call_bin_currency }
