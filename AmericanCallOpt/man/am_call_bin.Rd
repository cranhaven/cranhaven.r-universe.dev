\name{am_call_bin}
\alias{am_call_bin}
\title{
Binomial pricing of a standard American call
}
\description{
Pricing of American call option using a binomial approximation
}
\usage{
am_call_bin(S, K, r, sigma, t, steps)
}
\arguments{
  \item{S}{spot price}
  \item{K}{exercise price}
  \item{r}{risk-free interest rate}
  \item{sigma}{volatility}
  \item{t}{time to maturity}
  \item{steps}{number of steps in binomial tree}
}

\details{

The valuation problem of an American option is not trivial because, due to the payoff structure, it may be optimal to use (exercise) the option before the final date. This optimal exercise policy will affect the value of the option. However, the exercise policy is not known. There is therefore no general analytical solution for American options. There are some special cases, though. For American call options on assets that do not have any payouts, the American call price is the same as the European one, since the optimal exercise policy is to not exercise.

Allowing for the possibility of early exercise of American options makes binomial approximations useful from a valuation perspective. At each node we calculate the value of the option as a function of prices in the subsequent periods. At each point in time, we check for the value exercising of exercising the option. 

In the application presented in this package, I use a binomial tree. To find the option price, the algorithm rolls backwards starting from the final node. At node t, it calculate the call price as a function of two possible outcomes at time t+1. 

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
r<-0.1
sigma<-0.25
t<-1
steps<-100

call_price_am_bin<-am_call_bin(S, K, r, sigma, t, steps)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ am_call_bin }
