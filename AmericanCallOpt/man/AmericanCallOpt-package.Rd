\name{AmericanCallOpt-package}
\alias{AmericanCallOpt-package}
\alias{AmericanCallOpt}
\docType{package}
\title{
Pricing of selected American call options with payoff from the underlying asset
}
\description{
This package includes pricing function for selected American call options with underlying assets that generate payouts. The functions cover the cases of binomial pricing for American options with continuous payout from the underlying asset or commodity and with an underlying stock that pays proportional dividends in discrete t. There are also functions for American calls on futures using a binomial approximation, for futures currency option using a binomial approximation, and for American perpetual call options. Each type of option is dealt with in a corresponding R function.
}
\details{
\tabular{ll}{
Package: \tab AmericanCallOpt\cr
Type: \tab Package\cr
Version: \tab 0.95\cr
Date: \tab 2012-03-02\cr
License: \tab GPL-3\cr
}
This package includes a set of pricing functions for American call options. The following cases are covered:
(1) Pricing of American call option using the standard binomial approximation;
(2) Hedge parameters for an American call option with a standard binomial tree;
(3) Binomial pricing of American option with continuous payout from the underlying asset;
(4) Binomial pricing of American option with an underlying stock that pays proportional dividends in discrete time;
(5) Pricing of American call option on futures using a binomial approximation;
(6) Pricing of futures currency option using a binomial approximation;
(7) Pricing of American perpetual call option.
The user should kindly notice that this material is for educational purposes only. The codes are not optimized for computational efficiency as they are meant to represent standard cases of analytical and numerical solution. 
}

\author{
Paolo Zagaglia

Maintainer: Paolo Zagaglia <paolo.zagaglia@gmail.com>
}

\references{
John Hull, "Options, Futures and other Derivative Securities", Prentice-Hall, second edition, 1993.
}

\examples{
#--------------------------------------------------------------------------
#
#  Example program for pricing different American call options using 
#  alternative methods
#
#  IMPORTANT: 
#  These programs are educational purpose only. The codes are not 
#  optimized for computational efficiency as they are meant to 
#  represent standard cases of anaytical and numerical solution. 
#
#--------------------------------------------------------------------------


rm(list=ls())


#--------------------------------------------------------------------------
# Price of American call option using a binomial approximation
S<-100
K<-100
r<-0.1
sigma<-0.25
t<-1
steps<-100

call_price_am_bin<-am_call_bin(S, K, r, sigma, t, steps)
#--------------------------------------------------------------------------


#--------------------------------------------------------------------------
# Hedge parameters for an American call option using a binomial tree
S<-100 
K<-100
r<-0.1 
sigma<-0.25
t<-1.0 
steps<-100

hedge<-am_call_partials(S, K, r, sigma, t, steps)
#--------------------------------------------------------------------------


#--------------------------------------------------------------------------
# Binomial American option price with continous payout from the 
# underlying commodity
S<-100 
K<-100
r<-0.10 
y<-0.02
sigma<-0.25
t<-1
steps<-100

call_price_bin_contpay<-am_call_bin_contpay(S, K, r, y, sigma, t, steps)
#--------------------------------------------------------------------------


#--------------------------------------------------------------------------
# Binomial price of an American option with an underlying stock that 
# pays proportional dividends in discrete t
S<-100 
K<-100
r<-0.10 
sigma<-0.25
t<-1
steps<-100
dividend_times<-matrix( c(0.25, 0.75) )
dividend_yields<-matrix( c(0.025, 0.025) )

call_price_bin_propdiv<-am_call_bin_propdiv(S, K, r, sigma, t, 
				steps, dividend_times, dividend_yields)
#--------------------------------------------------------------------------


#--------------------------------------------------------------------------
# Pricing an american call option on futures using a binomial approximation
F<-50 
K<-45
r<-0.08 
sigma<-0.2
t<-0.5
steps<-100

call_price_bin_futures<-am_call_bin_futures(F, K, r, sigma, t, steps)
#--------------------------------------------------------------------------


#--------------------------------------------------------------------------
# Pricing a futures currency option using a binomial approximation
S<-50 
K<-52
r<-0.08 
r_f<-0.05
sigma<-0.2 
t<-0.5
steps<-100

call_price_bin_currency<-am_call_bin_currency(S, K, r, r_f, sigma, t, steps)
#--------------------------------------------------------------------------


#--------------------------------------------------------------------------
# Price for an American perpetual call option
S<-50.0 
K<-40.0
r<-0.05 
y<-0.02
sigma<-0.05

call_price_perpetual<-am_call_perpetual(S, K, r, y, sigma)
#--------------------------------------------------------------------------


}
