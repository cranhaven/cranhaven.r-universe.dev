\name{FinAsym-package}
\alias{FinAsym-package}
\alias{FinAsym}
\docType{package}
\title{
Classifies implicit trading activity from market quotes and computes the probability of informed trading
}
\description{
This package uses market quotes to classify implicit buy and sell-initiated trades in OTC markets through the algorithm of Lee and Ready (1991). Based on information for trade initiation, the package computes the probability of informed trading of Easley and O'Hara (1987).
}
\details{
\tabular{ll}{
Package: \tab FinAsym\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-04-02\cr
License: \tab GPL-3\cr
LazyLoad: \tab yes\cr
}
The package consists of two R functions. The classification of implicit trade origination from quotes is produced by classify_quotes.R. This functions takes two inputs, namely a matrix with as many rows as the number of intraday quotes, and two columns. It also requires the user to specify which column numbers include the bid and ask prices. The likelihood function for the PIN model is coded in the function pin_likelihood.R. This takes as inputs the parameter values - in the order of epsilon, mu, alpha, delta - and the time series of daily data on number of no trades, buy- and sell-initiated trades.
}
\author{
Maintainer: Paolo Zagaglia <paolo.zagaglia@gmail.com>
}
\references{
D. Easley and M. O'Hara. Price, Trade Size, and Information in Securities Markets. Journal of Financial Economics, 19(1), 69-90, 1987.

C. M. C. Lee and M. J. Ready. Inferring Trade Direction from Intraday Data. Journal of Finance, 46(2), 733-746, 1991.
}


\keyword{ package }
\examples{
#Sample data
mydata<-matrix(c(4.56, 4.7, 4.57, 4.64, 4.53, 
4.65, 4.59, 4.66, 4.55, 4.65, 4.59, 4.66, 4.59, 
4.66, 4.55, 4.65, 4.55, 4.65, 4.55, 4.65, 4.59, 
4.66, 4.55, 4.65, 4.59, 4.66, 4.59, 4.66), 
nrow=14, byrow=TRUE)

#Trading day
gname <- 'June 29 2006'
	
ind_bid <- c(1)
ind_ask <- c(2)
	
quotes_types <- classify_quotes(mydata, ind_bid, 
ind_ask, gname)

#Again, sample data for 15 trading sessions
qdata      <- matrix(0, 3 ,1)
qdata[1]   <- 9
qdata[2]   <- 6
qdata[3]   <- 13
classified <- matrix(qdata,3,15)

#Make a starting guess at the solution
par0 <- c(0.5, 0.5, 0.5, 0.5)  

param_optim <- optim(par0, pin_likelihood, 
gr=NULL, classified)

alph <- c( param_optim$par[3] )
miu  <- c( param_optim$par[2] )
delt <- c( param_optim$par[4] )
epsi <- c( param_optim$par[1] )

PSI <- c( ( 1-miu )*epsi + miu )
pin <- c( miu/PSI )

msg_pin <- paste("The average PIN over the 
trading period is:", pin, sep=" ")

print( msg_pin )
}
