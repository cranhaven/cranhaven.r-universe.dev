\name{plot_FV_pre_norm_kmom}
\encoding{WINDOWS-1252}
\alias{plot_FV_pre_norm_kmom}
\title{Plot the final expected value
of an \eqn{n}-payment annuity, with payments of 1 unit each made at
the beginning of every year (annuity-due), valued at the rate \eqn{X},
using the estimated moments of the normal distribution. 
}
\usage{
plot_FV_pre_norm_kmom(data,years,lwd,lty)
}
\arguments{\item{data}{ A vector of interest rates. }
\item{years}{ The number of years of the income. Default is 10 years.}
\item{lwd}{The width of the curve. Default is 1.5. }
\item{lty}{The style of the curve. Default is 1. }
}

\description{
Plot the final expected value
of an \eqn{n}-payment annuity, with payments of 1 unit each made at
the beginning of every year (annuity-due), valued at the rate \eqn{X},
using the estimated moments of the normal distribution.

}

\author{
Salvador Cruz Rambaud, Fabrizio Maturo, Ana María Sánchez Pérez 
}



\examples{

# example 1
data<-rnorm(n=30,m=0.03,sd=0.01)
plot_FV_pre_norm_kmom(data,8)


# example 2
data<-rnorm(n=200,m=0.075,sd=0.2)
plot_FV_pre_norm_kmom(data,8)




}

