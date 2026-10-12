\name{assets}
\alias{assets}
\docType{data}
\title{Returns of the daily closing prices of assets, Standard and Poors 500 Index and T-bill rates}
\description{Returns for all the trading days from November 1, 1993 to April 3, 2003.}

\usage{data("assets")}
\format{
  A data frame with 2363 observations on the following 8 variables.
  \describe{
    \item{\code{tbill}}{a vector of returns of the T-bill rates for the trading days.}
    \item{\code{msf}}{a vector of returns of the daily closing prices of Microsoft Corporation.}
    \item{\code{sp500}}{a vector of returns of the Standard and Poors 500 index for the trading days.}
    \item{\code{ge}}{a vector of returns of the daily closing prices of General Electric Company.}
    \item{\code{ford}}{a vector of returns of the daily closing prices of Forward Industries Inc.}
    \item{\code{day}}{day of trading.}
    \item{\code{month}}{month of trading.}
    \item{\code{year}}{year of trading.}
  }
}

\references{
Ruppert, D (2004) \emph{Statistics and Finance}. Springer, New York.

Venables, W. N. and Ripley, B. D. (2002) \emph{Modern Applied Statistics with S}. Fourth edition. Springer.
}

