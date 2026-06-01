\name{exch}
\alias{exch}
\docType{data}
\title{Pound-dollar Exchange-rate Data
}
\description{
Harvey et al. (1994) presented daily observations for the weekday closing exchange rates for
the U.K. Sterling/U.S. Dollar from 1/10/81 to 28/6/85. 
}
\usage{data("exch")}
\format{
  A data frame with 944 observations on the following 4 variables.
  \describe{
    \item{\code{rt}}{exchange rate at time t}
    \item{\code{yt}}{mean-correted returns}
    \item{\code{yt1}}{one lag of yt}
    \item{\code{yt12}}{yt1*yt1}
  }
}
\references{
Harvey, A.C., Ruiz, E. and Shephard, N. (1994). Multivariate stochastic
variance models. Review of Economic Studies, 61, 247--264.
}

