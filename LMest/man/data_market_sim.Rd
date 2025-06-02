\name{data_market_sim}
\alias{data_market_sim}

\docType{data}

\title{Marketing dataset}


\description{Simulated dataset related to customers of four different brands along with the prices of each transaction.}

\usage{data(data_market_sim)}

\format{
  A data frame with 200  observations on the following variables:
  \describe{
   \item{\code{id}}{subject id.}
    \item{\code{time}}{occasion of observation.}
   \item{\code{brand}}{0 if the customer has purchased the product from brand A, 1 if brand B, 2 if brand C, 3 if brand D.}
   \item{\code{price}}{0 if the price of the
transaction is in the range [0.1, 10], 1  if it is in (10, 30], 
2  if it is in (30, 60], 3  if it is in (30, 100],  4  if it is in (100, 500] (in thousands of Euros).}
   \item{\code{age}}{age of the customer in years}
   \item{\code{income}}{income declared by the customer at the time of the first purchase (in thousands of Euros).}
  }
}


\references{ Pennoni, F., Pandolfi, S. and Bartolucci, F. (2024), LMest: An R Package for Estimating Generalized Latent Markov Models, \emph{Submitted to the R Journal}, pp. 1-30.}

\examples{data(data_market_sim)
}

\keyword{datasets}
