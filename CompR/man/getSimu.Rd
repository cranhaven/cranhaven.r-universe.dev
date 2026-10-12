\name{getSimu}
\alias{getSimu}
\title{
Gets the results of Likelihood Ratio Test.
}
\description{
Gets the results of Likelihood Ratio Test obtained by Monte-Carlo simulations.
}
\usage{
getSimu(object)
}
\arguments{
  \item{object}{
An object of class \code{LvrRatio}}
}
\value{
A matrix with the number of classes under H0, the values of Loglikelihood under H0 and H1 and the differences between these Loglikelihoods.
}
