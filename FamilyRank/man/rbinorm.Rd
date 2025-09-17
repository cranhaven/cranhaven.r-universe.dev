\name{rbinorm}
\alias{rbinorm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Bimodal Normal Distribution}
\description{Simulates random data from a bimodal Gaussian distribution.}
\usage{
rbinorm(n, mean1, mean2, sd1, sd2, prop)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{Number of observations to simulate}
  \item{mean1}{Mean of mode 1}
  \item{mean2}{Mean of mode 2}
  \item{sd1}{Standard deviation of mode 1}
  \item{sd2}{Standard deviation of mode 2}
  \item{prop}{Probability of being in mode 1. \code{1 - prop} is the probability of being in mode 2.}
}
\details{This function is modeled off of the \code{\link{rnorm}} function.}
\value{Generates random deviates}

\author{Michelle Saul}
\examples{
## Generate 100 samples from a two component Guassian curve
samples <- rbinorm(n=100, mean1=10, mean2=20, sd1=1, sd2=2, prop=.5)

## Plot distribution of simulated data
plot(density(samples))
}
