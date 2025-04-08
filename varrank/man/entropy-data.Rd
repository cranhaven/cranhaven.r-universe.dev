% entropy-data.Rd ---
% Author           : Gilles Kratzer
% Created on :       28.09.2017
% Last modification :
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{entropy.data}
\alias{entropy.data}
\title{Computes an Empirical Estimation of the Entropy from a Table of Counts}

\usage{
entropy.data(freqs.table)
}
\arguments{
  \item{freqs.table}{a table of counts.}}
\description{
This function empirically estimates the Shannon entropy from a table of counts using the observed frequencies.
}

\details{The general concept of entropy is defined for probability distributions. The 'entropy.data' function estimates empirical entropy from data. The probability is estimated from data using frequency tables. Then the estimates are plug-in in the definition of the entropy to return the so-called empirical entropy. A common known problem of empirical entropy is that the estimations are biased due to the sampling noise. This is also known that the bias will decrease as the sample size increases.}

\value{Shannon's entropy estimate on natural logarithm scale.}

\author{Gilles Kratzer}

\examples{
library("varrank")

rv <- rnorm(n = 100, mean = 0, sd = 2)

entropy.data(freqs.table = discretization(data.df = rv,
discretization.method = "fd",
frequency = TRUE)[[1]])
}
