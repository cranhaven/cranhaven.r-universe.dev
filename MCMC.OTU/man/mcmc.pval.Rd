\name{mcmc.pval}
\alias{mcmc.pval}
\title{
calculates p-value based on Bayesian z-score or MCMC sampling
}
\description{
Accessory function to OTUsummary function
}
\usage{
mcmc.pval(dat, testlim = 0, sided = 2, ptype="z")
}
\arguments{
  \item{dat}{
a table of MCMC samples
}
  \item{testlim}{
positive value to test whether the posterior crosses it (for variance components)
}
  \item{sided}{
sidedness of the test
}
  \item{ptype}{
'z' for z-score based p-values, 'mcmc' for standard sampling based p-values
}
}
\value{
A vector of calculated Bayesian p-values. For standard 'mcmc' p=values, the minimal possible value is 1/(MCMC sample size).
z-score based p-values are useful to approximate very low p-values with limited MCMC sample sizes.
}
\author{
Mikhail Matz, UT Austin
<matz@utexas.edu>
}
