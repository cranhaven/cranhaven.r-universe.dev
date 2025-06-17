\name{hybrid}
\alias{hybrid}
\title{Hybrid data}
\usage{
hybrid
}
\description{
Hybrid data is taken from Weerahandi (1995) where the goal is to compare four means of corn yields by four hybrids: A, B, C, D.

An agricultural research scientist is interested in comparing four hybrids of corn. The four corn hybrids were planted in a random order in 22 plots of equal size and fairly homogeneous soil conditions. A set of data on yield from corn hybrids obtained from the experiment.

The usual P-value based on the assumption of equal population within hybrid variances (F statistic 1.841) is 0.176, thus leading to acceptance of the null hypothesis of equal means. It is however clear from the values of the sample standard deviations that the assumption of equal population variances may not be tenable for this data set.
}
\examples{
library(doex)
hybrid$data;
hybrid$species;
}

\value{
  \item{data}{A set of data on yield from corn hybrids obtained from the experiment.}
  \item{species}{A set of corn hybrids.}
}
\author{Mustafa CAVUS}
\references{
Weerahandi, S. (1995) Exact Statistical Methods for Data Analysis. New York: Springer.
}
