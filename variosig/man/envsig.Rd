\name{envsig}
\alias{envsig}
\title{Determine Significance of Spatial Dependence Using Pointwise Variogram Envelope}
\description{
Determine the significance of spatial dependence at different scales using pointwise variogram envelope based on permutation test.
}
\usage{
envsig(envlist, index = NULL, method = c("eb", "fisher", "min"))
}

\arguments{
\item{envlist}{output from \code{envelope} function.}
\item{index}{integer. Indicating the index which the permutation test for spatial dependence is performed up to. For example, \code{index = 2} indicates only the first two variogram estimates are considered in the hypothesis test, i.e. null hypothesis is there is no spatial dependence at small scales. If \code{NULL}, all variogram estimates are considered, which corresponds to the null hypothesis of no overall spatial dependence.}
\item{method}{string. One of p-value combination methods.}
}

\value{ A list contains:
\item{p.pointwise}{ Adjusted pointwise p-values.}
\item{p.overall}{ Overall p-value of the permutation test.}
}

\details{
The default and preferred method for computing overall p-value is "eb" (empirical Brown's method), which has good power and close to nominal type I error rate. "fisher" (Fisher's method assumes independent pointwise p-values and requires higher sample size to achieve good power. "min" has the highest power but also much higher type I error rate.
}

\references{Walker, D. D., J. C. Loftis, and J. P. W. Mielke (1997). Permutation methods for determining the significance of spatial dependence. Mathematical Geology 29(8), 1011–1024.

Fisher R. A. (1932). Statistical methods for research workers, 4th ed. Oliver & Boyd.

Poole, W., D. L. Gibbs, I. Shmulevich, B. Bernard, and T. A. Knijnenburg (2016). Combining dependent P-values with an empirical adaptation of Brown’s method. Bioinformatics 32(17), 430–436.

Wang, C., Furrer, R. (2018) Monte Carlo Permutation Tests for Assessing Spatial Dependence at Difference Scales. Nonparametric Statistics. (Submitted)}

\author{Craig Wang}
\seealso{
\code{\link{envelope}} to use Monte Carlo permutations for generating variogram envelope.
}
\examples{
\dontrun{
library(sp)
data(meuse)
coordinates(meuse) = ~x+y
vario0 <- gstat::variogram(log(zinc)~1, meuse)
varioEnv <- envelope(vario0, data = meuse, formula = log(zinc)~1,
  nsim = 500, cluster = TRUE, n.cluster = 10)
envplot(varioEnv)
envsig(varioEnv, index = 2, method = "eb")
}
}
