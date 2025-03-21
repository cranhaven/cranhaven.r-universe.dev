\name{simdata58}
\alias{simdata58}
\docType{data}
\title{Simulated Dataset 58}
\description{The 58th dataset from the simulation study has 100 subjects (sample size is 100). Each subject has three repeated measurements. Detailed model mechanism can be found in the setting II for type III time-dependent covariate on page 90 of Lai and Small (2007). The two random effects in the mechanism are mutually independent and normally distributed with mean 0 and variances 1. The true values of 0 and 0.69 are corresponded to the marginal intercept and slope, accordingly.
}
\usage{data("simdata58")}
\format{
  A data frame with 100 subjects and each subject has three repeated measurements, i.e., number of cluster size or time points. A list that contains one variable:
  \describe{
    \item{\code{y}}{A column matrix of the continuous outcome values.}
    \item{\code{int}}{A column matrix of the intercept values of one.}
    \item{\code{x1}}{A column matrix of the continuous covariate values.}
    \item{\code{id}}{A column matrix of the numbers of identification.}
    \item{\code{visit}}{A column matrix of the order of cluster size or time points.}
  }
}
\references{Lai, T.L., Small, D. (2007). Marginal regression analysis of longitudinal data with time-dependent covariates: a generalized method-of-moments approach. \emph{Journal of the Royal Statistical Society: Series B}, 69, 79--99.
}
\examples{
library(marlod)
data(simdata58)
}
\keyword{datasets}
