\encoding{utf8}
\name{SLM1}
\alias{SLM1}
\docType{data}
\title{
First simulated data for the simple linear regression model
}
\description{
First data used in example 4 of Salmerón, García and García (2024) (subsection 4.4) on the special case of the simple linear model.
}
\usage{data("SLM1")}
\format{
  A data frame with 50 observations on the following 3 variables:
  \describe{
    \item{\code{y1}}{Dependent variable simulated as y = 3 + 4*V + u where u is normally distributed with a mean of 0 and a variance of 2.}
    \item{\code{cte}}{Intercept.}
    \item{\code{V}}{Simulated from a normal distribution with a mean of 10 and a variance of 100.}
  }
}
\references{
Salmerón, R., García, C.B. and García, J. (2025). A redefined Variance Inflation Factor: overcoming the 
limitations of the Variance Inflation Factor. Computational Economics, 65, 337-363, doi: https://doi.org/10.1007/s10614-024-10575-8.
}
\examples{
  head(SLM1, n=5)
  y = SLM1[,1]
  x = SLM1[,2:3]
  multicollinearity(y, x)
}
\keyword{datasets}
