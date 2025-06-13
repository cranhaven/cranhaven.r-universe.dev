\docType{package}
\name{A3-package}
\alias{A3-package}
\title{A3 Error Metrics for Predictive Models}
\description{
  A package for the generation of accurate, accessible, and
  adaptable error metrics for developing high quality
  predictions and inferences. The name A3 (pronounced "A-Cubed")
  comes from the combination of the first letters of these three
  primary adjectives.
}
\details{
The overarching purpose of the outputs and tools in this package
  are to make the accurate assessment of model errors more
  accessible to a wider audience. Furthermore, a standardized set
  of reporting features are provided by this package which create
  consistent outputs for virtually any predictive model. This makes
  it straightforward to compare, for instance, a linear regression model
  to more exotic techniques such as Random forests or Support
  vector machines.
  
  The standard outputs for each model fit provided by the A3 package
  include:
  \itemize{
  \item Average Slope: Equivalent to a linear regression coefficient.
  \item Cross Validated \eqn{R^2}: Robust calculation of \eqn{R^2} (percent of squared error explained by the model compared to the null model) values adjusting for over-fitting.
  \item p Values: Robust calculation of p-values requiring no parametric assumptions other than independence between observations (which may be violated if compensated for).
  }
  
  The primary functions that will be used are
  \code{\link{a3}} for arbitrary modeling functions and
  \code{\link{a3.lm}} for linear models. This package also
  includes \code{\link{print.A3}} and \code{\link{plot.A3}}
  for outputting the A3 results.
}

\author{
  Scott Fortmann-Roe \email{scottfr@berkeley.edu} \url{http://Scott.Fortmann-Roe.com}
}

