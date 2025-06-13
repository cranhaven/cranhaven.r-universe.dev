\name{NReg}
\alias{NReg}
\title{Artificial Example for Normal Regression}
\description{
  A simple artificial regression example containing 200 data points  
  with two latent classes. The data set includes one independent variable 
  (uniform on \eqn{[0,10]}) and one dependent variable with normal 
  distribution.
}

\usage{data("NReg")}

\format{
  This data frame contains the following columns:
  \describe{
    \item{x}{
      a numeric vector giving the independent variable.
    }
    \item{yn}{
      a numeric vector giving the dependent variable with normal distribution.
    }
    \item{class}{
      a numeric vector indicating the labeling of the data points to distinct 
      classes.
    }
  }
}
\examples{
data("NReg", package = "flexmixNL")
plot(yn ~ x, col = class, data = NReg)
}
\keyword{datasets}