\name{GReg}
\alias{GReg}
\title{Artificial Example for Gamma Regression}
\description{
  A simple artificial regression example containing 200 data points  
  with two latent classes. The data set includes one independent variable 
  (uniform on \eqn{[0,10]}) and one dependent variable with gamma distribution.
}

\usage{data("GReg")}

\format{
  This data frame contains the following columns:
  \describe{
    \item{x}{
      a numeric vector giving the independent variable.
    }
    \item{yg}{
      a numeric vector giving the dependent variable with gamma distribution.
    }
    \item{class}{
      a numeric vector indicating the labeling of the data points to two 
      distinct classes.
    }
  }
}
\examples{
data("GReg", package = "flexmixNL")
plot(yg ~ x, col = class, data = GReg)
}
\keyword{datasets}