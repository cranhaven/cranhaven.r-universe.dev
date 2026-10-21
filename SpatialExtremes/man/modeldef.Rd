\name{modeldef}
\alias{modeldef}
\alias{modeldef.lm}
\alias{modeldef.rb}
\title{Define a model for the spatial behaviour of the GEV parameters}
\description{This function defines the model for the spatial behaviour
  of the GEV parameter.
}
\usage{modeldef(data, formula)
}
\arguments{
  \item{data}{A matrix representing the data. Each column corresponds to
    one location.}
  \item{formula}{A R formula. See details for further details.}
}
\value{
  \bold{need to be documented}
}
\seealso{
  \code{\link{formula}}
}
\examples{
## 1- A design matrix from a classical linear model 
n.site <- 5
coord <- matrix(rnorm(2*n.site, sd = sqrt(.2)), ncol = 2)
colnames(coord) <- c("lon", "lat")
loc.form <- loc ~ lat + I(lon^2)
modeldef(coord, loc.form)

## 2- A design and penalization matrix from a penalized smoothin spline
x <- sort(runif(10, -2, 10))
n.knots <- 3
knots <- quantile(x, prob = 1:n.knots / (n.knots + 2))
modeldef(x, y ~ rb(x, knots = knots, degree = 3, penalty = 1))
}
\author{Mathieu Ribatet}
\keyword{design}