\name{Bt.test}
\alias{Bt.test}
\title{
Bayesian t-test using reference priors.
}
\description{
The Bayesian ``t-test'' developed by Bernardo and Perez (2007) that
calculates the Bayes-factor against the null hypothesis of no
difference. 
}
\usage{
Bt.test(formula, data, plotit = TRUE)
}
\arguments{
  \item{formula}{
the usual formula interface: response ~ factor
}
  \item{data}{
a data.frame with the response values and the factor values for all
samples; the factor can only have two factor levels
}
  \item{plotit}{
is plotted output required?
}
}
\value{
none supplied: the function is used for the plotted and printed output
it produces
}
\references{
J. Bernardo and S. Perez. Comparing normal means: New methods for an
old problem. Bayesian Analysis, 2:45--58, 2007.

van Hulst, R. 2018. Evaluating Scientific Evidence. ms. 
  
}
\author{
Robert van Hulst
}
\seealso{
  \code{\link{B2Nsir}}
  
  \code{\link{t.test}}
}
\examples{
data(bodytemp)
Bt.test(temperature ~ gender, bodytemp)

Bt.test(heart.rate ~ gender, bodytemp)
}

\keyword{ htest }
