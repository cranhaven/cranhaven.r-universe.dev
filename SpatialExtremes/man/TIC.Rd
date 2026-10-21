\name{TIC}
\alias{TIC}
\alias{TIC.default}
\title{Takeuchi's information criterion}
\description{
  Computes a the Takeuchi's information criterion which is equivalent to
  the AIC when the model is miss-specified.
}
\usage{
TIC(object, \dots, k = 2)
}
\arguments{
  \item{object}{An object of class \code{maxstab} or
    \code{spatgev}. Often, it will be the output of the
    \code{\link{fitmaxstab}} or \code{\link{fitspatgev}} function.}
  \item{\dots}{Additional objects of class \code{maxstab} or
    \code{spatgev} for which TIC should be computed.}
  \item{k}{Numeric. The penalty per parameter to be used. The case k = 2
    (default) correspond to the classical TIC and \eqn{k = \log n}{k=
      log n}, n number of observations, is the robust version of the
    BIC.}
}
\value{
  Numeric.
}
\details{
  TIC is like AIC so that when comparing models one wants to get the
  lowest TIC score.
}
\references{
  Gao, X. and Song, P. X.-K. (2009) Composite likelihood Bayesian
  information criteria for model selection in high dimensional
  data. Preprint.

  Sakamoto, Y., Ishiguro, M., and Kitagawa G. (1986) Akaike Information
  Criterion Statistics. D. Reidel Publishing Company.

  Varin, C. and Vidoni, P. (2005) A note on composite likelihood
  inference and model selection. Biometrika \bold{92}(3):519--528.
}
\seealso{
  \code{\link{fitmaxstab}}, \code{\link{AIC}}
}
\examples{
##Define the coordinate of each location
n.site <- 50
locations <- matrix(runif(2*n.site, 0, 100), ncol = 2)
colnames(locations) <- c("lon", "lat")

##Simulate a max-stable process - with unit Frechet margins
data <- rmaxstab(40, locations, cov.mod = "whitmat", nugget = 0.2, range =
30, smooth = 0.5)

M0 <- fitmaxstab(data, locations, "powexp", fit.marge = FALSE)
M1 <- fitmaxstab(data, locations, "cauchy", fit.marge = FALSE)

TIC(M0, M1)
TIC(M0, M1, k = log(nrow(data)))
}
\author{Mathieu Ribatet}
\keyword{htest}
