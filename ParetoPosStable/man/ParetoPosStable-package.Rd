\name{ParetoPosStable-package}
\alias{ParetoPosStable-package}
\alias{ParetoPosStable}
\docType{package}
\title{
Computing, fitting and validating the PPS distribution
}
\description{
Statistical functions to describe a Pareto Positive Stable (PPS) 
distribution and fit it to real data. Graphical and statistical tools to 
validate the fits are included.
}
\details{
\tabular{ll}{
Package: \tab ParetoPosStable\cr
Type: \tab Package\cr
Version: \tab 1.0.3\cr
Date: \tab 2014-10-22\cr
License: \tab GPL-2 | GPL-3\cr
}
}
\author{
Antonio Jose Saez-Castillo, Faustino Prieto and Jose Maria Sarabia

Maintainer: Antonio Jose Saez-Castillo <ajsaez@ujaen.es>
}
\references{
Sarabia, J.M and Prieto, F. (2009). The Pareto-positive stable distribution: A new descriptive model for city size data, \emph{Physica A: Statistical Mechanics and its Applications}, \bold{388}(19), 4179-4191.
}
\examples{
x <- rPPS(50, 1.2, 100, 2.3)
fit <- PPS.fit(x)
print(fit)
coef(fit)
se(fit, k = 50, show.iters = FALSE)
logLik(fit)
par(mfrow=c(2,2))
plot(fit)
GoF(fit, k = 50, show.iters = FALSE)
}

