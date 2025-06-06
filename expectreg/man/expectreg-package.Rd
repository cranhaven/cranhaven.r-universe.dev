\name{expectreg-package}
\Rdversion{1.1}
\alias{expectreg-package}
\alias{expectreg}
\docType{package}
\title{
Expectile and Quantile Regression
}
\description{
Expectile and quantile regression of models with nonlinear effects
  e.g. spatial, random, ridge using least asymmetric weighed squares / absolutes
  as well as boosting; also supplies expectiles for common distributions.
}
\details{

\itemize{
\item This package requires the packages \code{\link[=BayesX-package]{BayesX}}, \code{\link[=mboost-package]{mboost}}, \code{\link[=splines-package]{splines}} and \code{quadprog}.
}

}
\author{
Fabian Otto-Sobotka \cr
Carl von Ossietzky University Oldenburg \cr
\url{https://uol.de} \cr

Elmar Spiegel \cr
Helmholtz Centre Munich \cr
\url{https://www.helmholtz-munich.de} \cr

Sabine Schnabel \cr
Wageningen University and Research Centre \cr
\url{https://www.wur.nl}

Linda Schulze Waltrup \cr
Ludwig Maximilian University Munich \cr
\url{https://www.lmu.de} \cr

with contributions from

Paul Eilers \cr
Erasmus Medical Center Rotterdam \cr
\url{https://www.erasmusmc.nl}

Thomas Kneib \cr
Georg August University Goettingen \cr
\url{https://www.uni-goettingen.de} \cr

Goeran Kauermann \cr
Ludwig Maximilian University Munich \cr
\url{https://www.lmu.de} \cr

Maintainer: Fabian Otto-Sobotka <fabian.otto-sobotka@uni-oldenburg.de>

}
\references{
Fenske N and Kneib T and Hothorn T (2009)
\emph{ Identifying Risk Factors for Severe Childhood Malnutrition
by Boosting Additive Quantile Regression}
Technical Report 052, University of Munich

He X (1997)
\emph{ Quantile Curves without Crossing }
The American Statistician, 51(2):186-192

Koenker R (2005)
\emph{ Quantile Regression }
Cambridge University Press, New York

Schnabel S and Eilers P (2009)
\emph{ Optimal expectile smoothing }
Computational Statistics and Data Analysis, 53:4168-4177

Schnabel S and Eilers P (2011)
\emph{ Expectile sheets for joint estimation of expectile curves }
(under review at Statistical Modelling)

Sobotka F and Kneib T (2010)
\emph{ Geoadditive Expectile Regression }
Computational Statistics and Data Analysis,
doi: 10.1016/j.csda.2010.11.015.
}

\keyword{ package }
\keyword{ nonparametric }
\keyword{ smooth }% __ONLY ONE__ keyword per line
\keyword{ multivariate }
\keyword{ regression }
\keyword{ nonlinear }
\keyword{ models }
\seealso{
\code{\link[=mboost-package]{mboost}}, \code{\link[=BayesX-package]{BayesX}}
}
\examples{
data(dutchboys)

## Expectile Regression using the restricted approach
ex = expectreg.ls(dist ~ rb(speed),data=cars,smooth="f",lambda=5,estimate="restricted")
names(ex)

## The calculation of expectiles for given distributions
enorm(0.1)
enorm(0.5)

## Introducing the expectiles-meet-quantiles distribution
x = seq(-5,5,length=100)
plot(x,demq(x),type="l")

## giving an expectile analogon to the 'quantile' function
y = rnorm(1000)

expectile(y)

eenorm(y)
}
