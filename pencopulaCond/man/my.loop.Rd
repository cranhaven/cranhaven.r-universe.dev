\name{my.loop}
\alias{my.loop}

\title{Iterative loop for calculating the optimal coefficients 'b'.}

\description{
  Calculating the optimal coefficients 'b' iteratively, using quadratic programing.
}

\usage{
my.loop(penden.env)
}

\arguments{
  \item{penden.env}{Containing all information, environment of pencopula()}
 }

 \details{
   'my.loop' optimates the log-likelihhod iteratively. Therefore, the
   routine checks the relative chance in the weights and stops the
   iteration, if the relative change of all weights 'b' is less than one
   percent. During the calculations of new weights 'b' in the routine
   'new.weights', most of the values are called '.temp'. This add on
   unterlines the temporarily values. 'my.loop' checks the relative
   change in the weights. If the change is greater than one percent, the
   the real values are overwritten with the '.temp' values.
}

\value{
  \item{liste}{The results of each iteration are written in a matrix called 'liste',
   saved in the environment. 'liste' contains the penalized
   log-likelihood, the log-likelihood, 'lambda' and the weights 'b'.}
}

\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}
\keyword{math}
\references{Estimating Non-Simplified Vine Copulas Using Penalized Splines, Schellhase, C. and Spanhel, F. (2017), Statistics and Computing.}
