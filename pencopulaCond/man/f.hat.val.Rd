\name{f.hat.val}
\alias{f.hat.val}
\title{Calculating the actual fitted values 'f.hat.val' of the estimated density function}

\description{
Calculating the actual fitted values of the response, depending on the actual parameter set b
}
\usage{
f.hat.val(penden.env,cal=FALSE,temp=FALSE)
}
\arguments{
  	\item{penden.env}{Containing all information, environment of pencopula()}
	\item{cal}{if TRUE, the final weights of one iteration are used for
    the calculation of the fitted values.}
	\item{temp}{if TRUE, the iteration for optimal weights is still in
    progress and the temporary weights are used for calculation of the
    fitted values.}
}
\value{
  \item{f.hat.val}{Fitted values for the current coefficents}.
  f.hat.val is saved in the environment.
}
\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}
\keyword{nonparametric}
\references{Flexible Copula Density Estimation with Penalized
  Hierarchical B-Splines, Kauermann G., Schellhase C. and Ruppert, D. (2013), Scandinavian Journal of Statistics 40(4), 685-705.

Estimating Non-Simplified Vine Copulas Using Penalized Splines, Schellhase, C. and Spanhel, F. (2017), Statistics and Computing.}
