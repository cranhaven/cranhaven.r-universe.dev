\name{lower.upper}
\alias{lower.upper}

\title{
Calculates lower band and upper band
}

\description{
Calculates lower and upper bands for each data point, using a set of cut-points which is obtained from the Gaussian copula.
}

\usage{
lower.upper(y)
}

\arguments{
	\item{y}{ An (\eqn{n \times p}) matrix or a \code{data.frame} corresponding to the data matrix (\eqn{n} is the sample size and \eqn{p} is the number of variables). It also could be an object of class \code{"episim"}.
}
}

\value{
\item{lower}{
A \eqn{n} by \eqn{p} matrix representing the lower band for each data point.
}
\item{upper}{
A \eqn{n} by \eqn{p} matrix representing the upper band for each data point.
}
}

\author{ 
  Pariya Behrouzi and Ernst C. Wit \cr
  Maintainer: Pariya Behrouzi <pariya.behrouzi@gmail.com>
}

\references{
Behrouzi, P., and Wit, E. C. (2019). Detecting epistatic selection with partially observed genotype data by using copula graphical models. Journal of the Royal Statistical Society: Series C (Applied Statistics), 68(1), 141-160. \cr
}

\seealso{
  \code{\link{cutoffs}} and \code{\link{netgwas-package}}.
}

\examples{
D <- simgeno(p = 100, n = 50, k = 3)
lower.upper(D$data)
}