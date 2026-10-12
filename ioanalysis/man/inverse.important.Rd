\name{inverse.important}
\alias{inverse.important}

\title{
Inverse.Important Coefficients
}
\description{
Calculates the inverse-important coefficients as in Blair and Miller (2009)
}
\usage{
inverse.important(io, i, j, delta.aij)
}

\arguments{
  \item{io}{An \code{InputOutput} class object from \code{\link{as.inputoutput}}}
  \item{i}{Integer. The row component of the change in the matrix of technical input coefficients}
  \item{j}{Integer. The column component of the change in the matrix of technical input coefficients}
  \item{delta.aij}{Integer. By how much aij should change by}
}

\details{
The inverse-important coefficients is the change in the Leontief matrix due to a specified change in one element of the matrix of technical input coefficients (A). This uses the formula:
\deqn{ \Delta L = \frac{\Delta a_{ij}}{1-l_{ji}\Delta a_{ij}} F_1(i,j) }
where F_1(X,Y) is the first order field of influence.
}
\value{
Returns the change in the Leontief matrix due the change in one element of the matrix of technical input coefficients. To find the new Leontief inverse induced by this change, use io$L + inverse.important().
}
\references{
Blair, P.D. and Miller, R.E. (2009). "Input-Output Analysis: Foundations and Extensions". Cambridge University Press
}
\author{
John J. P. Wade, Ignacio Sarmiento-Barbieri
}



\examples{
data(toy.IO)
class(toy.IO)
i <- 3
j <- 4
delta.aij <- 0.5
II <- inverse.important(toy.IO, i, j, delta.aij)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

