\name{lq}
\alias{lq}


\title{
Simple Location Quotient Updating
}
\description{
Uses simple linear quotient technique to update the matrix of technical input coefficients (\code{A})
}
\usage{
lq(io)
}

\arguments{
  \item{io}{An \code{InputOutput} class object from \code{\link{as.inputoutput}}}
}
\details{
Uses the simple linear quotient technique as follows:
\deqn{lq_i = \frac{X_i^r / X^r}{X_i^n / X^n}}
where \eqn{X^n} is the total production, \eqn{X^r} is the total production for region r, \eqn{X^r_i} is the production for region r sector i, and \eqn{X^n_i} is the total production for the ith sector.

Then lq is converted such that if \eqn{lq_i > 1}, then \eqn{lq_i = 1}. Then lq is converted into a diagonal matrix of values less than or equal to 1, which gives us our final results
\deqn{\hat{A} = A lq}
}
\value{
Produces the forecast of the matrix of technical input coefficients (\code{A}) using the Slq technique.
}
\references{Blair, P.D. and Miller, R.E. (2009). "Input-Output Analysis: Foundations and Extensions". Cambridge University Press

Nazara, Suahasil & Guo, Dong & Hewings, Geoffrey J.D., & Dridi, Chokri, 2003. "PyIO. Input-Output Analysis with Python". REAL Discussion Paper 03-t-23. University of Illinois at Urbana-Champaign. (\url{http://www.real.illinois.edu/d-paper/03/03-t-23.pdf})}

\author{
John J. P. Wade
}



\examples{
data(toy.IO)
class(toy.IO)

Anew <- lq(toy.IO)
}

