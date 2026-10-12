\name{mpm}
\alias{mpm}

\title{Multiplier Product Matrix}
\description{\code{mpm} calculates the multiplier product matrix using an \code{InputOutput} object calculated from \code{\link{as.inputoutput}}. The method is described below.}
\usage{
mpm(io)
}

\arguments{
 \item{io}{An \code{InputOutput} class object from \code{\link{as.inputoutput}}}
}
\details{
Let \eqn{L} be the Leontief inverse. Then the multiplier product matrix \eqn{M} is calculated as follows:
\deqn{M = 1/v L_c L_r}
where \eqn{v = t(1) L 1} such that \eqn{1} is a column matrix of ones, \eqn{L_c = L 1} is a column matrix of row sums, and \eqn{L_r = t(1) L} is a row matrix of column sums.
}
\value{
\item{M}{Multiplier Product Matrix}
}
\references{
Nazara, Suahasil & Guo, Dong & Hewings, Geoffrey J.D., & Dridi, Chokri, 2003. "PyIO. Input-Output Analysis with Python". REAL Discussion Paper 03-t-23. University of Illinois at Urbana-Champaign. (\url{http://www.real.illinois.edu/d-paper/03/03-t-23.pdf})
}
\author{
John J. P. Wade
}

\examples{
data(toy.IO)
class(toy.IO)
M <- mpm(toy.IO)
}

