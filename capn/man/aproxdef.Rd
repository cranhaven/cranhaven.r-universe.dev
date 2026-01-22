\name{aproxdef}
\alias{aproxdef}
\title{Defining Approximation Space}
\usage{
aproxdef(deg, lb, ub, delta)
}
\arguments{
\item{deg}{An array of degrees of approximation function: degrees of Chebyshev polynomials}

\item{lb}{An array of lower bounds}

\item{ub}{An array of upper bounds}

\item{delta}{discount rate}
}
\value{
A list containing the approximation space
}
\description{
The function defines an approximation space for all three approximation apporoaches (V, P, and Pdot).
}
\details{
For the \eqn{i}-th dimension of \eqn{i = 1, 2, \cdots, d}, suppose a polynomial approximant
\eqn{s_{i}} over a bounded interval \eqn{[a_{i},b_{i}]} is defined by Chebysev nodes. Then, a \eqn{d}-dimensional
Chebyshev grids can be defined as:\cr
\cr
\eqn{\mathbf{S} = \left\{ (s_{1},s_{2},\cdots,s_{d}) \vert a_{i} \leq s_{1} \leq b_{i}, i = 1, 2, \cdots, d \right\} }.\cr
\cr
Suppose we impletement \eqn{n_{i}} numbers of polynomials (i.e., \eqn{(n_{i}-1)}-th order) for the \eqn{i}-th dimension.
The approximation space is defined as:\cr
\cr
deg = c(\eqn{n_{1},n_{2},\cdots,n_{d}}),\cr
lb = c(\eqn{a_{1},a_{2},\cdots,a_{d}}), and\cr
ub = c(\eqn{b_{1},b_{2},\cdots,b_{d}}).\cr
\cr
\code{delta} is the given constant discount rate.
}
\examples{
## Reef-fish example: see Fenichel and Abbott (2014)
delta <- 0.02
upper <- 359016000     # upper bound on approximation space
lower <- 5*10^6        # lower bound on approximation space
myspace <- aproxdef(50,lower,upper,delta)
## Two dimensional example
ub <- c(1.5,1.5)
lb <- c(0.1,0.1)
deg <- c(20,20)
delta <- 0.03
myspace <- aproxdef(deg,lb,ub,delta)
}
\references{
Fenichel, Eli P. and Joshua K. Abbott. (2014) "\href{http://www.journals.uchicago.edu/doi/abs/10.1086/676034}{Natural Capital: From Metaphor to Measurement}."
             \emph{Journal of the Association of Environmental Economists}. 1(1/2):1-27.
}
\seealso{
\code{\link{vaprox}, \link{vsim}, \link{paprox}, \link{psim}, \link{pdotaprox}, \link{pdotsim}}
}
