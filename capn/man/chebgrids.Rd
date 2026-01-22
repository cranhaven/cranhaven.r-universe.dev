\name{chebgrids}
\alias{chebgrids}
\title{Generating Chebyshev grids}
\usage{
chebgrids(nnodes, lb, ub, rtype = NULL)
}
\arguments{
\item{nnodes}{An array of numbers of nodes}

\item{lb}{An array of lower bounds}

\item{ub}{An array of upper bounds}

\item{rtype}{A type of results; default is NULL that returns a list class; if rtype = list, returns a list class; if rtype = grid, returns a matrix class.}
}
\value{
A list with \eqn{d} elements of Chebyshev nodes or a \eqn{ \left( \displaystyle \prod_{i=1}^{d} n_{i} \right) \times d} matrix of Chebyshev grids
}
\description{
This function generates a grid of multi-dimensional Chebyshev nodes.
}
\details{
For the \eqn{i}-th dimension of \eqn{i = 1, 2, \cdots, d}, suppose a polynomial approximant
\eqn{s_{i}} over a bounded interval \eqn{[a_{i},b_{i}]} is defined by Chebysev nodes. Then, a \eqn{d}-dimensional
Chebyshev grids can be defined as:\cr
\cr
\eqn{\mathbf{S} = \left\{ (s_{1},s_{2},\cdots,s_{d}) \vert a_{i} \leq s_{1} \leq b_{i}, i = 1, 2, \cdots, d \right\} }.\cr
\cr
This is all combinations of \eqn{s_{i}}. Two types of results are provided. '\code{rtype = list}' provides a list
of \eqn{d} dimensions wherease '\code{rtype = grids}' creates a \eqn{ \left( \displaystyle \prod_{i=1}^{d} n_{i} \right) \times d} matrix.
}
\examples{
## Chebyshev grids with two-dimension
chebgrids(c(5,3), c(1,1), c(2,3))
# Returns the same results
chebgrids(c(5,3), c(1,1), c(2,3), rtype='list')
## Returns a matrix grids with the same domain
chebgrids(c(5,3), c(1,1), c(2,3), rtype='grid')
## Chebyshev grids with one-dimension
chebgrids(5,1,2)
chebnodegen(5,1,2)
## Chebyshev grids with three stock
chebgrids(c(3,4,5),c(1,1,1),c(2,3,4),rtype='grid')
}
\seealso{
\code{\link{chebnodegen}}
}
