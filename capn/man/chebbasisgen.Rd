\name{chebbasisgen}
\alias{chebbasisgen}
\title{Generating Unidimensional Chebyshev polynomial (monomial) basis}
\usage{
chebbasisgen(stock, npol, a, b, dorder = NULL)
}
\arguments{
\item{stock}{An array of Chebyshev polynomial nodes \eqn{s_{i}} \cr
(an array of stocks in \code{capn}-packages)}

\item{npol}{Number of polynomials (n polynomials = (n-1)-th degree)}

\item{a}{The lower bound of inverval [a,b]}

\item{b}{The upper bound of inverval [a,b]}

\item{dorder}{Degree of partial derivative of the basis; Default is NULL; if dorder = 1, returns the first order partial derivative}
}
\value{
A matrix (number of nodes (\eqn{m}) x npol (\eqn{n})) of (monomial) Chebyshev polynomial basis
}
\description{
The function calculates the monomial basis of Chebyshev polynomials for the given unidimensional nodes,
\eqn{s_{i}}, over a bounded interval [a,b].
}
\details{
Suppose there are \eqn{m} numbers of Chebyshev nodes over a bounded interval [a,b]:\cr
\cr
\eqn{s_{i} \in [a,b],} for \eqn{i = 1,2,\cdots,m}.\cr
\cr
These nodes can be nomralized to the standard Chebyshev nodes over the domain [-1,1]:\cr
\cr
\eqn{z_{i} = \frac{2(s_{i} - a)}{(b - a)} - 1}.\cr
\cr
With normalized Chebyshev nodes, the recurrence relations of Chebyshev polynomials of order \eqn{n} is defined as:\cr
\cr
\eqn{T_{0} (z_{i}) = 1}, \cr
\eqn{T_{1} (z_{i}) = z_{i}}, and \cr
\eqn{T_{n} (z_{i}) = 2 z_{i} T_{n-1} (z_{i}) - T_{n-2} (z_{i})}.\cr
\cr
The interpolation matrix (Vandermonde matrix) of (n-1)-th Chebyshev polynomials with \eqn{m} nodes,
\eqn{\Phi_{mn}} is:\cr
\cr
\eqn{ \Phi_{mn} = \left[ \begin{array}{ccccc}
1 & T_{1} (z_{1}) & \cdots & T_{n-1} (z_{1})\\
1 & T_{1} (z_{2}) & \cdots & T_{n-1} (z_{2})\\
\vdots & \vdots & \ddots & \vdots\\
1 & T_{1} (z_{m}) & \cdots & T_{n-1} (z_{m})
\end{array} \right] }.\cr
\cr
The partial derivative of the monomial basis matrix can be found by the relation:\cr
\cr
\eqn{(1-z_{i}^{2}) T'_{n} (z_{i}) = n[ T_{n-1} (z_{i}) - z_{i} T_{n} (z_{i}) ]}.\cr
\cr
The technical details of the monomial basis of Chebyshev polynomial can be referred from Amparo et al. (2007)
and Miranda and Fackler (2012).
}
\examples{
## Reef-fish example: see Fenichel and Abbott (2014)
data("GOM")
nodes <- chebnodegen(param$nodes,param$lowerK,param$upperK)
## An example of Chebyshev polynomial basis
chebbasisgen(nodes,20,0.1,1.5)
## The partial derivative of Chebyshev polynomial basis with the same function
chebbasisgen(nodes,20,0.1,1.5,1)
}
\references{
Amparo, Gil, Javier Segura, and Nico Temme. (2007) \emph{Numerical Methods for Special Functions}. Cambridge: Cambridge University Press.\cr
Miranda, Mario J. and Paul L. Fackler. (2002) \emph{Applied Computational Economics and Finance}. Cambridge: The MIT Press.
}
\seealso{
\code{\link{chebnodegen}}
}
