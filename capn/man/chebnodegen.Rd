\name{chebnodegen}
\alias{chebnodegen}
\title{Unidimensional Chebyshev nodes}
\usage{
chebnodegen(n, a, b)
}
\arguments{
\item{n}{A number of nodes}

\item{a}{The lower bound of inverval [a,b]}

\item{b}{The upper bound of interval [a,b]}
}
\value{
An array n Chebyshev nodes
}
\description{
The function generates uni-dimensional chebyshev nodes.
}
\details{
A polynomial approximant \eqn{s_{i}} over a bounded interval [a,b] is constructed by:\cr
\cr
\eqn{s_{i} = \frac{b+a}{2} + \frac{b-a}{2}cos (\frac{n - i + 0.5 }{n} \pi )} for \eqn{i = 1,2,\cdots,n}. \cr
\cr
More detail explanation can be refered from Miranda and Fackler (2002, p.119).
}
\examples{
## 10 Chebyshev nodes in [-1,1]
chebnodegen(10,-1,1)
## 5 Chebyshev nodes in [1,5]
chebnodegen(5,1,5)
}
\references{
Miranda, Mario J. and Paul L. Fackler. (2002) \emph{Applied Computational Economics and Finance}. Cambridge: The MIT Press.
}
