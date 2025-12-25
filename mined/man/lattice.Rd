\name{Lattice}
\alias{Lattice}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Good lattice points
}
\description{
Generate good rank-1 lattice points with prime number of points by using the fast component-by-component construction algorithm of Nuyens and Cools (2006). Refer Nuyens (2007) for more details.
}
\usage{
Lattice(n, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
The number of points, which should be a prime.
}
  \item{p}{
The number of dimensions.
}
}
\value{
{
An n-by-p matrix containing the good lattice points.}
}
\references{
%% ~put references to the literature/web site here ~
Nuyens, D. and Cools, R. (2006). "Fast algorithms for component-by-component construction of rank-1 lattice rules in shift-invariant reproducing kernel Hilbert spaces.", \emph{Mathematics of Computation}, 75, 903-920.

Nuyens, D. (2007). "Fast Construction of Good Lattice Rules.", \emph{Ph.D Thesis}, Katholieke Universiteit Leuven, Leuven, Belgium.
}
\author{
Dianpeng Wang <wdp@bit.edu.cn> and V. Roshan Joseph <roshan@gatech.edu>
}


%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
library(mined)
res <- Lattice(101, 2)
plot(res[, 1], res[, 2], col='red',xlab='First dimension', ylab='Second dimension', pch=15)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Quasi-Monte Carlo}% use one of  RShowDoc("KEYWORDS")
\keyword{Experimental Design}% __ONLY ONE__ keyword per line
