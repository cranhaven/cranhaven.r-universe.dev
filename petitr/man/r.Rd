\name{r}
\Rdversion{1.1}
\alias{r}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ malthusian parameter
}
\description{
	Calculates the intrinsic rate of increase by solving the Birch equation. Uses the Newton method. 
}
\usage{
r(tab, eps = eps, maxiter = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab}{
	a data.frame with three columns : x, the age, lx, the proportion of survivors at age x, mx, the offspring number per individual in the age class x
}
  \item{eps}{
 Precision for the convergence of Newton method. Default is object eps transmitted by the calling function r. must be defined for a standalone use
}
  \item{maxiter}{
maximum number of iterations for the Newton's method. default = 100
}
}
\value{
a single numeric value : r
}
\references{
 Lotka 1924, Birch 1948.
}
\author{
Jean-Sebastien PIERRE
}

\seealso{
 petitr,xlxmx,ranova
}
\examples{
data (tblif)
r(tblif,eps=0.0000001)
}
\keyword{ misc}
