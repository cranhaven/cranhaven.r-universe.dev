\name{onions}
\alias{onions}
\non_function{}
\title{Onions data}
\usage{data(onions)}
\description{
The \code{onions} data frame contains 84 sets of observations
from an experiment involving the production of white Spanish onions
in two South Australian locations.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{dens}{areal density of plants (plants per square metre)}
    \item{yield}{onion yield (grammes per plant).}
    \item{location}{indicator of location: 0=Purnong Landing, 1=Virginia.}
  }
}
\source{
Ratkowsky, D. A. (1983).
\emph{Nonlinear Regression Modeling: A Unified Practical Approach.}
New York: Marcel Dekker.
}
\references{
    Ruppert, D., Wand, M.P. and Carroll, R.J. (2003)\cr
     \emph{Semiparametric Regression} Cambridge University Press.\cr
     \url{http://stat.tamu.edu/~carroll/semiregbook/}
}
\examples{
library(SemiPar)
data(onions)
attach(onions)
points.cols <- c("red","blue")
plot(dens,yield,col=points.cols[location+1],pch=16)
legend(100,250,c("Purnong Landing","Virginia"),col=points.cols,pch=rep(16,2))
}
\keyword{datasets}
