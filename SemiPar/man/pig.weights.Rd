\name{pig.weights}
\alias{pig.weights}
\non_function{}
\title{Pig weight data}
\usage{data(pig.weights)}
\description{
The \code{pig.weights} data frame has 9 repeated 
weight measures on 48 pigs. 
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{id.num}{identification number of pig.}
    \item{num.weeks}{number of weeks since measurements commenced.}
    \item{weight}{bodyweight of pig "id.num" after "num.weeks" weeks.}
  }
}
\source{
Diggle, P.J., Heagerty, P., Liang, K.-Y. and Zeger, S.L. (2002).
\emph{Analysis of Longitudinal Data, Second Edition}, Oxord: Oxford
University Press.
}
\references{
    Ruppert, D., Wand, M.P. and Carroll, R.J. (2003)\cr
     \emph{Semiparametric Regression} Cambridge University Press.\cr
     \url{http://stat.tamu.edu/~carroll/semiregbook/}
}
\examples{
library(SemiPar)
data(pig.weights)
library(lattice)
xyplot(weight~num.weeks,data=pig.weights,groups=id.num,type="b")
}
\keyword{datasets}
