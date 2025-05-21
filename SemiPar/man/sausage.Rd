\name{sausage}
\alias{sausage}
\non_function{}
\title{Sausage data}
\usage{data(sausage)}
\description{
The \code{sausage} data frame has data on 54 `hot dog'
sausages.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{type}{type of meat.}
    \item{calories}{number of calories.}
    \item{sodium}{measure of sodium content.}
  }
}
\source{
Moore, D.S. and McCabe, G.P. (2003).
\emph{Introduction to the Practice of Statistics, Fourth
Edition}, W.H. Freeman and Company.
}
\references{
    Ruppert, D., Wand, M.P. and Carroll, R.J. (2003)\cr
     \emph{Semiparametric Regression} Cambridge University Press.\cr
     \url{http://stat.tamu.edu/~carroll/semiregbook/}
}
\examples{
library(SemiPar)
data(sausage)
attach(sausage)
points.cols <- c("red","blue","green")
plot(sodium,calories,col=points.cols[type],pch=16)
legend(200,180,c("beef","pork","poultry"),col=points.cols,pch=rep(16,3))
}
\keyword{datasets}
