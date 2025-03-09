\name{aovFbyrow}
\alias{aovFbyrow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{calculate aov F-statistic for each row of a matrix}
\description{
Returns on aov F-statistic for each row of \code{x}
}
\usage{
aovFbyrow(x=matrix(rnorm(1000), ncol=20), cl = factor(rep(1:3, c(7,9,4))))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{features by observations matrix}
  \item{cl}{factor that classifies the values in each row}
}
\details{
This uses the functions \code{qr()} and \code{qr.qty()} for the main
part of the calculation, for handling the calculations efficently
}
\value{
one F-statistic for each row of \code{x}
}
\author{John Maindonald}
\seealso{See also \code{\link{orderFeatures}}}
\examples{
mat <- matrix(rnorm(1000), ncol=20)
cl <- factor(rep(1:3, c(7,9,4)))
Fstats <- aovFbyrow(x = mat, cl = cl)

## The function is currently defined as
aovFbyrow <-
function(x=matrix(rnorm(1000), ncol=20),
         cl=factor(rep(1:3, c(7,9,4)))){
    y <- t(x)
    qr.obj <- qr(model.matrix(~cl))
    qty.obj <- qr.qty(qr.obj,y)
    tab <- table(factor(cl))
    dfb <- length(tab)-1
    dfw <- sum(tab)-dfb-1
    ms.between <- apply(qty.obj[2:(dfb+1), , drop=FALSE]^2, 2, sum)/dfb
    ms.within <- apply(qty.obj[-(1:(dfb+1)), , drop=FALSE]^2, 2, sum)/dfw
    Fstat <- ms.between/ms.within
  }
}
\keyword{htest}% at least one, from doc/KEYWORDS
